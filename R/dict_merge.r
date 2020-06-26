dict_merge <- function(dict,
                       reference_dict,
                       dict_diff = NULL) {
  survey_name <- attr(dict, "survey_name")
  survey_name_ref <- attr(reference_dict, "survey_name")

  newname <- colnames(dict)[1]
  newname_ref <- colnames(reference_dict)[1]

  if (survey_name == survey_name_ref) {
    stop("Dictionaries to be merged are from the same survey")
  }

  if (any(duplicated(dict_diff$name))) {
    stop("Non-unique mapping in dict_diff.")
  }

  if (is.null(dict_diff)) {
    message("Consider using 'dict_compare' to track potential matching items")
  } else if (nrow(dict_diff) > 0) {

    # Find items with differing number of levels
    levels_diff <- dict_diff %>%
      filter(n_levels != n_levels_ref) %>%
      select(name, name_reference, n_levels, n_levels_ref)

    if (nrow(levels_diff) > 0) {
      for (i in seq(nrow(levels_diff))) {
        warning(
          levels_diff[i, "name"], " and ", levels_diff[i, "name_reference"],
          " cannot be merged due to differing number of levels."
        )
      }
    }

    # Remove items with differing levels
    dict_diff <- dict_diff %>% filter(!name %in% levels_diff[["name"]])

    # Recode item, label and newname in dict according to dict_ref
    dict$question <- recode(
      dict$question,
      !!!setNames(
        dict_diff[["question_reference"]],
        dict_diff[["question"]]
      )
    )

    dict[[newname]] <- recode(
      dict[[newname]],
      !!!setNames(
        dict_diff[["name_reference"]],
        dict_diff[["name"]]
      )
    )

    dict["label"] <- map(
      unique(dict[[newname]]),
      function(x) {
        label <- filter(dict, .data[[newname]] == x) %>% select(label)
        if (x %in% dict_diff[["name"]]) {
          sub_dict_diff <- dict_diff %>% filter(name == x)
          n_levels <- sub_dict_diff %>% select(n_levels)
          n_levels_ref <- sub_dict_diff %>% select(n_levels_ref)
          if (n_levels == n_levels_ref) {
            name_ref <- sub_dict_diff[["name_reference"]]
            label <- filter(reference_dict, .data[[newname_ref]] == name_ref) %>% select(label)
          }
        }

        return(label)
      }
    ) %>%
      bind_rows()
  }


  # If there is no survey indicator variabe, add it
  if (is.null(reference_dict[survey_name_ref])) {
    reference_dict[survey_name_ref] <- TRUE
  }

  merged <- full_join(reference_dict,
    bind_cols(dict, setNames(tibble(TRUE), survey_name)),
    by = c(newname_ref = newname, "question", "label"),
    suffix = c(
      paste0("_", survey_name_ref),
      paste0("_", survey_name)
    )
  ) %>%
    select(.data[[newname_ref]], everything())

  duplicated_ref_names_is <- dict_diff$name_reference %>%
    subset(duplicated(.)) %>%
    map(~ which(reference_dict[[newname_ref]] == .x)) %>%
    unlist()

  to_fill <- setdiff(
    make.unique(dict_diff[["name_reference"]]),
    unique(dict_diff[["name_reference"]])
  ) %>%
    map(~ which(merged[[newname_ref]] == .x)) %>%
    unlist()

  if (length(to_fill) != length(duplicated_ref_names_is)) {
    stop("Check if questions with matching names have the same number of levels.")
  }

  merged[to_fill, -1] <- merged[duplicated_ref_names_is, -1]

  return(merged)
}
