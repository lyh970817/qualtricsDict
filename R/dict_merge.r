dict_merge <- function(dict,
                       reference_dict,
                       dict_diff = NULL,
                       force_level = FALSE) {
  survey_name <- attr(dict, "survey_name")
  survey_name_ref <- attr(reference_dict, "survey_name")

  newname <- get_newname(dict)
  newname_ref <- get_newname(reference_dict)

  if (survey_name == survey_name_ref) {
    stop("Dictionaries to be merged are from the same survey")
  }

  if (any(duplicated(dict_diff$name))) {
    stop("Non-unique mapping in dict_diff.")
  }


  if (is.null(dict_diff)) {
    # Implement this
    message("Consider using 'dict_compare' to track potential matching items")
  } else if (nrow(dict_diff) > 0) {
    browser()
    if (!(all(dict_diff[["name"]] %in% dict[[newname]]) &&
      all(dict_diff[["name_reference"]] %in% reference_dict[[newname]]))) {
      stop("dict_diff file does not correspond to dicts to be merged.")
    }
    # Recode item, label and newname in dict according to dict_ref
    if (force_level) {
      dict_labels <- map(
        dict_diff[["name"]],
        function(x) {
          filter(dict, .data[[newname]] == x) %>%
            select(label) %>%
            pull()
        }
      ) %>%
        setNames(dict_diff[["name"]])

      dict_ref_labels <- map(
        dict_diff[["name_reference"]],
        function(x) {
          filter(reference_dict, .data[[newname_ref]] == x) %>%
            select(label) %>%
            pull()
        }
      ) %>%
        setNames(dict_diff[["name_reference"]])

      dict[match_all(dict[[newname]], dict_diff[["name"]]), "label"] <-
        map2(dict_labels, dict_ref_labels, function(x, y) {
          # If different length, overwrite the longer length with shorter
          # length
          len <- ifelse(length(x) < length(y), length(x), length(y))
          x[seq(len)] <- y[seq(len)]
          return(x)
        }) %>%
        unlist()
    }
    else {
      # Find items with differing number of levels
      levels_diff <- dict_diff %>%
        filter(n_levels != n_levels_ref) %>%
        select(name, name_reference, n_levels, n_levels_ref)

      if (nrow(levels_diff) > 0) {
        for (i in seq(nrow(levels_diff))) {
          warning(
            levels_diff[i, "name"], " and ", levels_diff[i, "name_reference"],
            " have differing number of levels."
          )
        }
      }
      # dict_diff <- dict_diff[!dict_diff$name %in% levels_diff$name, ]
    }

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
        # dict_diff[["name_reference"]],
        make.unique(dict_diff[["name_reference"]]),
        dict_diff[["name"]]
      )
    )
  }

  # If there is no survey indicator variabe, add it
  reference_dict[survey_name_ref] <- TRUE
  dict[survey_name] <- TRUE

  # If already more than 1 dictionary merged in reference_dict, manually
  # add suffix to dict
  n_indicator <- sum(map_lgl(reference_dict, is.logical))
  if (n_indicator > 1) {
    lgl_not_label <- !colnames(dict) %in% c("label", newname)
    colnames(dict)[lgl_not_label] <- paste(colnames(dict)[lgl_not_label],
      survey_name,
      sep = "_"
    )
  }

  merged <- full_join(reference_dict, dict,
    by = c(setNames(newname, newname_ref), "label"),
    suffix = c(
      paste0("_", survey_name_ref),
      paste0("_", survey_name)
    )
  ) %>%
    select(.data[[newname_ref]], everything())

  dup_ref_names <- keep(dict_diff[["name_reference"]], duplicated)

  dup_names <- setdiff(
    make.unique(dict_diff[["name_reference"]]),
    unique(dict_diff[["name_reference"]])
  )

  dup_ref_rows <- map2(dup_ref_names, dup_names, function(x, y) {
    x_rows <- which(reference_dict[[newname_ref]] == x)
    y_rows <- which(merged[[newname]] == y)
    len <- ifelse(length(x_rows) < length(y_rows),
      length(x_rows),
      length(y_rows)
    )
    # Should be looking for matching labels instead
    x_rows[seq(len)]
  }) %>%
    unlist()

  # Refactoring required
  to_fill_rows <- map2(dup_ref_names, dup_names, function(x, y) {
    x_rows <- which(reference_dict[[newname_ref]] == x)
    y_rows <- which(merged[[newname]] == y)
    len <- ifelse(length(x_rows) < length(y_rows), length(x_rows), length(y_rows))
    # Should be looking for matching labels instead
    y_rows[seq(len)]
  }) %>%
    unlist()

  to_fill_cols <- setdiff(
    grep(survey_name_ref, colnames(merged), value = T),
    survey_name_ref
  )

  merged[to_fill_rows, to_fill_cols] <-
    merged[dup_ref_rows, to_fill_cols]

  merged <- reorder(merged, newname_ref)

  return(merged)
}

# name_map_merge <- function(ref, x, name_cols = c(1, 1), name_map, ...) {
#   args <- list(...)
#   by_ref <- names(args[["by"]])
#   by_x <- args[["by"]]

#   ref_df <- as.data.frame(ref)
#   rownames(ref_df) <- ref_df[[1]]

#   x_df <- as.data.frame(x)
#   rownames(x_df) <- x_df[[1]]

#   x_df[name_map[[2]], by_x] <- ref_df[name_map[[1]], by_ref]

#   merged <- full_join(as_tibble(x_df),
#     as_tibble(ref_df),
#     by = by,
#     ...
#   )
#   return(merged)
# }
