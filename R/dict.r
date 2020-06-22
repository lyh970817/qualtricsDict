dict_generate <- function(newname = "easyVariableName",
                          survey_name,
                          split_by_block = FALSE,
                          survey_start = NULL,
                          survey_end = NULL,
                          json,
                          reference_dict = NULL,
                          dict_diff = NULL,
                          qid = FALSE,
                          api_key = NULL,
                          surveyID = NULL,
                          datacenter = NULL) {
  dict <- json %>% select(
    QuestionID,
    .data[[newname]], surveyBlock,
    itemLabel, recodeLevel, valueLabel,
    questionType
  )

  attr(dict, "survey_name") <- survey_name

  if (!is.null(reference_dict)) {
    reference_dict["QuestionID"] <- NULL

    dict <- if (is.null(dict_diff)) {
      dict_merge(dict, reference_dict)
    } else {
      dict_merge(dict, reference_dict, dict_diff)
    }
    dict <- dict %>%
      filter(.data[[survey_name]]) %>%
      select(
        -contains(attr(reference_dict, "survey_name")),
        -.data[[survey_name]]
      ) %>%
      setNames(c(
        newname, "itemLabel", "valueLabel",
        "QuestionID", "surveyBlock",
        "recodeLevel", "questionType"
      )) %>%
      select(
        QuestionID,
        .data[[newname]], surveyBlock,
        itemLabel, recodeLevel, valueLabel,
        questionType
      )
  }

  if (!qid) {
    dict <- select(dict, -QuestionID)
  }

  if (split_by_block) {
    return(split(dict, dict$surveyBlock))
  } else {
    return(dict)
  }
}

dict_compare <- function(dict, reference_dict) {
  qtr <- reference_dict[["itemLabel"]]
  qt <- dict[["itemLabel"]]
  qt_a <- ifelse(qt %in% qtr, NA, qt)

  match_is <- match(qt, qtr)
  amatch_is <- amatch(qt_a, qtr, maxDist = 1000)

  qt_ais <- get_match(amatch_is)[[1]]
  qtr_ais <- get_match(amatch_is)[[2]]

  qt_is <- get_match(match_is)[[1]]
  qtr_is <- get_match(match_is)[[2]]

  name <- dict[["easyVariableName"]][c(qt_ais, qt_is)]

  labels <- map(name, ~ dict %>%
    filter(easyVariableName == .x) %>%
    select(valueLabel) %>%
    unlist())

  name_reference <- reference_dict[["easyVariableName"]][c(qtr_ais, qtr_is)]
  labels_ref <- map(name_reference, ~ reference_dict %>%
    filter(easyVariableName == .x) %>%
    select(valueLabel) %>%
    unlist())

  label_match <- map2_lgl(labels, labels_ref, ~ identical(.x, .y))

  return(
    tibble(
      name = dict[["easyVariableName"]][c(qt_ais, qt_is)],
      itemLabel = qt[c(qt_ais, qt_is)],
      n_levels = map_dbl(labels, length),
      name_reference = reference_dict[["easyVariableName"]][c(qtr_ais, qtr_is)],
      itemLabel_reference = qtr[c(qtr_ais, qtr_is)],
      n_levels_ref = map_dbl(labels_ref, length),
      identical = c(
        rep(FALSE, times = length(qt_ais)),
        rep(TRUE, times = length(qt_is))
      ),
      label_match = label_match
    ) %>%
      # Do we still need this?
      .[!duplicated(.), ] %>%
      na.omit()
  )
}

dict_merge <- function(dict,
                       reference_dict,
                       dict_diff = NULL,
                       upload = NULL) {
  if (attr(reference_dict, "survey_name") == attr(dict, "survey_name")) {
    stop("Dictionaries to be merged are from the same questionnaire.")
  }

  if (any(duplicated(dict_diff[["name"]]))) {
    stop("Non-unique mapping in dict_diff.")
  }

  if (is.null(dict_diff)) {
    message("Consider using 'dict_compare' to track potential matching items")
  } else if (nrow(dict_diff) > 0) {
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

    dict <- dict %>%
      mutate(
        itemLabel = recode(
          itemLabel,
          !!!setNames(
            dict_diff[["itemLabel_reference"]],
            dict_diff[["itemLabel"]]
          )
        ),
        easyVariableName = recode(
          easyVariableName,
          !!!setNames(
            make.unique(dict_diff[["name_reference"]]),
            dict_diff[["name"]]
          )
        )
      )

    dict["valueLabel"] <- map(unique(dict$easyVariableName), function(x) {
      if (x %in% dict_diff[["name"]]) {
        sub_dict_diff <- dict_diff %>% filter(name == x)
        n_levels <- sub_dict_diff %>% select(n_levels)
        n_levels_ref <- sub_dict_diff %>% select(n_levels_ref)
        if (n_levels == n_levels_ref) {
          name_ref <- sub_dict_diff[["name_reference"]]
          valueLabel <- filter(reference_dict, easyVariableName == name_ref) %>% select(valueLabel)
        }
        else {
          valueLabel <- filter(dict, easyVariableName == x) %>% select(valueLabel)
        }
      }
      else {
        valueLabel <- filter(dict, easyVariableName == x) %>% select(valueLabel)
      }
      return(valueLabel)
    }) %>%
      bind_rows()
  }
  # If there is no survey indicator variabe, add it
  if (!any(map_lgl(reference_dict, is.logical))) {
    reference_dict[attr(reference_dict, "survey_name")] <- TRUE
  }

  survey_name <- attr(dict, "survey_name")
  survey_name_ref <- attr(reference_dict, "survey_name")

  merged <- full_join(reference_dict,
    bind_cols(dict, setNames(tibble(TRUE), survey_name)),
    by = c("easyVariableName", "itemLabel", "valueLabel"),
    suffix = c(
      paste0("_", survey_name_ref),
      paste0("_", survey_name)
    )
  ) %>%
    # Discard the duplicated generated rows
    select(easyVariableName, everything())

  duplicated_ref_names_is <- dict_diff$name_reference %>%
    subset(duplicated(.)) %>%
    map(~ which(reference_dict$easyVariableName == .x)) %>%
    unlist()

  to_fill <- setdiff(
    make.unique(dict_diff[["name_reference"]]),
    unique(dict_diff[["name_reference"]])
  ) %>%
    map(~ which(merged$easyVariableName == .x)) %>%
    unlist()

  if (length(to_fill) != length(duplicated_ref_names_is)) {
    stop("Check if questions with matching names have the same number of levels.")
  }

  merged[to_fill, 2:6] <- merged[duplicated_ref_names_is, 2:6]

  return(merged)
}


## Or do we just get a matrix and order the similarities?
