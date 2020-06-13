dict_generate <- function(newname = "easyVariableName",
                          study_name,
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
        newname, "itemLabel", "recodeLevel",
        "QuestionID", "surveyBlock",
        "valueLabel", "questionType"
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
  amatch_is <- amatch(qt_a, qtr, maxDist = 20)

  qt_ais <- get_match(amatch_is)[[1]]
  qtr_ais <- get_match(amatch_is)[[2]]

  qt_is <- get_match(match_is)[[1]]
  qtr_is <- get_match(match_is)[[2]]

  return(
    tibble(
      name = dict[["easyVariableName"]][c(qt_ais, qt_is)],
      itemLabel = qt[c(qt_ais, qt_is)],
      name_reference = reference_dict[["easyVariableName"]][c(qtr_ais, qtr_is)],
      itemLabel_reference = qtr[c(qtr_ais, qtr_is)],
      identical = c(
        rep(FALSE, times = length(qt_ais)),
        rep(TRUE, times = length(qt_is))
      )
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
  }

  # If there is no survey indicator variabe, add it
  if (!any(map_lgl(reference_dict, is.logical))) {
    reference_dict[attr(reference_dict, "survey_name")] <- TRUE
  }

  survey_name <- attr(dict, "survey_name")
  survey_name_ref <- attr(reference_dict, "survey_name")

  merged <- full_join(reference_dict, bind_cols(
    dict,
    setNames(tibble(TRUE), survey_name)
  ),
  by = c("easyVariableName", "itemLabel", "recodeLevel"),
  suffix = c(
    paste0("_", survey_name_ref),
    paste0("_", survey_name)
  )
  ) %>%
    # Discard the duplicated generated rows
    select(easyVariableName, everything())

  duplicated_ref_names <- dict_diff$name_reference %>% subset(duplicated(.))

  to_fill <- setdiff(
    make.unique(dict_diff[["name_reference"]]),
    unique(dict_diff[["name_reference"]])
  )

  merged[merged$easyVariableName %in% to_fill, 2:6] <-
    merged[merged$easyVariableName %in% duplicated_ref_names, 2:6]

  return(merged)
}


## Or do we just get a matrix and order the similarities?
