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
    .data[[newname]], surveyBlock,
    itemLabel, recodeLevel, valueLabel
  )

  if (qid) {
    dict <- bind_cols(json["QuestionID"], dict)
  }

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
      setNames(c(newname, "itemLabel", "recodeLevel", "QuestionID", "surveyBlock", "valueLabel")) %>%
      select(
        .data[[newname]], surveyBlock,
        itemLabel, recodeLevel, valueLabel
      )
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
  qt[qt %in% qtr] <- NA

  amatch_is <- amatch(qt, qtr, maxDist = 20)
  qt_is <- which(!is.na(amatch_is))
  qtr_is <- discard(amatch_is, is.na)

  return(
    tibble(
      name = dict[["easyVariableName"]][qt_is],
      itemLabel = qt[qt_is],
      name_reference = reference_dict[["easyVariableName"]][qtr_is],
      itemLabel_reference = qtr[qtr_is]
    ) %>%
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

  if (any(duplicated(dict_diff[["name"]])) ||
    any(duplicated(dict_diff["name_reference"]))) {
    stop("Non-unique mapping in dict_diff.")
  }

  qt <- dict[["itemLabel"]]
  qtr <- reference_dict[["itemLabel"]]

  match_is <- match(qt, qtr)
  qt_is <- which(!is.na(match_is))
  qtr_is <- discard(match_is, is.na)
  dict$easyVariableName[qt_is] <- reference_dict$easyVariableName[qtr_is]

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
            dict_diff[["name_reference"]],
            dict_diff[["name"]]
          )
        )
      )
  }

  # If there is no survey indicator, add it
  if (!any(map_lgl(reference_dict, is.logical))) {
    reference_dict[attr(reference_dict, "survey_name")] <- TRUE
  }

  survey_name <- attr(dict, "survey_name")
  survey_name_ref <- attr(reference_dict, "survey_name")

  return(
    full_join(reference_dict, bind_cols(
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
  )
}


## Or do we just get a matrix and order the similarities?
