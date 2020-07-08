dict_generate <- function(surveyID,
                          survey_name = NULL,
                          newname = "question_name",
                          block_pattern = NULL,
                          block_sep = "_",
                          split_by_block = FALSE,
                          block = NULL,
                          dict_diff = NULL,
                          import_id = TRUE) {
  easyname_gen <- ifelse(
    newname == "easyname",
    TRUE, FALSE
  )

  dict <- recode_json(surveyID,
    import_id = import_id,
    easyname_gen = easyname_gen,
    block_pattern = block_pattern,
    block_sep = block_sep
  )

  if (!is.null(block)) {
    dict <- dict[dict$block == block, ]
  }

  dict <- dict[c(
    "qid", newname, "block", "question",
    "item", "level", "label", "type"
  )]

  if (!is.null(survey_name)) {
    # Is it possible for a qualtrics survey to have no name in metadata?
    attr(dict, "survey_name") <- survey_name
  }

  if (!is.null(dict_diff)) {
    dict[[newname]] <- recode(
      dict[[newname]],
      !!!setNames(
        dict_diff[["name"]],
        make.unique(dict_diff[["name_reference"]])
      )
    )
  }
  # Temporary
  dict$item[dict$item == dict$question] <- NA

  attr(dict, "surveyID") <- surveyID

  if (!import_id) {
    dict$qid <- NULL
  }

  if (split_by_block) {
    dict <- split(dict, dict$block)
  }

  return(dict)
}
