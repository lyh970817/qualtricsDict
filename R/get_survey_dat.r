get_survey_dat <- function(newname = "easyVariableName",
                           split_by_block = FALSE,
                           keys = NULL,
                           # survey_start = NULL,
                           # survey_end = NULL,
                           survey_start = NULL,
                           survey_end = NULL,
                           numeric_to_pos = FALSE,
                           # numeric_to_pos = FALSE (exclude columns),
                           # If no exclude warning
                           # dict,
                           # Just use dict
                           dict,
                           # api_key,
                           # surveyID,
                           # datacenter,
                           api_key,
                           surveyID,
                           datacenter,
                           # Use stikcy for attributes
                           ...) {

  # Change api_key and survey_id for consistency

  qualtrics_api_credentials(
    api_key = api_key,
    base_url = "https://eu.qualtrics.com",
    install = F,
    overwrite = T
  )

  args <- list(...)
  args$force_request <- TRUE
  args$surveyID <- surveyID
  args$import_id <- TRUE
  args$convert <- FALSE
  args$label <- FALSE

  # survey <- do.call(fetch_survey, args)
  # save(survey, file = "./cache/survey.RData")
  load(file = "./cache/survey.RData")

  qid_pattern <- '\\{"ImportId":"(.+)".+'
  qid_pattern_choice <- '\\{"ImportId":"(.+)","choiceId":"([-0-9]+)"'

  grep(qid_pattern, colnames(survey), v = T)
  qid_rename <- str_match(colnames(survey), qid_pattern)[, 2]
  qid_rename_choice <- str_match(colnames(survey), qid_pattern_choice)
  qid_choices <- paste(qid_rename_choice[, 2], qid_rename_choice[, 3], sep = "_")
  qid_rename[grep("choiceId", qid_rename)] <- qid_choices[grep("choiceId", qid_rename)]

  names(survey) <- qid_rename

  survey_start <- if (!is.null(survey_start)) {
    grep(survey_start, dict[["qualtricsName"]])[1]
  } else {
    1
  }

  survey_end <- if (!is.null(survey_end)) {
    rev(grep(survey_end, dict[["qualtricsName"]]))[1]
  } else {
    nrow(dict)
  }

  dict <- dict[survey_start:survey_end, ]

  if (length(newname) > 1) {
    uni_qids <- unique(dict$QuestionID)

    if (length(newname) != length(uni_qids)) {
      stop("Length of new names don't match rows in dict")
    }

    othernames <- newname %>%
      setNames(uni_qids) %>%
      recode(dict$QuestionID, !!!.)

    dict <- bind_cols(tibble(othername = othernames))
    newname <- "othername"
  }

  # survey_recode(dict, survey)

  browser()
  survey_recode <- function(dict, dat) {
    non_unique_names <- check_names(dict, newname)
    non_unique_names[[2]][[1]]

    # write_csv(x = non_unique_names[[2]], path = "./duplicated_names.csv")
    if (any(map_dbl(non_unique_names, nrow) > 0)) {
      return(non_unique_names)
      stop("There are non-unique name mappings.")
    }

    qids <- dict[c("QuestionID", newname)] %>%
      subset(!duplicated(.))

    dat_cols <- c("Login ID", keys, "startDate", "endDate", unique(qids[["QuestionID"]]))

    newnames <- setNames(qids[["QuestionID"]], qids[[newname]])
    dat <- rename(dat[dat_cols], !!!newnames)

    split_dict <- split(dict, factor(dict$QuestionID))
    mistake_dict <- check_jsons(split_dict)
    skip_qids <- unique(mistake_dict[["qid"]])

    survey_item_recode <- function(var, item_dict) {
      if (is.na(item_dict[["recodeLevel"]]) &&
        # No recode and not text entry, is numerc
        item_dict[["questionType"]] != "Text entry") {
        if (numeric_to_pos) {
          var <- abs(as.numeric(var))
        } else {
          var <- as.numeric(var)
        }
      }

      if (!is.na(item_dict[["recodeLevel"]])) {

        # If only one row in dict it's multiple options
        if (nrow(item_dict) == 1) {
          yes <- item_dict[["valueLabel"]]
          levels <- 1
          labels <- yes
          if (exists("unanswer_recode_multi")) {
            levels <- c(levels, unanswer_recode_multi)
            labels <- c(labels, paste("No", yes))
          }
        }

        # If multiple rows it's ordinal
        if (nrow(item_dict) > 1) {
          levels <- item_dict[["recodeLevel"]]
          labels <- item_dict[["valueLabel"]]
        }

        if (exists("unanswer_recode")) {
          levels <- c(levels, unanswer_recode)
          labels <- c(labels, "Seen but not answered")
        }
      }

      var <- factor(var, levels = levels, labels = labels)

      attr(var, "label") <- item_dict[["questionText"]]
      attr(var, "names") <- NULL

      return(var)
    }

    dat <- dat[names(newnames)] %>%
      modify2(., split_dict, survey_item_recode) %>%
      bind_cols(
        select(dat, -names(newnames)),
        .,
        setNames(dat[names(newnames)], paste(names(newnames), "numeric", sep = "_"))
      )

    unique_pairs <- split_dict %>%
      map(select, valueLabel, recodeLevel) %>%
      enframe(value = "pair") %>%
      group_by(pair) %>%
      summarize(qid = list(name), .groups = "drop")

    attr(dat, "unique_pairs") <- unique_pairs

    if (length(skip_qids) > 0) {
      attr(dat, "mistakes") <- mistake_dict
    }

    return(dat)
  }

  if (split_by_block == TRUE) {
    keys <- dict %>%
      filter(questionName %in% keys) %>%
      select(qid) %>%
      pull() %>%
      unique()

    comm_vars <- filter(dict, questionName %in% keys)
    block_dict <- suppressWarnings(map(
      split(dict, dict$block),
      ~ bind_rows(
        comm_vars[-match(comm_cols$questionName, .x$questionName)],
        .x
      )
    ))

    return(map(block_dict, survey_recode, dat = survey))
  } else {
    return(survey_recode(dict, dat = survey))
  }
}
