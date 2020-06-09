get_survey_dat <- function(newname = "easyVariableName",
                           split_by_block = FALSE,
                           keys = NULL,
                           survey_start = NULL,
                           survey_end = NULL,
                           numeric_to_pos = FALSE,
                           json,
                           dict,
                           api_key,
                           surveyID,
                           datacenter,
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
  # save(survey, file = "../cache/survey.RData")
  load(file = "../cache/survey.RData")

  qid_pattern <- ':\\"(.+?)\\"'
  qid_rename <- str_match(colnames(survey), qid_pattern)[, 2]
  names(survey) <- qid_rename

  survey_start <- if (!is.null(survey_start)) {
    grep(survey_start, json[["qualtricsName"]])[1]
  } else {
    1
  }

  survey_end <- if (!is.null(survey_end)) {
    rev(grep(survey_end, json[["qualtricsName"]]))[1]
  } else {
    nrow(json)
  }

  json <- json[survey_start:survey_end, ]

  if (length(newname) > 1) {
    uni_qids <- unique(json$QuestionID)

    if (length(newname) != length(uni_qids)) {
      stop("Length of new names don't match rows in json")
    }

    othernames <- newname %>%
      setNames(uni_qids) %>%
      recode(json$QuestionID, !!!.)

    json <- bind_cols(tibble(othername = othernames))
    newname <- "othername"
  }

  # survey_recode(json, survey)

  survey_recode <- function(json, dat) {
    non_unique_names <- check_names(json, newname)
    non_unique_names[[2]][[1]]

    # write_csv(x = non_unique_names[[2]], path = "./duplicated_names.csv")
    if (any(map_dbl(non_unique_names, nrow) > 0)) {
      return(non_unique_names)
      stop("There are non-unique name mappings.")
    }

    qids <- json[["QuestionID"]]
    dat_cols <- c("externalDataReference", keys, "startDate", "endDate", qids)

    newnames <- setNames(json[[newname]], qids)
    dat <- rename(dat[dat_cols], !!!newnames)

    split_jsons <- split(json, factor(json$QuestionID))
    mistake_jsons <- check_jsons(split_jsons)
    skip_qids <- unique(mistake_jsons[["qid"]])

    all_cols <- json %>% filter(QuestionID %in% skip_qids)
    write_csv(mistake_jsons, path = "../cache/mistakes.csv")
    write_csv(all_cols, path = "../cache/mistakes_allcols.csv")

    # test <- read_csv("../cache/mistakes_allcols.csv")

    # json[json$QuestionID == "QID124934471", "QuestionID"] %>% na.omit()
    # all_cols[all_cols$QuestionID == "QID124934471", "QuestionID"] %>% na.omit()

    # mistake_jsons %>% filter(qid == "QID613")
    # print(mistake_jsons, n = 100)

    survey_item_recode <- function(var, item_json) {
      attr(var, "label") <- item_json[["text"]]

      if (is.na(item_json[["recode"]]) &&
        item_json[["questionType"]] == "Text entry") {
        return(var)
      }

      if (is.na(item_json[["recode"]]) &&
        # No recode and not text entry, is numerc
        item_json[["questionType"]] != "Text entry") {
        if (numeric_to_pos) {
          return(abs(as.numeric(var)))
        } else {
          return(as.numeric(var))
        }
      }

      # If only one row in json it's multiple options
      if (nrow(item_json) == 1) {
        yes <- item_json[["description"]]
        levels <- 1
        labels <- yes
        if (!is.null(unanswer_recode_multi)) {
          levels <- c(levels, unanswer_recode_multi)
          labels <- c(labels, paste("No", yes))
        }
      }

      # If multiple rows it's ordinal
      if (nrow(item_json) > 1) {
        levels <- item_json[["recode"]]
        labels <- item_json[["description"]]
      }

      if (!is.null(unanswer_recode)) {
        levels <- c(levels, unanswer_recode)
        labels <- c(labels, "Seen but not answered")
      }

      return(factor(var,
        levels = levels,
        labels = labels
      ))
    }

    dat[qids] <- modify2(dat[qids], split_jsons, survey_item_recode)

    unique_pairs <- split_jsons %>%
      map(select, description, recode) %>%
      enframe(value = "pair") %>%
      group_by(pair) %>%
      summarize(qid = list(name), .groups = "drop")

    attributes(dat, "unique_pairs") <- unique_pairs

    if (length(skip_qids) > 0) {
      attributes(dat, "mistakes") <- mistake_jsons
    }
  }

  if (split_by_block == TRUE) {
    keys <- json %>%
      filter(questionName %in% keys) %>%
      select(qid) %>%
      pull() %>%
      unique()

    comm_vars <- filter(json, questionName %in% keys)
    block_jsons <- suppressWarnings(map(
      split(json, json$block),
      ~ bind_rows(
        comm_vars[-match(comm_cols$questionName, .x$questionName)],
        .x
      )
    ))
    # json <- block_jsons[["COVID_Baseline_Demographics"]]
    map(block_jsons, survey_recode, dat = survey)
  } else {
    survey_recode(json, dat = survey)
  }
}

