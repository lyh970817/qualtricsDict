# Use stikcy for attributes
get_survey_data <- function(dict,
                            keys = NULL,
                            split_by_block = FALSE,
                            skip_mistakes = FALSE,
                            numeric_to_pos = FALSE,
                            numeric_to_pos_exclude = NULL,
                            na_remove_keys = TRUE,
                            ...) {
  newname <- get_newname(dict)
  # First validate the dictionary
  suppressWarnings(error_list <- dict_validate(dict)$error)
  if (!is.null(error_list$non_unique_names)) {
    return(error_list$non_unique_names)
  }

  if (!is.null(error_list$mistake_dict) > 0) {
    message("Potential errors with item recoding. Use mistakes() for details.")
  }
  skip_qids <- unique(error_list$mistake_dict[["qid"]])

  args <- list(...)
  args$force_request <- TRUE
  args$surveyID <- attr(dict, "surveyID")
  args$import_id <- TRUE
  args$convert <- FALSE
  args$label <- FALSE
  # What about text qids?
  # include_qids <- str_extract(dict[["qid"]], "QID[0-9]+")
  # args$include_questions <- include_qids

  survey <- do.call(fetch_survey, args)

  # survey_rename(survey)
  # save(survey, file = "./cache/survey.RData")
  # load("./cache/survey.RData")

  if (!is.null(skip_qids) & !skip_mistakes) {
    warning("Potential mistakes in dictionary,
            run 'dict_validate()' on the dictionary object for details or
            specify 'skip_mistakes = TRUE' to not apply recoding to
            variables with mistakes.")
  }

  if (skip_mistakes) {
    survey <- filter(survey, !qid %in% skip_qids)
  }

  if (split_by_block == TRUE) {
    keys <- unique(unlist(dict[dict[[newname]] %in% keys, "qid"]))
    keys_dat <- dict[dict[[newname]] %in% keys, ]

    block_dict <- map(
      split(dict, dict$block),
      ~ bind_rows(
        keys_dat[-match(keys_dat[[newname]], .x[[newname]])],
        .x
      ) %>%
        select(keys, everything())
    )

    return(map(block_dict, survey_recode,
      dat = survey,
      keys = keys,
      unanswer_recode = args$unanswer_recode,
      unanswer_recode_multi = args$unanswer_recode_multi,
      numeric_to_pos = numeric_to_pos
    ))
  } else {
    return(survey_recode(dict,
      dat = survey, keys = keys,
      unanswer_recode = args$unanswer_recode,
      unanswer_recode_multi = args$unanswer_recode_multi,
      numeric_to_pos = numeric_to_pos
    ))
  }
}

survey_recode <- function(dict, dat, keys, unanswer_recode, unanswer_recode_multi, numeric_to_pos) {
  unique_qids <- unique(dict[["qid"]])
  unique_newname <- unique(dict[[get_newname(dict)]])

  # How to determine which is ID column?
  keys <- c("externalDataReference", "startDate", "endDate", keys)
  dat_cols <- c(keys, unique_qids)
  newnames <- setNames(unique_qids, unique_newname)
  dat <- rename(dat[dat_cols], !!!newnames)

  # if (na_remove_keys) {
  #   na_keys_lgl <- or(map(dat[keys], is.na))
  #   dat <- dat[!na_keys_lgl, ]
  # }

  # level = unique to preserve ordering
  split_dict <- split(dict, factor(dict$qid, level = unique(dict$qid)))
  dat_vars <- map2_df(
    dat[unique_newname], split_dict,
    ~ survey_item_recode(.x, .y,
      unanswer_recode = unanswer_recode,
      unanswer_recode_multi = unanswer_recode_multi,
      numeric_to_pos = numeric_to_pos
    )
  )

  dat <- bind_cols(
    dat[keys], dat_vars,
    setNames(
      # There is a bug in this sjlabelled function
      remove_all_labels(dat[unique_newname]),
      paste(unique_newname, "numeric", sep = "_")
    )
  )

  return(dat)
}

survey_item_recode <- function(var, item_dict, unanswer_recode, unanswer_recode_multi, numeric_to_pos) {
  if (all(item_dict[["type"]] == "Text") || grepl("_TEXT", item_dict[["level"]])) {
    if (numeric_to_pos) {
      var <- abs(as.numeric(var))
    } else {
      var <- as.numeric(var)
    }
  }
  else {
    if (all(item_dict[["type"]] == "Multiple Categorical")) {
      # If only one row in dict it's multiple options
      yes <- item_dict[["label"]]
      levels <- 1
      labels <- yes
      if (!is.null(unanswer_recode_multi)) {
        levels <- c(levels, unanswer_recode_multi)
        labels <- c(labels, paste("No", yes))
      }
    }

    # If multiple rows it's ordinal
    if (nrow(item_dict) > 1) {
      labels <- item_dict[["label"]]
      levels <- item_dict[["level"]]
      labels <- grep("TEXT", labels, invert = T, value = T)
      levels <- grep("TEXT", levels, invert = T, value = T)
    }

    if (!is.null(unanswer_recode)) {
      levels <- c(levels, unanswer_recode)
      labels <- c(labels, "Seen but not answered")
    }

    tryCatch(var <- factor(var, levels = levels, labels = labels), warn = function(e) browser())
  }
  text_label <- unique(paste(item_dict[["question"]], item_dict[["item"]]))
  var <- sjlabelled::set_label(var, text_label)

  return(var)
}
