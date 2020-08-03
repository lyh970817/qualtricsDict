recode_json <- function(surveyID, import_id,
                        easyname_gen, block_pattern, block_sep) {
  suppressMessages(
    suppressWarnings(
      invisible(capture.output(
        survey <- # Hides the progress bar
          fetch_survey(surveyID,
            import_id = TRUE, convert = FALSE,
            label = FALSE, force_request = TRUE,
            limit = 1,
          )
      ))
    )
  )

  qids_data <- str_extract(colnames(survey), "QID[0-9]+") %>%
    discard(is.na)

  mt <- metadata(surveyID,
    get = list(
      "questions" = TRUE,
      "metadata" = TRUE,
      "blocks" = TRUE,
      "responsecounts" = FALSE
    )
  )

  question_meta <- map(
    mt$questions, `[`,
    c(
      "questionName",
      "questionType", "questionText",
      "blocks", "columns",
      "choices", "subQuestions"
    )
  )

  blocks <- mt$blocks %>%
    # set the names to block names so we can enframe
    setNames(map_chr(., "description")) %>%
    map("elements") %>%
    map(~ unlist(map(.x, "questionId"))) %>%
    enframe(value = "qid", name = "block") %>%
    unnest(qid)

  question_meta <- question_meta[unique(qids_data)]
  blocks <- pull(blocks[blocks$qid %in% unique(qids_data), "block"])
  question_meta <- map2(question_meta, blocks, function(x, y) {
    x["block"] <- y
    return(x)
  })


  json <- imap(question_meta, function(qjson, qid) {
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
    choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    # Compulsory variables
    question_name <- qjson$questionName
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector
    block <- qjson$block

    # Levels have recodes and original levels
    # Labels have lbels and original levels
    # Subquestions have questions and original levels

    level <- unlist(map(qjson$choices, "recode"))
    label <- unlist(map(qjson$choices, "choiceText"))

    # Discard qids that are not in the datafile

    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))

    # if (qid == "QID644") {
    #
    # }

    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))
    # subQuestions instead of items
    item <- unlist(map(qjson$subQuestions, "choiceText"))
    sub_selector <- qjson$questionType$subSelector
    if (length(has_text_sub) > 0) {
      item <- add_text(item, has_text_sub)
      sub_q_len <- sub_q_len + 1
    }

    # if (qid == "QID682") {
    #   browser()
    # }

    # Zero length columns means it's a carried forward question
    if (type == "SBS" & length(qjson$columns) != 0) {
      # Reuse matrix code
      level_lens <- map(qjson$columns, "choices") %>% map_dbl(length)
      choice_len <- sum(level_lens)

      question <- map(qjson$columns, "questionText") %>%
        map2(level_lens, ~ rep(.x, each = .y)) %>%
        unlist() %>%
        rep(times = sub_q_len)

      level <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "recode")) %>%
        unlist()

      label <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "description")) %>%
        unlist()

      # Supposedly we can easily add text here but needs testing
      item <- unlist(map(qjson$subQuestions, "choiceText"))
      qid <- paste(qid, sep = "#", seq(length(qjson$columns))) %>%
        map2(level_lens, rep) %>%
        unlist() %>%
        rep(times = length(item))
    }


    if (type == "Slider") {
      choice_len <- 1
      sub_q_len <- 1
      slider_level <- level
      level <- NA
    }


    t <- tryCatch(
      tibble(
        qid,
        block = block,
        question_name, question,
        item = rep(item, each = choice_len) %>% null_na(),
        level = rep(level, times = sub_q_len) %>% null_na(),
        label = rep(label, times = sub_q_len) %>% null_na(),
        type, selector,
        sub_selector = null_na(sub_selector)
      ),
      error = function(e) {
        warning("Unsuccessful question ", qid)
        return(NULL)
      }
    )

    return(t)
  }) %>%
    discard(is.null) %>%
    bind_rows() %>%
    # Don't get rid of bracekts in labels
    remove_format(skip = "label")


  json <- recode_type(json)

  if (import_id) {
    json <- recode_qids(json, survey)
  }
  if (easyname_gen) {
    json <- easyname_gen(json, block_pattern, block_sep)
  }


  attr(json, "survey_name") <- as.character(mt$metadata$name)
  attr(json, "surveyID") <- surveyID

  return(json)
}

recode_qids <- function(json, survey) {
  colnames_survey <- str_remove(colnames(survey), "_$")
  colnames_survey_nosfx <- str_extract(colnames_survey, "^QID[0-9]+(#[0-9]+)?")

  json_sfx <- json %>%
    split(.$qid) %>%
    imap(function(x, n) {
      # print(n)
      if (n == "QID694") {
        # What's going on here??
        return(x)
      }
      unique_qids <- colnames_survey[which(colnames_survey_nosfx == n)]
      if (length(unique_qids) == 0) {
        # Note unique_qids can be zero as welcome messages are not exported
        # but are in json
      }
      else if (all(grepl("TEXT", unique_qids))) {
        x["qid"] <- unique_qids
      }
      else {
        # If without subquestions TEXT is in label, is in item if with subquestions
        # Do we really need this? Need to determine if the ordering of the
        # text colulmns are done correctly
        has_text_lgl <-
          grepl("TEXT", x$label) |
            grepl("TEXT", x$item)

        non_text_qids <- unique_qids %>%
          grep("TEXT", ., value = T, invert = T) %>%
          rep(each = nrow(x[!has_text_lgl, "qid"]) / length(.))

        text_qids <- unique_qids %>%
          grep("TEXT", ., value = T)

        if (length(non_text_qids) > 0) {
          if (nrow(x[!has_text_lgl, "qid"]) != length(non_text_qids)) {
            warning("Excluded levels in ", n, " is not supported")
          }
          x[!has_text_lgl, "qid"][seq(length(non_text_qids)), ] <- non_text_qids
        }
        if (length(text_qids) > 0) {
          tryCatch(x[has_text_lgl, "qid"] <- text_qids, error = function(e) browser())
        }
      }
      return(x)
    }) %>%
    bind_rows() %>%
    filter(qid %in% colnames_survey)
}

recode_type <- function(json) {
  json <- mutate(json,
    type = case_when(
      selector == "Likert" ~ "Ordinal",
      selector == "MACOL" | selector == "MAVR" ~ "Multiple Categorical",
      type == "TE" | grepl("TEXT", qid) ~ "Text",
      type == "MC" ~ "Categorical",
      type == "Slider" ~ "Continuous",
      TRUE ~ type
    )
  )
  return(json)
}

add_text <- function(x, has_text, label = F) {
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i] + (i - 1)
      text <- names(x)[pos]
      text_nm <- x[pos]
      x <- append(x,
        paste(text_nm, sep = "_", "TEXT"),
        after = pos
      )

      # Required for sub
      names(x)[pos + 1] <- paste(text, sep = "_", "TEXT")
    }
    return(x)
  }
}
