recode_json <- function(surveyID, import_id = TRUE, easyname_gen = T) {
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
      "questionType", "questionText",
      "blocks", "columns",
      "choices", "subQuestions"
    )
  )

  json <- imap(question_meta, function(qjson, qid) {

    # Make sure length is at least one so 'rep' won't empty a variable
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
    choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    # Compulsory variables
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector

    level <- unlist(map(qjson$choices, "recode"))
    label <- unlist(map(qjson$choices, "choiceText"))
    sub_selector <- qjson$questionType$subSelector

    # These do not appear in colnames
    if (!is.null(label)) {
      which_na <- label == "N/A"
      label <- label[!which_na]
      level <- level[!which_na]
    }

    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))
    has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))

    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    item <- unlist(map(qjson$subQuestions, "choiceText"))
    if (length(has_text_sub) > 0) {
      item <- add_text(item, has_text_sub)
      sub_q_len <- sub_q_len + 1
    }

    if (type == "SBS") {
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

      item <- unlist(map(qjson$subQuestions, "choiceText"))
    }

    if (type == "Slider") {
      choice_len <- 1
      sub_q_len <- 1
      level <- NA
    }

    tibble(
      qid, question,
      item = rep(item, each = choice_len) %>% null_na(),
      level = rep(level, times = sub_q_len) %>% null_na(),
      label = rep(label, times = sub_q_len) %>% null_na(),
      type, selector,
      sub_selector = null_na(sub_selector)
    )
  }) %>%
    bind_rows() %>%
    remove_format()

  blocks <- mt$blocks %>%
    # set the names to block names so we can enframe
    setNames(map_chr(., "description")) %>%
    map("elements") %>%
    map(~ unlist(map(.x, "questionId"))) %>%
    enframe(value = "qid", name = "block") %>%
    unnest(qid)

  json <- left_join(json, blocks) %>%
    select(qid, block, everything())

  if (import_id) {
    json <- recode_qids(json, surveyID)
  }
  if (easyname_gen) {
    json <- easyname_gen(json)
  }

  return(json)
}

recode_qids <- function(json, surveyID) {
  survey <- suppressMessages(fetch_survey(surveyID,
    import_id = TRUE, convert = FALSE,
    label = FALSE, force_request = TRUE,
    limit = 1,
  )) %>% survey_rename()

  # Just take the intersection between nosfx and qids in the dictionary and
  # replace the second row

  # Second idea is to recode the duplicated qid in readr or make.unique way
  # and do the same for the survey data frame.
  qid_cols_all <- grep("QID", colnames(survey), value = T)
  qid_cols_nosfx <- str_replace(qid_cols_all, "(#[0-9])?_.+", "")

  json_sfx <- json %>%
    split(.$qid) %>%
    imap(function(x, n) {
      qid_cols <- qid_cols_all[which(qid_cols_nosfx == n)]
      if (length(qid_cols) == 0) {
        # Note qid_cols can be zero as welcome messages are not exported
        # but are in json
      }
      else if (all(grepl("TEXT", qid_cols))) {
        x["qid"] <- qid_cols
      }
      else {
        # If without subquestions TEXT is in label, is in item if with subquestions
        has_text_lgl <-
          grepl("TEXT", x$label) |
            grepl("TEXT", x$item)

        non_text_qids <- qid_cols %>%
          grep("TEXT", ., value = T, invert = T) %>%
          rep(each = nrow(x[!has_text_lgl, "qid"]) / length(.))

        text_qids <- qid_cols %>%
          grep("TEXT", ., value = T)

        if (length(non_text_qids) > 0) {
          x[!has_text_lgl, "qid"] <- non_text_qids
        }
        if (length(text_qids) > 0) {
          x[has_text_lgl, "qid"] <- text_qids
        }
      }
      return(x)
    }) %>%
    bind_rows() %>%
    filter(qid %in% colnames(survey))
}

recode_type <- function(json) {
  json <- mutate(json,
    type = case_when(
      type == "MC" ~ "Categorical",
      type == "TE" ~ "Text",
      type == "Slider" ~ "Continuous",
      selector == "Likert" ~ "Ordinal",
      TRUE ~ type
    )
  )
}

add_text <- function(x, has_text) {
  if (!is.null(x)) {
    for (i in has_text) {
      x <- c(
        x[1:i],
        paste(x[i], sep = "_", "TEXT"),
        # This will produce NA, needs removal
        x[i + 1:length(x)]
      ) %>%
        discard(is.na)
    }
    return(x)
  }
}
