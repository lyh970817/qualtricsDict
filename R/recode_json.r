recode_json <- function(surveyID, import_id,
                        easyname_gen, block_pattern, block_sep) {
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

  json <- imap(question_meta, function(qjson, qid) {

    # Make sure length is at least one so 'rep' won't empty a variable
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
    choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    # Compulsory variables
    question_name <- qjson$questionName
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

    new_qid <-
      if (type == "SBS") {
        paste(qid, sep = "#", seq_along(unique(question))) %>%
          map2(level_lens, rep) %>%
          unlist() %>%
          paste(sep = "_", rep(seq_along(unique(item)), each = sum(level_lens)))
      }
      else if (type == "TE" & selector != "FORM") {
        paste(qid, sep = "_", "TEXT")
      }
      else if (type == "TE" & selector == "FORM") {
        paste(qid, sep = "_", seq_along(unique(label)))
      }
      else if (type == "TE" & selector != "FORM") {
        paste(qid, sep = "_", "TEXT")
      }
      else if (type == "Matrix") {
        paste(qid, sep = "_", names(item)) %>%
          rep(each = choice_len)
      }
      else if (type == "Slider") {
        paste(qid, sep = "_", seq_along(item))
      }
      else if (selector == "MACOL" | selector == "MAVR") {
        paste(qid, sep = "_", level)
      }
      # WHat should the logic really be here?
      else if (any(grepl("TEXT", level))) {
        paste(qid, sep = "_", level) %>%
          str_remove("_-?[0-9]+$")
      }
      else {
        qid
      }

    # if (qid == "QID68") {
    #
    # }
    tibble(
      new_qid,
      qid, question_name, question,
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


  json <- left_join(json, blocks, by = "qid") %>%
    mutate(qid = new_qid) %>%
    select(qid, block, everything())

  # json <- recode_type(json)

  # if (import_id) {
  #   json <- recode_qids(json, surveyID)
  # }
  if (easyname_gen) {
    json <- easyname_gen(json, block_pattern, block_sep)
  }


  attr(json, "survey_name") <- mt$metadata$name
  attr(json, "surveyID") <- surveyID

  return(json)
}

recode_qids <- function(json, surveyID) {
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

  survey <- survey_rename(survey)

  colnames_survey <- colnames(survey)
  colnames_survey_nosfx <- str_extract(colnames_survey, "QID[0-9]+")

  json_sfx <- json %>%
    split(.$qid) %>%
    imap(function(x, n) {
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
      selector == "Likert" ~ "Ordinal",
      selector == "MACOL" ~ "Multiple Categorical",
      type == "TE" | grepl("TEXT", qid) ~ "Text",
      type == "MC" ~ "Categorical",
      type == "Slider" ~ "Continuous",
      TRUE ~ type
    )
  )
  return(json)
}

add_text <- function(x, has_text) {
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i]
      text <- names(has_text)[i]
      x <- c(
        x[1:pos],
        paste(text, sep = "_", "TEXT"),
        # This will produce NA, needs removal
        x[pos + 1:length(x)]
      ) %>%
        discard(is.na)
    }
    return(x)
  }
}
