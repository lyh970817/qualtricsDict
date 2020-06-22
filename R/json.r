library(qualtRics)
library(tidyverse)

qualtrics_api_credentials(
  api_key = "lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw",
  base_url = "https://eu.qualtrics.com",
  install = FALSE,
  overwrite = T
)
surveyID <- "SV_0DrSSOISyMOqN5r"

mt <- metadata(surveyID,
  get = list(
    "questions" = TRUE,
    "metadata" = TRUE,
    "blocks" = TRUE,
    "responsecounts" = FALSE
  )
)

remove_format <- function(dat) {
  mutate_if(
    dat, is.character,
    ~ str_remove_all(., "<[^>]+>|\\n") %>%
      str_remove_all("Selected Choice - ")
  )
}

null_na <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}

question_meta <- map(mt$questions, `[`, c("questionType", "questionText", "blocks", "columns", "choices", "subQuestions"))

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

  has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))
  has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))

  if (length(has_text) > 0) {
    # Add text level and labels directly after the non-text level
    for (i in has_text) {
      level <- c(
        level[1:i],
        paste(level[i], sep = "_", "HASTEXT"),
        # This will produce NA, needs removal
        level[i + 1:length(level)]
      ) %>%
        discard(is.na)

      label <- c(
        label[1:i],
        paste(label[i], sep = "_", "HASTEXT"),
        label[i + 1:length(label)] %>%
          discard(is.na)
      )
    }
  }

  if (!is.null(label)) {
    which_na <- label == "N/A"
    label <- label[!which_na]
    level <- level[!which_na]
  }

  item <- unlist(map(qjson$subQuestions, "choiceText"))
  if (length(has_text_sub) > 0) {
    for (i in has_text_sub) {
      item <- c(
        item[1:i],
        paste(item[i], sep = "_", "HASTEXT"),
        # This will produce NA, needs removal
        item[i + 1:length(level)]
      ) %>%
        discard(is.na)
      sub_q_len <- sub_q_len + 1
    }
  }
  sub_selector <- qjson$questionType$subSelector

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
    # These are continuous variables with some strange question index
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
    else if (type == "TE") {
      if (selector == "FORM") paste(qid, sep = "_", seq_along(unique(level)))
      if (selector != "FORM") paste(qid, sep = "_", "TEXT")
    }
    else if (type == "Matrix") {
      paste(qid, sep = "_", seq_along(item)) %>%
        rep(each = choice_len)
    }
    else if (type == "Slider") {
      paste(qid, sep = "_", seq_along(item))
    }
    else {
      qid
    }
  print(qid)
  tryCatch(
    t <- tibble(
      # new_qid,
      qid,
      question,
      level = rep(level, times = sub_q_len) %>% null_na(),
      label = rep(label, times = sub_q_len) %>% null_na(),
      item = rep(item, each = choice_len) %>% null_na(),
      type,
      selector, sub_selector = null_na(sub_selector)
    ),
    error = function(e) browser()
  )
  return(t)
}) %>%
  bind_rows() %>%
  remove_format()

qnames <- grep("QID", colnames(survey), v = T)
q_split <- json %>%
  mutate(qid = str_replace(qid, "_.+", "")) %>%
  split(.$qid) %>%
  imap(function(x, n) {
    c_names_i <- which(str_replace(qnames, "(#[0-9])?_.+", "") == n)

    if (length(c_names_i) == 0) {
      return(x)
    }

    c_names <- qnames[c_names_i]

    non_text_qids <- c_names %>%
      grep("TEXT", ., value = T, invert = T) %>%
      rep(times = nrow(x[!grepl("HASTEXT", x$label), "qid"]) / length(.))

    text_qids <- c_names %>%
      grep("TEXT", ., value = T)

    if (length(non_text_qids) > 0) {
      x[
        !(grepl("HASTEXT", x$label) |
          grepl("HASTEXT", x$item) |
          x$type == "TE"
        ),
        "qid"
      ] <-
        non_text_qids
    }

    if (length(text_qids) > 0) {
      x[
        grepl("HASTEXT", x$label) |
          grepl("HASTEXT", x$item) |
          x$type == "TE",
        "qid"
      ] <-
        text_qids
    }

    return(x)
  }) %>%
  bind_rows()

not_in <- q_split$qid[!q_split$qid %in% colnames(survey)]
not_in <- colnames(survey)[!colnames(survey) %in% q_split$qid]
not_in_d <- json %>% filter(qid %in% not_in)
print(not_in_d %>% select(qid, question), n = 100)
grep("QID124957050", q_split$qid, v = T)

unique(json$type)

json %>%
  filter(type == "SBS") %>%
  select(qid) %>%
  pull()

print(json %>%
  filter(type == "SBS"), n = 100)

unique(json$type)

json_block <- mt$blocks %>%
  # set the names to block names so we can enframe
  setNames(map_chr(., "description")) %>%
  map("elements") %>%
  map(~ unlist(map(.x, "questionId"))) %>%
  enframe(value = "qid", name = "block") %>%
  unnest(qid)

nrow(json_block)
length(unique(json$qid))

dict <- left_join(json, json_block)
dict <- dict %>%
  mutate()
unique(dict$block)

pcl <- dict %>%
  filter(block == "COVID_Measures_PCL6") %>%
  select(-qid)

print(pcl$new_qid)
mysurvey <- fetch_survey(
  # force_request = T,
  surveyID = surveyID,
  verbose = TRUE,
  import_id = T,
  limit = 10,
  unanswer_recode = -99,
  unanswer_recode_multi = 0,
  force_request = T,
)
mysurvey[grep("124964827", colnames(mysurvey))] %>%
  map(attributes)


readRenviron("~/.Renviron")
surveys <- all_surveys()
