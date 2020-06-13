library(qualtRics)
library(tidyverse)

qualtrics_api_credentials(
  api_key = "lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw",
  base_url = "https://kcliop.qualtrics.com",
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
  # if (qid == "QID124964827") {
  #   browser()
  # }

  # Make sure length is at least one so 'rep' won't empty a variable
  sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
  choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

  type <- qjson$questionType$type

  questionText <- qjson$questionText

  # Change to map_chr??
  recode <- unlist(map(qjson$choices, "recode")) %>%
    rep(times = sub_q_len) %>%
    null_na()

  description <- unlist(map(qjson$choices, "choiceText")) %>%
    rep(times = sub_q_len) %>%
    null_na()

  questionSelector <- qjson$questionType$selector

  text <- unlist(map(qjson$subQuestions, "choiceText")) %>%
    rep(each = choice_len) %>%
    null_na()

  recode <- unlist(map(qjson$subQuestions, "recode")) %>%
    rep(each = choice_len) %>%
    null_na()

  questionSubSelector <- qjson$questionType$subSelector %>%
    null_na()

  if (type == "SBS") {
    col_lens <- map(qjson$columns, "choices") %>% map_dbl(length)

    questionText <- map(qjson$columns, "questionText") %>%
      map2(col_lens, ~ rep(.x, each = .y)) %>%
      unlist() %>%
      rep(times = sub_q_len)

    recode <- map(qjson$columns, "choices") %>%
      map(~ map_chr(.x, "recode")) %>%
      unlist() %>%
      rep(times = sub_q_len)

    description <- map(qjson$columns, "choices") %>%
      map(~ map_chr(.x, "description")) %>%
      unlist() %>%
      rep(times = sub_q_len)

    text <- unlist(map(qjson$subQuestions, "choiceText")) %>%
      rep(each = sum(col_lens)) %>%
      null_na()
  }

  tibble(
    qid, recode, description, text,
    questionText,
    type, questionSelector,
    questionSubSelector
  )
}) %>%
  bind_rows() %>%
  remove_format()

json_block <- mt$blocks %>%
  setNames(map_chr(., "description")) %>%
  map("elements") %>%
  map(~ unlist(map(.x, "questionId"))) %>%
  enframe(value = "qid", name = "block") %>%
  unnest(qid)

dict <- left_join(json, json_block)
