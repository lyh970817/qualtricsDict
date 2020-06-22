library(qualtRics)
library(tidyverse)

load(file = "../cache/survey.RData")
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
    ~ str_remove_all(., "(<[^>]+>|\\n)+") %>%
      str_remove_all("Selected Choice - ") %>%
      str_remove_all("\\(.*\\)") %>%
      str_squish()
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
  sub_selector <- qjson$questionType$subSelector

  # These do not appear in colnames
  if (!is.null(label)) {
    which_na <- label == "N/A"
    label <- label[!which_na]
    level <- level[!which_na]
  }

  has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))
  has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))

  add_text <- function(x, has_text) {
    if (is.null(x)) {
      return(x)
    }

    for (i in has_text) {
      x <- c(
        x[1:i],
        paste(x[i], sep = "_", "TEXT"),
        # This will produce NA, needs removal
        x[i + 1:length(x)]
      ) %>%
        discard(is.na)
      return(x)
    }
  }

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
    # These are continuous variables with some strange question index
    choice_len <- 1
    sub_q_len <- 1
    level <- NA
  }

  # new_qid <-
  #   if (type == "SBS") {
  #     paste(qid, sep = "#", seq_along(unique(question))) %>%
  #       map2(level_lens, rep) %>%
  #       unlist() %>%
  #       paste(sep = "_", rep(seq_along(unique(item)), each = sum(level_lens)))
  #   } else if (type == "TE" & selector == "FORM") {
  #     paste(qid, sep = "_", seq_along(unique(label)))
  #     # if (selector != "FORM") {
  #     #   paste(qid, sep = "_", "TEXT")
  #     # }
  #   }
  #   else if (type == "TE" & selector == "FORM") {
  #     paste(qid, sep = "_", seq_along(unique(label)))
  #   }
  #   else if (type == "TE" & selector != "FORM") {
  #     paste(qid, sep = "_", "TEXT")
  #   }
  #   else if (type == "Matrix") {
  #     paste(qid, sep = "_", seq_along(item)) %>%
  #       rep(each = choice_len)
  #   }
  #   else if (type == "Slider") {
  #     paste(qid, sep = "_", seq_along(item))
  #   }
  #   else if (type == "MC") {
  #     paste(qid, sep = "_", level)
  #   }
  #   else {
  #     qid
  #   }

  # if (qid == "QID124934741") {
  #   browser()
  # }

  tibble(
    # new_qid,
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

json_block <- mt$blocks %>%
  # set the names to block names so we can enframe
  setNames(map_chr(., "description")) %>%
  map("elements") %>%
  map(~ unlist(map(.x, "questionId"))) %>%
  enframe(value = "qid", name = "block") %>%
  unnest(qid)

json <- left_join(json, json_block) %>%
  select(qid, block, everything())

qid_cols <- grep("QID", colnames(survey), v = T)
qid_cols_nosfx <- str_replace(qnames, "(#[0-9])?_.+", "")
json_sfx <- json %>%
  split(.$qid) %>%
  imap(function(x, n) {
    c_names <- qid_cols[which(qid_cols_nosfx == n)]
    if (length(c_names) == 1) {
      # Note c_names can be zero as welcome messages are not exported
      # but are in json
      # If length 1 the question could be of TE type and with TEXT in
      # colname
      x["qid"] <- c_names
    }
    else if (length(c_names) > 1) {
      # Without subquestions TEXT is in label, in item if with subquestions
      has_text_lgl <-
        grepl("TEXT", x$label) |
          grepl("TEXT", x$item)

      non_text_qids <- c_names %>%
        grep("TEXT", ., value = T, invert = T) %>%
        rep(each = nrow(x[!has_text_lgl, "qid"]) / length(.))

      text_qids <- c_names %>%
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
  filter(qid %in% colnames(survey)) %>%
  unite(question, question, item, na.rm = T, sep = " ")


write_csv(json_sfx, "../cache/coping_dict_new.csv")

not_in <- json_sfx$qid[!json_sfx$qid %in% colnames(survey)]
not_in <- colnames(survey)[!colnames(survey) %in% json_sfx$qid]
not_in_d <- json %>% filter(qid %in% not_in)

map(not_in, ~ json_sfx %>%
  filter(qid == .x) %>%
  select(question))
grep("QID124931461", q_split$qid, v = T)


dict <- left_join(json, json_block)
dict <- dict %>%
  mutate()
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
