which_not_onetoone <- function(cols) {
  which_not_oneto <- function(cols, from, to) {
    cols %>%
      group_by(.data[[from]]) %>%
      filter(length(unique(.data[[to]])) != 1) %>%
      summarize(!!to := unique(.data[[to]]), .groups = "keep")
  }
  names_cols <- colnames(cols)
  map(
    names_cols,
    ~ which_not_oneto(cols, from = .x, to = setdiff(names_cols, .x))
  )
}

is_onetoone <- function(cols) {
  !any(map_dbl(which_not_onetoone(cols), nrow) > 0)
}


get_match <- function(matches) {
  list(
    which(!is.na(matches)),
    discard(matches, is.na)
  )
}

null_na <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}

remove_format <- function(dat) {
  mutate_if(
    dat, is.character,
    ~ str_remove_all(., "(<[^>]+>|\\n)+") %>%
      str_remove_all("Selected Choice - ") %>%
      str_remove_all("\\(.*\\)") %>%
      str_squish()
  )
}

survey_rename <- function(survey) {

  # Question mark for non-greedy match
  qid_pattern <- '\\{"ImportId":"(.+?)".+'
  qid_pattern_choice <- '\\{"ImportId":"(.+)","choiceId":"([-0-9]+)"'

  qid_rename <- str_match(colnames(survey), qid_pattern)[, 2]
  qid_rename_choice <- str_match(colnames(survey), qid_pattern_choice)
  qid_rename <- ifelse(is.na(qid_rename_choice[, 1]),
    qid_rename,
    paste(qid_rename_choice[, 2], qid_rename_choice[, 3], sep = "_")
  )

  return(setNames(survey, qid_rename))
}
