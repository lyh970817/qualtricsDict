#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import stringi
#' @import stringdist
#' @import slowraker
#' @importFrom magrittr %>%

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

# survey_rename <- function(survey) {

#   # Question mark for non-greedy match
#   qid_pattern <- '\\{"ImportId":"(.+?)".+'
#   qid_pattern_choice <- '\\{"ImportId":"(.+)","choiceId":"([-0-9]+)"'

#   qid_rename <- str_match(colnames(survey), qid_pattern)[, 2]
#   qid_rename_choice <- str_match(colnames(survey), qid_pattern_choice)
#   qid_rename <- ifelse(is.na(qid_rename_choice[, 1]),
#     qid_rename,
#     paste(qid_rename_choice[, 2], qid_rename_choice[, 3], sep = "_")
#   )

#   return(setNames(survey, qid_rename))
# }

unique_expand <- function(x, ...) {
  # Suppose a one-to-one mapping between unique(x') and unique(...),
  # expand x = unique(x') to the same length of ...
  # and preserve the mapping
  if (all(is.na(x))) {
    return(x)
  }
  y <- paste(...)
  recode(y, !!!setNames(x, unique(y)))
}

expand_make_unique <- function(x, expand, y) {
  stopifnot(length(expand) == length(y))

  expand_x <- unique_expand(x, expand)
  # bind_cols() outputs a message for not giving colnames
  expand_unique_x <- suppressMessages(bind_cols(expand_x, expand, y) %>%
    unique() %>%
    pull(1) %>%
    make.unique())

  return(expand_unique_x)
}

survey_rename <- function(survey) {
  qid_cols_nosfx <- str_replace(colnames(survey), "(#[0-9])?_[0-9_]+", "")
  qid_cols_all <- make.unique(qid_cols_nosfx)
  colnames(survey) <- qid_cols_all

  return(survey)
}

get_newname <- function(dict) {
  if ("qid" %in% colnames(dict)) {
    colnames(dict)[2]
  }
  else {
    colnames(dict)[1]
  }
}

match_all <- function(x, y) {
  unlist(map(y, ~ which(x == .x)))
}

reorder <- function(data, col) {
  fct <- factor(data[[col]], level = unique(data[[col]]))
  bind_rows(split(data, fct))
}

paste_narm <- function(...) {
  sep <- list(...)$sep %>%
    ifelse(is.null(.), " ", .)
  str_remove_all(paste(...), paste0(sep, "NA"))
}
