#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import stringi
#' @import stringdist
#' @import slowraker
#' @import qualtRics
#' @import sjlabelled
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

remove_format <- function(data, skip) {
  chr_cols <- discard(colnames(data), ~ . %in% c(skip))
  mutate_at(
    data, vars(-question_name),
    ~ str_remove_all(., "(<[^>]+>|\\n)+") %>%
      str_remove_all("Selected Choice - ") %>%
      str_squish()
  ) %>%
    mutate_at(
      vars(chr_cols),
      ~ str_remove_all(., "\\(.*\\)") %>%
        str_squish()
    )
}

unique_expand <- function(x, ...) {
  # Suppose x = unique(x') and a one-to-one mapping between x and
  # unique(paste(...)),
  # expand x to the same length of ...
  # and preserve the mapping
  if (all(is.na(x))) {
    return(x)
  }
  y <- paste(...)
  y[y == ""] <- " "
  recode(y, !!!setNames(x, unique(y)))
}

# reference_make_unique <- function(x, expand, y) {
#   # Create a unique mapping between set x and all pairs (expand, y),
#   # where for each e_i in expand there is k_i x_i in x (so that we can
#   # unique expand)
#   stopifnot(length(expand) == length(y))

#   expand_x <- unique_expand(x, expand)
#   # bind_cols() outputs a message for not giving colnames
#   # expand not required in the second column?
#   expand_unique_x <- suppressMessages(bind_cols(expand_x, expand, y) %>%
#     unique() %>%
#     pull(1) %>%
#     make.unique())

#   return(expand_unique_x)
# }

survey_rename <- function(survey) {
  qid_cols_nosfx <- str_replace(colnames(survey), "(#[0-9])?_[0-9_]+", "")
  qid_cols_all <- make.unique(qid_cols_nosfx)
  colnames(survey) <- qid_cols_all

  return(survey)
}

get_newname <- function(dict) {
  if ("easyname" %in% colnames(dict)) {
    return("easyname")
  }
  if ("question_name" %in% colnames(dict)) {
    return("easyname")
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
  str_remove_all(paste(...), paste0("NA")) %>%
    str_squish()
}

or <- function(x) {
  if (length(x) > 1) {
    lgl <- do.call(`|`, x)
  }
  else {
    lgl <- unlist(x)
  }

  lgl[is.na(lgl)] <- FALSE
  return(lgl)
}

split_orderd <- function(x, f) {
  split(x, f = factor(f, level = unique(f)))
}
