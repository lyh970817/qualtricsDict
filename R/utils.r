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

match_name_recode <- function(n, match_name) {
  if (length(n) == 1) {
    return(NA)
  }
  if (!is.null(match_name)) {
    value <- seq(max(match_name))
    value[!value %in% match_name] <-
      sort(value[!value %in% names(match_name)])
    value[match_name] <- as.numeric(names(match_name))
    return(value[n])
  } else {
    return(n)
  }
}

# match_name_recode(c(1, 2, 3), c("1" = 2))
