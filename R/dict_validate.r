dict_validate <- function(dict) {
  # WHY IS THIS SLOW? RESUME
  message("Validating dictionary...")
  newname <- get_newname(dict)

  split_dict <- split(dict, factor(dict$qid))

  level_label_pairs <- split_dict %>%
    map(select, label, level) %>%
    enframe(value = "pair") %>%
    group_by(pair) %>%
    summarize(qid = list(name), .groups = "drop")

  error_list <- list()
  non_unique_names <- check_names(dict, newname)
  if (any(map_dbl(non_unique_names, nrow) > 0)) {
    message("There are non-unique name mappings.")
    error_list$non_unique_names <- non_unique_names
  }

  mistake_dict <- check_json(split_dict)

  if (nrow(mistake_dict) > 0) {
    message("There are items with potential incorrect recodings.")
    error_list$mistake_dict <- mistake_dict
  }

  if (length(error_list) == 0) {
    return(level_label_pairs)
  }
  else {
    return(list(
      level_label_pairs = level_label_pairs,
      errors = error_list
    ))
  }
}

check_names <- function(dict, newname) {
  cols <- dict[c("qid", newname)]
  which_not_onetoone(cols)
}

check_item <- function(dat, qid) {
  newname <- get_newname(dat)
  item_name <- dat[dat$qid == qid, newname]
  cols <- dat[c("label", "level")]

  # Here recode is sometimes "none" and will cause a warning
  col2_pos <- suppressWarnings(
    as.numeric(cols[[2]]) %>%
      subset(. >= 0)
  )

  has_mistake <- c(
    # Check correspondence
    !is_onetoone(cols),
    # Check constant step == 1
    !(all(diff(sort(col2_pos)) == 1) | length(diff(col2_pos)) == 0),
    # Check duplication
    any(duplicated(cols[[1]])),
    any(duplicated(cols[[2]]))
  )

  if (any(has_mistake)) {
    bind_cols(
      tibble(
        qid = qid,
        item_name,
        mistake = paste(which(has_mistake), collapse = "")
      ),
      cols
    )
  }
}

check_json <- function(split_jsons) {
  mistakes <- imap(split_jsons, check_item) %>%
    bind_rows()
  if (nrow(mistakes) > 0) {
    return(mistakes)
  } else {
    return(tibble())
  }
}
