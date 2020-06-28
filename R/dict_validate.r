dict_validate <- function(dict) {
  message("Validating dictionary...")

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
    return(unique_pairs)
  }
  else {
    return(list(
      level_label_pairs = level_label_pairs,
      errors = error_list
    ))
  }
}
