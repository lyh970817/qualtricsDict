extract_names <- function(dict) {
  unique(dict[c(colnames(dict)[1], "question")])
}

update_names <- function(dict, names) {
  # Some check first to see if is one to one

  # dict[[1]] <- recode(
  #   dict[["question"]],
  #   !!!setNames(names[[1]], unique(dict[["question"]]))
  # )
  dict[[1]] <- unique_expand(dict[["question"]], names[1])
  return(dict)
}
