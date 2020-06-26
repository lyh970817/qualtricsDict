easyname_gen <- function(json, block_pattern) {
  json$question[is.na(json$question)] <- json$question[is.na(json$question)]

  # keywords <- slowrake(str_remove_all(unique(json$question), "\\(.+\\)"),
  #   all_words = paste(json$question, collapse = ""), stop_pos = NULL
  # )

  # Refer to qualtrics package on how to cache the results
  # save(keywords, file = "./keywords.RData")
  load("./keywords.RData")

  keywords_single <- imap_chr(keywords, function(x, i) {
    if (all(is.na(x))) {
      nm <- unique(json$question)[i]
    }
    else if (stri_count_words(unique(json$question)[i]) <= 7) {
      nm <- unique(json$question)[i]
    }
    else {
      nm <- paste(x[[1]], collapse = " ") %>%
        str_split(" ") %>%
        unlist() %>%
        .[1:4] %>%
        discard(is.na) %>%
        paste(collapse = "_")
    }
    return(tolower(str_replace_all(nm, "\\s", "_")))
  }) %>%
    make.unique()

  # Specify regex pattern or a function?
  # perhaps still a function
  # and need to join columns of a matrix
  block_single <- str_match(unique(json$block), block_pattern)[, 2] %>%
    tolower() %>%
    make.unique()

  json$question_easy <- unique_expand(keywords_single, json$question)
  json$block_easy <- unique_expand(block_single, json$block)

  # Do we need to remove non-easy names here?
  json <- json %>%
    unite(easyname, block_easy, question_easy, sep = ".") %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  return(json)

  # There are some variables that should have easy names determined by label
}
