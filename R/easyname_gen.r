easyname_gen <- function(json, block_pattern, block_sep) {
  json$item[is.na(json$item)] <- json$question[is.na(json$item)]

  message("Generating easy names...")
  keywords <- slowrake(str_remove_all(unique(json$item), "\\(.+\\)"),
    all_words = paste(json$item, collapse = ""), stop_pos = NULL
  )

  # Refer to qualtrics package on how to cache the results
  # save(keywords, file = "./keywords.RData")
  load("./keywords.RData")

  keywords_single <- imap_chr(keywords, function(x, i) {
    if (all(is.na(x))) {
      nm <- unique(json$item)[i]
    }
    else if (stri_count_words(unique(json$item)[i]) <= 7) {
      nm <- unique(json$item)[i]
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

  block_single <-
    if (!is.null(block_pattern)) {
      str_match(unique(json$block), block_pattern)[, -1] %>%
        as_tibble() %>%
        unite(x, everything(),
          sep = block_sep,
          remove = T,
          na.rm = T
        ) %>%
        pull() %>%
        tolower() %>%
        make.unique()
    } else {
      NA
    }

  # unique_expand should take a factor like split so we can unique at
  # item and block
  json$question_easy <- unique_expand(keywords_single, json$item)
  json$block_easy <- unique_expand(block_single, json$block)

  json <- json %>%
    unite(easyname, block_easy, question_easy,
      sep = ".", na.rm = T
    ) %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  return(json)

  # There are some variables that should have easy names determined by label
}
