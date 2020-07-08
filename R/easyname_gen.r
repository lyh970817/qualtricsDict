easyname_gen <- function(json, block_pattern, block_sep) {
  # Temporary
  json$item[is.na(json$item)] <- json$question[is.na(json$item)]
  ma_lgl <- json$type == "Multiple Categorical"
  json$item[ma_lgl] <- paste(json$label[ma_lgl])

  if (file.exists("./.keywords.RData")) {
    load("./.keywords.RData")
  }

  if (!file.exists("./.keywords.RData") || length(unique(json$item)) != length(keywords)) {
    message("Generating easy names...")
    keywords <- slowrake(str_remove_all(unique(json$item), "\\(.+\\)"),
      all_words = paste(json$item, collapse = ""), stop_pos = NULL
    )
  }

  # Refer to qualtrics package on how to cache the results
  save(keywords, file = "./.keywords.RData")

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
  })
  # %>%
  #   make.unique()

  # Specify regex pattern or a function?
  # perhaps still a function
  # and need to join columns of a matrix

  # block_single <-
  #   if (!is.null(block_pattern)) {
  #     str_match(unique(json$block), block_pattern)[, -1] %>%
  #       as_tibble() %>%
  #       unite(x, everything(),
  #         sep = block_sep,
  #         remove = T,
  #         na.rm = T
  #       ) %>%
  #       pull() %>%
  #       tolower() %>%
  #       make.unique()
  #   } else {
  #     NA
  #   }

  block_single <-
    if (!is.null(block_pattern)) {
      map_chr(unique(json$block), block_pattern) %>%
        make.unique()
    } else {
      NA
    }

  # t <- bind_cols(json$item, json$question)
  # print(t, n = 2500)

  # keywords_item_question_unique <- reference_make_unique(keywords_single, json$item, json$question)

  json$question_easy <- unique_expand(keywords_single, json$item)
  json$block_easy <- unique_expand(block_single, json$block)

  json <- json %>%
    unite(easyname, block_easy, question_easy,
      sep = ".", na.rm = T
    ) %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  # txt_qs <- grep("_TEXT", json$level)
  # no_txt_name <- grep("text", json$easyname[txt_qs], invert = T)
  # json$easyname[txt_qs][no_txt_name] <- paste(json$easyname[txt_qs][no_txt_name], "_text")

  duplicated_easynames <- which_not_onetoone(json[c("easyname", "qid")])[[1]]
  duplicated_easynames["easyname"] <- make.unique(duplicated_easynames[["easyname"]])
  all_easynames <- bind_rows(
    duplicated_easynames,
    json[!json$qid %in% duplicated_easynames$qid, c("easyname", "qid")]
  )
  json$easyname <- recode(json$qid, !!!setNames(all_easynames$easyname, all_easynames$qid))

  json$easyname <- str_remove_all(json$easyname, "[^0-9A-Za-z_\\.]")

  return(json)

  # There are some variables that should have easy names determined by label
}
