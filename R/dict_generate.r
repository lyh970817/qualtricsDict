dict_generate <- function(surveyID,
                          survey_name = NULL,
                          newname = "question_name",
                          block_pattern = NULL,
                          block_sep = "_",
                          split_by_block = FALSE,
                          block = NULL,
                          dict_diff = NULL,
                          import_id = TRUE) {
  easyname_gen <- ifelse(
    newname == "easyname",
    TRUE, FALSE
  )

  dict <- recode_json(surveyID,
    import_id = import_id,
    easyname_gen = easyname_gen,
    block_pattern = block_pattern,
    block_sep = block_sep
  )

  if (!is.null(block)) {
    dict <- dict[dict$block == block, ]
  }

  dict <- dict[c(
    "qid", newname, "block", "question",
    "item", "level", "label", "type"
  )]

  if (!is.null(survey_name)) {
    # Is it possible for a qualtrics survey to have no name in metadata?
    attr(dict, "survey_name") <- survey_name
  }

  if (!is.null(dict_diff)) {
    dict[[newname]] <- recode(
      dict[[newname]],
      !!!setNames(
        make.unique(dict_diff[["name_reference"]]),
        dict_diff[["name"]]
      )
    )
  }
  # Temporary
  dict$item[dict$item == dict$question] <- NA

  attr(dict, "surveyID") <- surveyID

  if (!import_id) {
    dict$qid <- NULL
  }

  if (split_by_block) {
    dict <- split(dict, dict$block)
  }

  return(dict)
}

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


  json$question_easy <- unique_expand(keywords_single, json$item)
  json$block_easy <- unique_expand(block_single, json$block)

  json <- json %>%
    unite(easyname, block_easy, question_easy,
      sep = ".", na.rm = T
    ) %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  # Add txt to text questions
  txt_qs <- grep("_TEXT", json$qid)
  json$easyname[txt_qs] <- paste(json$easyname[txt_qs], ".txt")

  duplicated_easynames <- which_not_onetoone(json[c("easyname", "qid")])[[1]]
  duplicated_easynames["easyname"] <- make.unique(duplicated_easynames[["easyname"]])
  not_duplicated_easynames <-
    json[!json$qid %in% duplicated_easynames$qid, c("easyname", "qid")]

  all_easynames <- bind_rows(
    duplicated_easynames,
    not_duplicated_easynames
  )
  json$easyname <- recode(json$qid, !!!setNames(all_easynames$easyname, all_easynames$qid))

  json$easyname <- str_remove_all(json$easyname, "[^0-9A-Za-z_\\.]")

  return(json)

  # There are some variables that should have easy names determined by label
}
