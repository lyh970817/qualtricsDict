dict_compare <- function(dict, reference_dict, field = "item") {
  newname <- get_newname(dict)
  newname_ref <- get_newname(reference_dict)

  # Not every question will have item???
  question_ref <- do.call(paste_narm, as.list(reference_dict[field]))
  question_ref[question_ref == ""] <- reference_dict[["question"]][question_ref == ""]
  # ma_lgl_ref <-
  #   or(map(reference_dict %>% select(contains("type")), ~ .x == "Multiple Categorical"))
  # question_ref[ma_lgl_ref] <- paste(question_ref[ma_lgl_ref], reference_dict$label[ma_lgl_ref])

  question <- do.call(paste_narm, as.list(dict[field]))
  question[question == ""] <- dict[["question"]][question == ""]
  # ma_lgl <- dict$type == "Multiple Categorical"
  # question[ma_lgl] <- paste(question[ma_lgl], dict$label[ma_lgl])

  question_fuzzy <- ifelse(question %in% question_ref, NA, question)
  question_ref_fuzzy <- ifelse(question_ref %in% question, NA, question_ref)

  match_is <- match(question, question_ref)
  # Perhaps take Q+I and then I (filled by Q) only and then take the
  # union
  amatch_is <-
    if (!all(is.na(question_fuzzy)) && !all(is.na(question_ref_fuzzy))) {
      amatch(question_fuzzy, question_ref_fuzzy, maxDist = 1000)
    }
    else {
      NA
    }

  amatch_is[is.na(question_fuzzy)] <- NA
  question_fuzzy_is <- get_match(amatch_is)[[1]]
  question_ref_fuzzy_is <- get_match(amatch_is)[[2]]

  question_is <- get_match(match_is)[[1]]
  question_ref_is <- get_match(match_is)[[2]]

  nms <- dict[[newname]][
    c(
      question_fuzzy_is, question_is
    )
  ]

  labels <- map(nms, ~ dict %>%
    filter(.data[[newname]] == .x) %>%
    select(label) %>%
    unlist())

  nms_ref <- reference_dict[[newname_ref]][
    c(
      question_ref_fuzzy_is,
      question_ref_is
    )
  ]

  labels_ref <- map(nms_ref, ~ reference_dict %>%
    filter(.data[[newname_ref]] == .x) %>%
    select(label) %>%
    unlist())

  label_match <- map2_lgl(labels, labels_ref, ~ identical(.x, .y))

  if (all(is.na(amatch_is)) & all(is.na(match_is))) {
    tibble()
  }
  else {
    tibble(
      name = dict[[newname]][c(question_fuzzy_is, question_is)],
      question = question[c(question_fuzzy_is, question_is)],
      n_levels = map_dbl(labels, length),
      name_reference = reference_dict[[newname_ref]][
        c(question_ref_fuzzy_is, question_ref_is)
      ],
      question_reference = question_ref[c(question_ref_fuzzy_is, question_ref_is)],
      n_levels_ref = map_dbl(labels_ref, length),
      identical = c(
        rep(FALSE, times = length(question_fuzzy_is)),
        rep(TRUE, times = length(question_is))
      ),
      label_match = label_match
    ) %>%
      # Do we still need this?
      .[!duplicated(.), ] %>%
      na.omit()
  }
}

## Or do we just get a matrix and order the similarities?
