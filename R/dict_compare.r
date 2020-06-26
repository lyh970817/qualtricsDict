dict_compare <- function(dict, reference_dict) {
  newname <- colnames(dict)[1]
  newname_ref <- colnames(reference_dict)[1]

  # Not every question will have item???
  question_ref <- reference_dict[["question"]]
  question <- dict[["question"]]
  question_fuzzy <- ifelse(qt %in% qtr, NA, qt)

  match_is <- match(question, question_ref)
  # Perhaps take Q+I and then I (filled by Q) only and then take the
  # union
  amatch_is <- amatch(question_fuzzy, question_ref, maxDist = 1000)

  question_fuzzy_is <- get_match(amatch_is)[[1]]
  question_ref_fuzzy <- get_match(amatch_is)[[2]]

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
      question_ref_fuzzy,
      question_ref_is
    )
  ]

  labels_ref <- map(nms_ref, ~ reference_dict %>%
    filter(.data[[newname_ref]] == .x) %>%
    select(label) %>%
    unlist())

  label_match <- map2_lgl(labels, labels_ref, ~ identical(.x, .y))

  return(
    tibble(
      name = dict[[newname]][c(question_fuzzy_is, question_is)],
      question = qt[c(question_fuzzy_is, question_is)],
      n_levels = map_dbl(labels, length),
      name_reference = reference_dict[[newname]][
        c(question_ref_fuzzy, question_ref_is)
      ],
      question_reference = qtr[c(question_ref_fuzzy, question_ref_is)],
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
  )
}

## Or do we just get a matrix and order the similarities?