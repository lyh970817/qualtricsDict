check_names <- function(dat, newname) {
  cols <- dat[c("QuestionID", newname)]
  which_not_onetoone(cols)
}


check_jsons <- function(split_jsons) {
  check_item <- function(dat, qid) {
    # if (qid == "QID124934471") {
    #   browser()
    # }
    cols <- dat[c("valueLabel", "recodeLevel")]
    json %>% filter(QuestionID == "QID124934471")
    dat$valueLabel
    # Here recode is sometimes "none" and will cause a warning
    col2_pos <- as.numeric(cols[[2]]) %>%
      subset(. >= 0)

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
          mistake = paste(which(has_mistake), collapse = "")
        ),
        cols
      )
    }
  }

  mistakes <- imap(split_jsons, check_item) %>%
    bind_rows()
  if (nrow(mistakes) > 0) {
    return(mistakes)
  }
}
