devtools::load_all()

qualtrics_api_credentials(
  api_key = "lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw",
  base_url = "eu.qualtrics.com",
  install = F,
  overwrite = T
)
surveyID <- "SV_0DrSSOISyMOqN5r"
surveyID2 <- "SV_2l4GhidotLEBzYp"
surveyID2 <- "SV_5gXHZjBhmhorErP"

ramp_dict <- readr::read_csv("./cache/coping_dict.csv")

# How does the progress bar work?
dict <- dict_generate(surveyID2, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)
print(dict[["COVID_Baseline_Demographics"]], n = 100)
# load(file = "./dict.RData")
ramp_dict$itemLabel[ramp_dict$itemLabel == ramp_dict$questionText] <- NA

ramp_diff <- function(dict, ramp_dict, block1, block2) {
  dict <- dict[[block1]]

  ramp_dict <- ramp_dict[ramp_dict$surveyBlock == block2, c(1, 3, 2, 6, 4, 8, 9, 11)] %>%
    remove_format()

  names(ramp_dict) <- names(dict)

  # dict$item[is.na(dict$item)] <- dict$question[is.na(dict$item)]
  # dict$question <- dict$item
  # ramp_dict$question <- ramp_dict$item

  dict <- unite(dict, question, question, item,
    sep = " ",
    remove = FALSE, na.rm = T
  )


  ramp_dict <- unite(ramp_dict, question, question, item,
    sep = " ",
    remove = FALSE, na.rm = T
  )

  attr(dict, "survey_name") <- "COPING"
  attr(ramp_dict, "survey_name") <- "RAMP"
  dict_compare(dict, ramp_dict)
}


ramp_merge <- function(dict, ramp_dict, block1, block2, ramp_diff) {
  dict <- dict[[block1]]
  dict$item[is.na(dict$item)] <- dict$question[is.na(dict$item)]
  dict$question <- dict$item

  ramp_dict <- ramp_dict[ramp_dict$surveyBlock == block2, c(1, 3, 2, 6, 4, 8, 9, 11)]

  names(ramp_dict) <- names(dict)
  ramp_dict$question <- ramp_dict$item
  # dict <- unite(dict, question, question, item,
  #   sep = " ",
  #   remove = FALSE, na.rm = T
  # )
  # ramp_dict <- unite(ramp_dict, question, question, item,
  #   sep = " ",
  #   remove = FALSE, na.rm = T
  # )

  attr(dict, "survey_name") <- "COPING"
  attr(ramp_dict, "survey_name") <- "RAMP"
  diff <- dict_compare(dict, ramp_dict)
  dict_generate(surveyID2,
    newname = "easyname",
    block_pattern = "_([^_]+$)",
    survey_name = "COPING",
    reference_dict = ramp_dict,
    dict_diff = ramp_diff, block = block1,
    force_level = F
  )
}


# Merge should be based on label as well for multiple options, how?
d1 <- ramp_diff(dict, ramp_dict, "COVID_Baseline_Demographics")

d1 <- d1[c(-1, -2, -3), ]
m1 <- ramp_merge(dict, ramp_dict, "COVID_Baseline_Demographics", d1)
readr::write_csv(m1, "./cache/DEM_dict.csv")
dv1 <- dict_validate(m1)
readr::write_csv(dv1$errors$non_unique_names[[2]], "./cache/DEM_dict_validate.csv")


d2 <- ramp_diff(dict, ramp_dict, "COPING_MentalHealth", "COVID_Health_MHQ")
d2 <- d2[c(-1, -2), ]
m2 <- ramp_merge(dict, ramp_dict, "COVID_Health_MHQ", d2)
readr::write_csv(m2, "./cache/MHQ_dict.csv")
dv2 <- dict_validate(m2)
readr::write_csv(dv1$errors$non_unique_names[[2]], "./cache/MHQ_dict_validate.csv")


d3 <- ramp_diff(dict, ramp_dict, "COPING_Baseline_GAD7", "COVID_Baseline_GAD7")
d3 <- d3[-1, ]
m3 <- ramp_merge(dict, ramp_dict, "COPING_Baseline_GAD7", "COVID_Baseline_GAD7", d3)

dict_validate(m3)
gad <- get_survey_data(m3, unanswer_recode_multi = 0, unanswer_recode = -77)
saveRDS(gad, file = "gad_coping.rds")

d4 <- ramp_diff(dict, ramp_dict, "COPING_Baseline_PHQ9", "COVID_Baseline_PHQ9")
dict["COPING_Baseline_PHQ9"]
d4 <- d4[-1, ]
m4 <- ramp_merge(dict, ramp_dict, "COPING_Baseline_PHQ9", "COVID_Baseline_PHQ9", d4)
print(m4, n = 100)
phq <- get_survey_data(m4, unanswer_recode_multi = 0, unanswer_recode = -77)
dict_validate(m4)
saveRDS(phq, file = "phq_coping.rds")

d5 <- ramp_diff(dict, ramp_dict, "COVID_Baseline_OCI-R", "COVID_Baseline_OCI-R")
m5 <- ramp_merge(dict, ramp_dict, "COVID_Baseline_OCI-R", d4)
ocir <- get_survey_data(m4, unanswer_recode_multi = 0, unanswer_recode = -77)
saveRDS(ocir, file = "ocir_coping.rds")

d6 <- ramp_diff(dict, ramp_dict, "COVID_Measures_PCL6", "COVID_Measures_PCL6")
d6 <- d6[-7, ]
d6$name_reference
m6 <- ramp_merge(dict, ramp_dict, "COVID_Measures_PCL6", "COVID_Measures_PCL6", d6)
dict_validate(m6)
pcl <- get_survey_data(m6, unanswer_recode_multi = 0, unanswer_recode = -77)
saveRDS(pcl, file = "pcl_coping.rds")

# Should all have characters not NAs in label
ramp_dict[ramp_dict$surveyBlock == "COVID_Measures_PCL6", ]
dict[["COVID_Measures_PCL6"]]

ramp_dict[ramp_dict$surveyBlock == "COVID_Baseline_GAD7", ] %>% pull()
print(d3 %>% select(name, question, name_reference), n = 100)

names(dict)
unique(ramp_dict$surveyBlock)

dict %>%
  filter(grepl("QID124964827", qid)) %>%
  select(qid, easyname, question, level)

dict %>%
  filter(qid == "QID124934446.9") %>%
  select(question)

dict %>%
  filter(qid == "QID124931443.9") %>%
  select(question)
