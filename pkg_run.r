library(googlesheets4)
library(qualtRics)
library(tidyverse)
library(stringdist)
library(gladfunctions)
source("./R/utils.r")
source("./R/checks.r")
source("./R/dict.r")
source("./R/get_survey_dat.r")

json_recode <- function(sheet) {
  dict <- sheet %>%
    select(
      easyVariableName = easyname,
      itemLabel = title,
      valueLabel = labels,
      recodeLevel = levels
    ) %>%
    mutate(surveyBlock = "GLAD") %>%
    filter(!is.na(easyVariableName) & !is.na(itemLabel))
  attr(dict, "survey_name") <- "glad"
  return(dict)
}

dict_generate(
  json = filter(
    json,
    surveyBlock == block
  ),
  survey_name = "coping",
  qid = TRUE
)

subdict_generate <- function(json, block) {
  dict_generate(
    json = filter(
      json,
      surveyBlock == block
    ),
    survey_name = "coping",
    qid = TRUE
  )
}

url <- "https://docs.google.com/spreadsheets/d/1IKrEwkkH52Yxf-Ltfik46YzN8n5EaNIP_LRc-Eung8A/edit?usp=sharing"
json <- read_sheet(url)

unique(json$surveyBlock)

phq <- GLAD_sheet("PHQ")[[1]]
phq <- json_recode(phq)
phq_dict_c <- subdict_generate(json, "COVID_Baseline_PHQ9")
phq_diff <- dict_compare(phq_dict_c, phq)
# check without dict diff
t <- dict_merge(phq_dict_c, phq, phq_diff)
print(t, n = 200)

gad <- GLAD_sheet("GAD")[[1]]
gad_dict_c <- subdict_generate(json, "COVID_Baseline_GAD7")
gad_dict <- json_recode(gad)
gad_diff <- dict_compare(gad_dict_c, gad_dict)
# Check valueLabel matching if yes remove
# valueLabel should be the same/recodeLevel can be different
ocir <- subdict_generate(json, "COVID_Baseline_OCI-R")

dict <- dict_generate(
  json = filter(
    json,
    surveyBlock == "COVID_Baseline_PHQ9"
  ),
  survey_name = "coping",
  reference_dict = phq,
  dict_diff = phq_diff,
  qid = TRUE
)

json <- json %>%
  filter(coping) %>%
  select(
    -contains("glad"),
    -coping
  ) %>%
  setNames(c("easyVariableName", "itemLabel", "recodeLevel", "QuestionID", "surveyBlock", "valueLabel")) %>%
  select(
    QuestionID, easyVariableName, surveyBlock,
    itemLabel, recodeLevel, valueLabel
  )

api_key <- "kqBI3SOA54Ik6r1QFnYhq6W0oYDPyhxp9q5wwPtX"
surveyID <- "SV_0DrSSOISyMOqN5r"
dem <- json %>% filter(surveyBlock == "COVID_Baseline_Demographics")
unique(json$surveyBlock)

phq <- get_survey_dat(
  newname = "easyVariableName",
  json = dict, api_key = api_key,
  surveyID = surveyID,
  datacenter = "eu",
  # limit = 100,
  unanswer_recode = -99,
  unanswer_recode_multi = 0
)

dem <- get_survey_dat(
  newname = "easyVariableName",
  json = dem, api_key = api_key,
  surveyID = surveyID,
  datacenter = "eu",
  # limit = 100,
  unanswer_recode = -99,
  unanswer_recode_multi = 0
)
map(dem, typeof)
dat <- check_names(json, newname = "easyVariableName")[[2]]
dat %>% filter(grepl("PCL", easyVariableName))
gad <- readRDS("./cache/gad.rds")
saveRDS(gad, "./cache/gad.rds")
saveRDS(ocir, "./cache/ocir.rds")
saveRDS(phq, "./cache/phq.rds")
saveRDS(dem, "./cache/dem.rds")
