library(googlesheets4)
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
    filter(!is.na(easyVariableName))
  attr(dict, "survey_name") <- "glad"
  return(dict)
}

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

gad <- GLAD_sheet("GAD")[[1]]
phq <- GLAD_sheet("PHQ")[[1]]

gad_dict_c <- subdict_generate(json, "COVID_Baseline_GAD7")

gad_dict <- json_recode(gad)

dict_diff <- dict_compare(gad_dict_c, gad_dict)
dict_diff <- dict_compare(phq_dict_c, phq_dict)
t <- dict_merge(gad_dict_c, gad_dict, dict_diff)
t <- dict_merge(phq_dict_c, phq_dict, dict_diff)

api_key <- "lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw"
surveyID <- "SV_4PyAHbAxpdbyacl"
get_survey_dat(
  newname = "easyVariableName",
  json = json, api_key = api_key,
  surveyID = surveyID,
  datacenter = "eu",
  limit = 100
)
