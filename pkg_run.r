library("googlesheets4")
devtools::load_all()



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
json <- read_csv("./cache/coping_dict.csv")

unique(json$surveyBlock)

phq <- GLAD_sheet("PHQ")[[1]]
phq <- json_recode(phq)
phq_dict_c <- subdict_generate(json, "COVID_Baseline_PHQ9")
phq_diff <- dict_compare(phq_dict_c, phq)
# check without dict diff
t <- dict_merge(phq_dict_c, phq, phq_diff)

print(reference_dict, n = 200)
a <- t %>% select(-itemLabel, -n_levels)
gad <- GLAD_sheet("GAD")[[1]]
gad_dict_c <- subdict_generate(json, "COVID_Baseline_GAD7")
gad_dict <- json_recode(gad)
gad_diff <- dict_compare(gad_dict_c, gad_dict)
t <- dict_merge(gad_dict_c, gad_dict, gad_diff)
# Check valueLabel matching if yes remove
# valueLabel should be the same/recodeLevel can be different
ocir <- subdict_generate(json, "COVID_Baseline_OCI-R")

pcl <- GLAD_sheet("PCL")[[1]]
pcl_dict <- json_recode(pcl)
pcl_dict_c <- subdict_generate(json, "COVID_Measures_PCL6")
pcl_dict_new <- readRDS("~/pcl.rds")
pcl_dict_new$QuestionID
pcl_dict_new <- full_join(pcl_dict_c, pcl_dict_new, by = c(recodeLevel = "level", itemLabel = "item", valueLabel = "label")) %>%
  select(-QuestionID, -type, -question, -block, -selector, -sub_selector, QuestionID = new_qid)
pcl_dict_new <- pcl_dict_new %>%
  mutate(itemLabel = ifelse(grepl("related_pandemic", easyVariableName),
    paste(itemLabel, "(related to pandemic?)"),
    itemLabel
  ))

pcl_diff <- dict_compare(pcl_dict_new, pcl_dict) %>%
  filter(!grepl("related_pandemic", name))

dict <- dict_generate(
  json = pcl_dict_new,
  survey_name = "coping",
  reference_dict = pcl_dict,
  dict_diff = pcl_diff,
  qid = TRUE
)

t <- dict_merge(pcl_dict_new, pcl_dict, pcl_diff)
print(t, n = 100)
pcl_diff$name
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
dict <- json %>% filter(surveyBlock == "COVID_Health_MHQ")

dict_generate(surveyID = surveyID)

mhd <- get_survey_dat(
  newname = "easyVariableName",
  dict = dict, api_key = api_key,
  surveyID = surveyID,
  datacenter = "eu",
  # limit = 100,
  unanswer_recode = -99,
  unanswer_recode_multi = 0
)

a <- column_map(surveyID)
print(a, n = 200)
c_names <- str_split(a$choice, "\\.") %>%
  map_chr(~ paste(.x[1], .x[3], sep = "_"))
c_names <- ifelse(is.na(a[["choice"]]), a[["qid"]], c_names)
names(c_names) <- NULL
c_names[!c_names %in% colnames(survey)]
colnames(survey)[!colnames(survey) %in% c_names]

pcl <- get_survey_dat(
  newname = "easyVariableName",
  dict = dict, api_key = api_key,
  surveyID = surveyID,
  datacenter = "eu",
  # limit = 100,
  unanswer_recode = -99,
  unanswer_recode_multi = 0
)

dict$QuestionID

map(dem, typeof)
dat <- check_names(json, newname = "easyVariableName")[[2]]
dat %>% filter(grepl("PCL", easyVariableName))
gad <- readRDS("./cache/gad.rds")
saveRDS(mhd, "./cache/mhd.rds")
saveRDS(pcl, "./cache/pcl.rds")
saveRDS(ocir, "./cache/ocir.rds")
saveRDS(phq, "./cache/phq.rds")
saveRDS(dem, "./cache/dem.rds")
