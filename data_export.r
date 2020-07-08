devtools::load_all()
library(googlesheets4)
library("qualtRics")
url <- "https://docs.google.com/spreadsheets/d/1Ag0jmI0j15bEgxIuxaKyAHVyXZ9ZPPInndVyMNdra8Q/edit#gid=0"

qualtrics_api_credentials(
  api_key = " lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw",
  base_url = "eu.qualtrics.com",
  install = F,
  overwrite = T
)

gladID <- "SV_6DMA4YLjmSiUmbj"
rampID <- "SV_0DrSSOISyMOqN5r"
copingID <- "SV_4PyAHbAxpdbyacl"
copingID2 <- "SV_2l4GhidotLEBzYp"
copingID3 <- "SV_5gXHZjBhmhorErP"

block_fun <- function(x) {
  tolower(str_match(x, "_([^_]+$)")[, -1])
}

glad_block_fun <- function(x) {
  case_when(
    x == "(AGoD) Agoraphobia" ~ "ARGO",
    x == "(AGoD) Panic Disorder" ~ "PANC",
    x == "(AGoD) Social phobia" ~ "SOPH",
    x == "(AGoD) Specific phobias" ~ "SEPH",
    x == "Demographics" ~ "dem",
    x == "General info/ study feedback" ~ "feed",
    x == "Standardised assessment of severity of personality disorder (SASPD)" ~ "SASPD",
    x == "UKBB MHQ Section A - General Mental Health" ~ "mhq",
    x == "UKBB MHQ Section G - Subjective Wellbeing" ~ "SWE",
    x == "UKBB MHQ: Section B - Depression" ~ "phq",
    x == "UKBB MHQ: Section C - Anxiety" ~ "gad",
    x == "UKBB MHQ: Section D - Alcohol" ~ "ACL",
    x == "UKBB MHQ: Section E - Psychotic Experiences" ~ "PSE",
    x == "UKBB MHQ: Section F - Trauma" ~ "pcl",
    x == "Work and Social Adjustment Scale" ~ "SAC"
  )
}

# How does the progress bar work?
load("./image.RData")
dict_glad <- dict_generate(surveyID = gladID, newname = "easyname", block_pattern = glad_block_fun, split_by_block = T)
dict_ramp <- dict_generate(rampID, newname = "easyname", block_pattern = block_fun, split_by_block = T)
dict_coping1 <- dict_generate(copingID, newname = "easyname", block_pattern = block_fun, split_by_block = T)
dict_coping2 <- dict_generate(copingID2, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)
dict_coping3 <- dict_generate(copingID2, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)

# GLAD
valid_dem_glad <- dict_validate(dict_glad[["Demographics"]])
dem_glad <- dict_glad[["Demographics"]]
# write_sheet(dem_glad, ss = url, sheet = "GLAD_DEM")

valid_dem_ramp <- dict_validate(dict_ramp[["COVID_Baseline_Demographics"]])
dem_ramp <- dict_ramp[["COVID_Baseline_Demographics"]]

valid_dem_coping1 <- dict_validate(dict_coping1[["COPING_Demographics"]])
# Which demographics?
dem_coping <- dict_coping1[["COPING_Demographics"]]

dem_diff_ramp <- dict_compare(dict = dem_ramp, reference_dict = dem_glad, field = c("item"))
dem_diff_coping <- dict_compare(dict = dem_coping, reference_dict = dem_glad, field = c("item"))

write_sheet(dem_glad, ss = url, sheet = "GLAD_DEM")
write_sheet(dem_ramp, ss = url, sheet = "RAMP_DEM")
write_sheet(dem_coping, ss = url, sheet = "COPING_DEM")

write_sheet(dem_diff_ramp, ss = url, sheet = "RAMP_GLAD_DEM")
write_sheet(dem_diff_coping, ss = url, sheet = "COPING_GLAD_DEM")


# MHQ
valid_mhq_glad <- dict_validate(dict_glad[["UKBB MHQ Section A - General Mental Health"]])
mhq_glad <- dict_glad[["UKBB MHQ Section A - General Mental Health"]]

valid_mhq_ramp <- dict_validate(dict_ramp[["COVID_Health_MHQ"]])
mhq_ramp <- dict_ramp[["COVID_Health_MHQ"]]

valid_mhq_coping <- dict_validate(dict_coping1[["COPING_MentalHealth"]])
mhq_coping <- dict_coping1[["COPING_MentalHealth"]]

mhq_diff_ramp <- dict_compare(dict = mhq_ramp, reference_dict = mhq_glad, field = c("item"))
mhq_diff_coping <- dict_compare(dict = mhq_coping, reference_dict = mhq_glad, field = c("item"))

write_sheet(mhq_glad, ss = url, sheet = "GLAD_MHQ")
write_sheet(mhq_ramp, ss = url, sheet = "RAMP_MHQ")
write_sheet(mhq_coping, ss = url, sheet = "COPING_MHQ")

write_sheet(mhq_diff_ramp, ss = url, sheet = "RAMP_GLAD_MHQ")
write_sheet(mhq_diff_coping, ss = url, sheet = "COPING_GLAD_MHQ")

# PCL
valid_pcl_glad <- dict_validate(dict_glad[["UKBB MHQ: Section F - Trauma"]])
pcl_glad <- dict_glad[["UKBB MHQ: Section F - Trauma"]]

valid_pcl_ramp <- dict_validate(dict_ramp[["COVID_Baseline_PTSD"]])
pcl_ramp <- dict_ramp[["COVID_Baseline_PTSD"]]

valid_pcl_coping <- dict_validate(dict_coping1[["COVID_Measures_PCL6"]])
pcl_coping <- dict_coping1[["COVID_Measures_PCL6"]]

pcl_diff_ramp <- dict_compare(dict = pcl_ramp, reference_dict = pcl_glad, field = c("item"))
pcl_diff_coping <- dict_compare(dict = pcl_coping, reference_dict = pcl_glad, field = c("item"))

write_sheet(pcl_glad, ss = url, sheet = "GLAD_PCL")
write_sheet(pcl_ramp, ss = url, sheet = "RAMP_PCL")
write_sheet(pcl_coping, ss = url, sheet = "COPING_PCL")

write_sheet(pcl_diff_ramp, ss = url, sheet = "RAMP_GLAD_PCL")
write_sheet(pcl_diff_coping, ss = url, sheet = "COPING_GLAD_PCL")

# OCIR
valid_ocir_ramp <- dict_validate(dict_ramp[["COVID_Baseline_OCI-R"]])
ocir_ramp <- dict_ramp[["COVID_Baseline_OCI-R"]]
ocir_ramp_dat <- get_survey_data(ocir_ramp, limit = 10, unanswer_recode_multi = 0, unanswer_recode = -77)

# GAD
gad_glad <- dict_glad[["UKBB MHQ: Section C - Anxiety"]]
valid_gad_glad <- dict_validate(gad_glad)

gad_ramp <- dict_ramp[["COVID_Baseline_GAD7"]]
valid_gad_ramp <- dict_validate(gad_ramp)

gad_coping <- dict_coping1[["COPING_Baseline_GAD7"]]
valid_gad_coping <- dict_validate(gad_coping)

gad_diff_ramp <- dict_compare(dict = gad_ramp, reference_dict = gad_glad, field = c("item"))
gad_diff_coping <- dict_compare(dict = gad_coping, reference_dict = gad_glad, field = c("item"))

write_sheet(gad_glad, ss = url, sheet = "GLAD_GAD")
write_sheet(gad_ramp, ss = url, sheet = "RAMP_GAD")
write_sheet(gad_coping, ss = url, sheet = "COPING_GAD")

write_sheet(gad_diff_ramp, ss = url, sheet = "RAMP_GLAD_GAD")
write_sheet(gad_diff_coping, ss = url, sheet = "COPING_GLAD_GAD")

# PHQ
phq_glad <- dict_glad[["UKBB MHQ: Section B - Depression"]]
valid_phq_glad <- dict_validate(phq_glad)

phq_ramp <- dict_ramp[["COVID_Baseline_PHQ9"]]
valid_phq_coping <- dict_validate(phq_ramp)

phq_coping <- dict_coping1[["COPING_Baseline_PHQ9"]]
valid_phq_coping <- dict_validate(phq_ramp)

phq_diff_ramp <- dict_compare(dict = phq_ramp, reference_dict = phq_glad, field = c("item"))
phq_diff_coping <- dict_compare(dict = phq_coping, reference_dict = phq_glad, field = c("item"))

write_sheet(phq_glad, ss = url, sheet = "GLAD_PHQ")
write_sheet(phq_ramp, ss = url, sheet = "RAMP_PHQ")
write_sheet(phq_coping, ss = url, sheet = "COPING_PHQ")

write_sheet(phq_diff_ramp, ss = url, sheet = "RAMP_GLAD_PHQ")
write_sheet(phq_diff_coping, ss = url, sheet = "COPING_GLAD_PHQ")
