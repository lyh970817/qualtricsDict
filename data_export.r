devtools::load_all()
library(googlesheets4)
url <- "https://docs.google.com/spreadsheets/d/1Vp028XLxbcXeOdjeuI-3y-kVenKWDR4hW9iJ1tHpttI/edit#gid=935710032"
source("./surveyIDs.r")
# Read in the diff files from url

sheets <- sheet_names(url)
diff_sheets <- sheets[grepl("DIFF", sheets)]
diffs_dat <- map(diff_sheets, ~ read_sheet(url, sheet = .x)) %>%
  setNames(diff_sheets)

dict_gad_opt <- dict_edgiopt[["Module 2: Thoughts & behaviours"]]
diff <- dict_compare(dict_gad_opt, dict_glad[["UKBB MHQ: Section C - Anxiety"]])
diff[["name_reference"]]
gad_names_opt <- diff %>%
  filter(identical == TRUE & label_match == TRUE) %>%
  pull(name)

gad_names_opt <- diff %>%
  filter(identical & label_match) %>%
  pull(name)

dict_gad_opt %>%
  filter(dict_gad_opt$easyname %in% gad_names_opt) %>%
  select(question) %>%
  pull()

load(file = "~/Downloads/dic_intm2020-08-05.RData")
pcl_ramp
get_survey_data(
  dict = pcl_ramp,
  unanswer_recode_multi = 0,
  keys = "Login ID"
)

str(pcl_ramp)
rm("get_survey_data")

# Create diff_files from ramp and coping, because we're merging everything
# to GLAD
ramp_diff <- bind_rows(diffs_dat[grep("RAMP", names(diffs_dat))])
coping_diff <- bind_rows(diffs_dat[grep("COPING", names(diffs_dat))])
employment_coping_glad <- dict_coping_glad[["COVID_Baseline_Employment"]]
valid_employment_coping_glad <- dict_validate(dict = employment_coping_glad)
get_survey_data(
  dict = employment_coping_glad,
  unanswer_recode_multi = 0,
  limit = 100
)


block_fun <- function(x) {
  tolower(str_match(x, "_([^_]+$)")[, -1])
}

glad_block_fun <- function(x) {
  case_when(
    x == "(AGoD) Agoraphobia" ~ "ARGO",
    x == "(AGoD) Panic Disorder" ~ "PANC",
    x == "(AGoD) Social phobia" ~ "SOPH",
    x == "(AGoD) Specific phobias" ~ "SEPH",
    x == "Demographics" ~ "demographics",
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
save.image("./.RData")


dict_glad <- dict_generate(surveyID = gladID, newname = "easyname", block_pattern = glad_block_fun, split_by_block = T)
dict_glad <- dict_generate(surveyID = glad_ocir_ID, newname = "easyname", block_pattern = glad_block_fun, split_by_block = T)

dict_ramp <- dict_generate(rampID, newname = "easyname", block_pattern = block_fun, split_by_block = T)
dict_edgi <- dict_generate(edgiID, newname = "easyname", block_pattern = block_fun, split_by_block = T)
# Some strange qids starting with x11 that needs to be checked
dict_edgiopt <- dict_generate(edgi_optional_ID, newname = "easyname", block_pattern = block_fun, split_by_block = T)

dict_gad_opt <- dict_edgiopt[["Module 2: Thoughts & behaviours"]]
diff <- dict_compare(dict_gad_opt, dict_glad[["UKBB MHQ: Section C - Anxiety"]])
gad_names_opt <- diff %>%
  filter(identical & label_match) %>%
  pull(name)
dict_gad_opt %>% filter(dict_gad_opt$easyname %in% gad_names_opt)

dict_coping1 <- dict_generate(copingID, newname = "easyname", block_pattern = block_fun, split_by_block = T)
dict_coping2 <- dict_generate(copingID2, newname = "easyname", block_pattern = block_fun, split_by_block = T)
dict_coping3 <- dict_generate(coping_edgi_ID, newname = "easyname", block_pattern = block_fun, split_by_block = T, dict_diff = coping_diff)

dem_edgi <- dict_edgi[["Part 1 - Demographics and personal information"]]
dem_glad <- dict_glad[["Demographics"]]
rm(list = "dict_merge")
dem_diff_edgi_vs_glad <- dict_compare(
  dict = dem_edgi, # the dictionary to compare
  reference_dict = dem_glad, # the reference dictionary
  field = c("item") # comparison based on item
)

merged_dem1 <- dict_merge(
  dict = dem_edgi, # Dictionary to check
  dict_diff = dem_diff_edgi_vs_glad #
)


# GLAD
valid_dem_glad <- dict_validate(dict_glad[["Demographics"]])

dem_glad <- dict_glad[["Demographics"]]
# write_sheet(dem_glad, ss = url, sheet = "GLAD_DEM")

valid_dem_ramp <- dict_validate(dict_ramp[["COVID_Baseline_Demographics"]])
dem_ramp <- dict_ramp[["COVID_Baseline_Demographics"]]

valid_dem_coping1 <- dict_validate(dict_coping1[["COPING_Demographics"]])
# Which demographics?
dem_coping <- dict_coping1[["COVID_Baseline_Demographics"]]

dem_coping %>%
  filter(grepl("QID124991659", qid)) %>%
  select(1, 2, 3)

dem_diff_ramp <- dict_compare(dict = dem_ramp, reference_dict = dem_glad, field = c("item"))
# dem_diff_coping <- dict_compare(dict = dem_coping, reference_dict = dem_glad, field = c("item"))

merged_dem <- dict_merge(dem_ramp, dem_glad, diffs_dat[["RAMP_GLAD_DEM_DIFF"]])
merged_dem <- dict_merge(dem_coping, merged_dem, diffs_dat[["COPING_GLAD_DEM_DIFF"]])


write_sheet(dem_glad, ss = url, sheet = "GLAD_DEM")
write_sheet(dem_ramp, ss = url, sheet = "RAMP_DEM")
write_sheet(dem_coping, ss = url, sheet = "COPING_DEM")

write_sheet(dem_diff_ramp, ss = url, sheet = "RAMP_GLAD_DEM")
write_sheet(dem_diff_coping, ss = url, sheet = "COPING_GLAD_DEM")

write_sheet(merged_dem, ss = url, sheet = "MERGED_DEM")


# MHQ
valid_mhq_glad <- dict_validate(dict_glad[["UKBB MHQ Section A - General Mental Health"]])
mhq_glad <- dict_glad[["UKBB MHQ Section A - General Mental Health"]]

valid_mhq_ramp <- dict_validate(dict_ramp[["COVID_Health_MHQ"]])
mhq_ramp <- dict_ramp[["COVID_Health_MHQ"]]

valid_mhq_coping <- dict_validate(dict_coping1[["COPING_MentalHealth"]])
mhq_coping <- dict_coping1[["COPING_MentalHealth"]]

mhq_diff_ramp <- dict_compare(dict = mhq_ramp, reference_dict = mhq_glad, field = c("item"))
mhq_diff_coping <- dict_compare(dict = mhq_coping, reference_dict = mhq_glad, field = c("item"))

merged_mhq <- dict_merge(mhq_ramp, mhq_glad, diffs_dat[["RAMP_GLAD_MHQ_DIFF"]])
merged_mhq <- dict_merge(mhq_coping, merged_mhq, diffs_dat[["COPING_GLAD_MHQ_DIFF"]])

write_sheet(mhq_glad, ss = url, sheet = "GLAD_MHQ")
write_sheet(mhq_ramp, ss = url, sheet = "RAMP_MHQ")
write_sheet(mhq_coping, ss = url, sheet = "COPING_MHQ")

write_sheet(mhq_diff_ramp, ss = url, sheet = "RAMP_GLAD_MHQ")
write_sheet(mhq_diff_coping, ss = url, sheet = "COPING_GLAD_MHQ")

write_sheet(merged_mhq, ss = url, sheet = "MERGED_MHQ")

# PCL
valid_pcl_glad <- dict_validate(dict_glad[["UKBB MHQ: Section F - Trauma"]])
pcl_glad <- dict_glad[["UKBB MHQ: Section F - Trauma"]]

valid_pcl_ramp <- dict_validate(dict_ramp[["COVID_Baseline_PTSD"]])
pcl_ramp <- dict_ramp[["COVID_Screening_PTSD"]]

valid_pcl_coping <- dict_validate(dict_coping1[["COVID_Measures_PCL6"]])
pcl_coping <- dict_coping1[["COVID_Measures_PCL6"]]

pcl_diff_ramp <- dict_compare(dict = pcl_ramp, reference_dict = pcl_glad, field = c("item"))
pcl_diff_coping <- dict_compare(dict = pcl_coping, reference_dict = pcl_glad, field = c("item"))

merged_pcl <- dict_merge(pcl_ramp, pcl_glad, diffs_dat[["RAMP_GLAD_PCL_DIFF"]])
merged_pcl <- dict_merge(pcl_coping, merged_pcl, diffs_dat[["COPING_GLAD_PCL_DIFF"]])

write_sheet(pcl_glad, ss = url, sheet = "GLAD_PCL")
write_sheet(pcl_ramp, ss = url, sheet = "RAMP_PCL")
write_sheet(pcl_coping, ss = url, sheet = "COPING_PCL")

write_sheet(pcl_diff_ramp, ss = url, sheet = "RAMP_GLAD_PCL")
write_sheet(pcl_diff_coping, ss = url, sheet = "COPING_GLAD_PCL")

write_sheet(merged_pcl, ss = url, sheet = "MERGED_PCL")

# OCIR
valid_ocir_ramp <- dict_validate(dict_ramp[["COVID_Baseline_OCI-R"]])
ocir_ramp <- dict_ramp[["COVID_Baseline_OCI-R"]]

# GAD
gad_glad <- dict_glad[["UKBB MHQ: Section C - Anxiety"]]
valid_gad_glad <- dict_validate(gad_glad)

gad_ramp <- dict_ramp[["COVID_Baseline_GAD7"]]
valid_gad_ramp <- dict_validate(gad_ramp)

gad_coping <- dict_coping1[["COPING_Baseline_GAD7"]]
valid_gad_coping <- dict_validate(gad_coping)

gad_diff_ramp <- dict_compare(dict = gad_ramp, reference_dict = gad_glad, field = c("item"))
gad_diff_coping <- dict_compare(dict = gad_coping, reference_dict = gad_glad, field = c("item"))

merged_gad <- dict_merge(gad_ramp, gad_glad, diffs_dat[["RAMP_GLAD_GAD_DIFF"]])
merged_gad <- dict_merge(gad_coping, merged_gad, diffs_dat[["COPING_GLAD_GAD_DIFF"]])

write_sheet(merged_gad, ss = url, sheet = "MERGED_GAD")

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

merged_phq <- dict_merge(phq_ramp, phq_glad, diffs_dat[["RAMP_GLAD_PHQ_DIFF"]])
merged_phq <- dict_merge(phq_coping, merged_phq, diffs_dat[["COPING_GLAD_PHQ_DIFF"]])

write_sheet(merged_phq, ss = url, sheet = "MERGED_PHQ")

dem_coping_glad <- dict_coping2[["COPING_GLAD_Demographics"]]
mhq_coping_glad <- dict_coping2[["COPING_MentalHealth"]]
pcl_coping_glad <- dict_coping2[["COVID_Measures_PCL6"]]
gad_coping_glad <- dict_coping2[["COPING_Baseline_GAD7"]]
phq_coping_glad <- dict_coping2[["COPING_Baseline_PHQ9"]]

dem_coping_edgi <- dict_coping3[["COPING_GLAD_Demographics"]]
mhq_coping_edgi <- dict_coping3[["COPING_MentalHealth"]]
pcl_coping_edgi <- dict_coping3[["COVID_Measures_PCL6"]]
gad_coping_edgi <- dict_coping3[["COPING_Baseline_GAD7"]]
phq_coping_edgi <- dict_coping3[["COPING_Baseline_PHQ9"]]

saveRDS(get_survey_data(dem_glad, unanswer_recode_multi = 0), "~/Data/COPING/glad/dem_glad.rds")
saveRDS(get_survey_data(dem_ramp, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/ramp/dem_ramp.rds")
saveRDS(get_survey_data(dem_coping, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/coping_nbr/dem_coping_nbr.rds")
saveRDS(get_survey_data(dem_coping_glad, unanswer_recode_multi = 0), "~/Data/COPING/coping_glad/dem_coping_glad.rds")
saveRDS(get_survey_data(dem_coping_edgi, unanswer_recode_multi = 0), "~/Data/COPING/coping_edgi/dem_coping_edgi.rds")

saveRDS(get_survey_data(mhq_glad, unanswer_recode_multi = 0), "~/Data/COPING/glad/mhq_glad.rds")
saveRDS(get_survey_data(mhq_ramp, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/ramp/mhq_ramp.rds")
saveRDS(get_survey_data(mhq_coping, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/coping_nbr/mhq_coping_nbr.rds")
saveRDS(get_survey_data(mhq_coping_glad, unanswer_recode_multi = 0), "~/Data/COPING/coping_glad/mhq_coping_glad.rds")
saveRDS(get_survey_data(mhq_coping_edgi, unanswer_recode_multi = 0), "~/Data/COPING/coping_edgi/mhq_coping_edgi.rds")

saveRDS(get_survey_data(pcl_glad, unanswer_recode_multi = 0), "~/Data/COPING/glad/pcl_glad.rds")
saveRDS(get_survey_data(pcl_ramp, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/ramp/pcl_ramp.rds")
saveRDS(get_survey_data(pcl_coping, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/coping_nbr/pcl_coping_nbr.rds")
saveRDS(get_survey_data(pcl_coping_glad, unanswer_recode_multi = 0), "~/Data/COPING/coping_glad/pcl_coping_glad.rds")
saveRDS(get_survey_data(pcl_coping_edgi, unanswer_recode_multi = 0), "~/Data/COPING/coping_edgi/pcl_coping_edgi.rds")

saveRDS(get_survey_data(gad_glad, unanswer_recode_multi = 0), "~/Data/COPING/glad/gad_glad.rds")
saveRDS(get_survey_data(gad_ramp, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/ramp/gad_ramp.rds")
saveRDS(get_survey_data(gad_coping, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/coping_nbr/gad_coping_nbr.rds")
saveRDS(get_survey_data(gad_coping_glad, unanswer_recode_multi = 0), "~/Data/COPING/coping_glad/gad_coping_glad.rds")
saveRDS(get_survey_data(gad_coping_edgi, unanswer_recode_multi = 0), "~/Data/COPING/coping_edgi/gad_coping_edgi.rds")

saveRDS(get_survey_data(phq_glad, unanswer_recode_multi = 0), "~/Data/COPING/glad/phq_glad.rds")
saveRDS(get_survey_data(phq_ramp, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/ramp/phq_ramp.rds")
saveRDS(get_survey_data(phq_coping, unanswer_recode_multi = 0, keys = "Login ID"), "~/Data/COPING/coping_nbr/phq_coping_nbr.rds")
saveRDS(get_survey_data(phq_coping_glad, unanswer_recode_multi = 0), "~/Data/COPING/coping_glad/phq_coping_glad.rds")
saveRDS(get_survey_data(phq_coping_edgi, unanswer_recode_multi = 0), "~/Data/COPING/coping_edgi/phq_coping_edgi.rds")

saveRDS(get_survey_data(ocir_ramp, unanswer_recode_multi = 0), "~/Data/COPING/ramp/ocir_ramp.rds")

mhd_glad <- dict_glad[["UKBB MHQ Section A - General Mental Health"]]
mhd_edgi <- dict_edgi[["Part 3 - ED100K Screening questions - Section A"]]
mhd_edgi[c(48, 18), ]
dim(mhd_edgi)
valid_mhd_edgi <- dict_validate(dict = mhd_edgi)
# Compare with GLAD
mhd_diff_edgi_vs_glad <- dict_compare(dict = mhd_edgi, reference_dict = mhd_glad, field = c("item"))
mhd_diff_edgi_vs_glad[["name_reference"]]
# Upload the diff files to googlesheets and double check them
write_sheet(mhd_diff_edgi_vs_glad, ss = url, sheet = "EDGI_vs_GLAD_MHD_DIFF")
# Write sheet
write_sheet(mhd_edgi, ss = url, sheet = "EDGI_MHD")
saveRDS(
  object =
    get_survey_data(
      dict = mhd_edgi,
      unanswer_recode_multi = 0
    ),
  file = paste0("../data_raw/", date, "/edgi/mhd_edgi.rds")
)
