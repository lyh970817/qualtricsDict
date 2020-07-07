devtools::load_all()

qualtrics_api_credentials(
  api_key = "",
  base_url = "eu.qualtrics.com",
  install = F,
  overwrite = T
)

# How does the progress bar work?
dict_glad <- dict_generate(gladID, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)
dict_ramp <- dict_generate(rampID, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)
dict_coping1 <- dict_generate(copingID, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)
dict_coping2 <- dict_generate(copingID2, newname = "easyname", block_pattern = "_([^_]+$)", split_by_block = T)

valid_dem_ramp <- dict_validate(dict_ramp[["COVID_Baseline_Demographics"]])
dem_ramp <- dict_ramp[["COVID_Baseline_Demographics"]]

valid_dem_coping1 <- dict_validate(dict_coping1[["COVID_Baseline_Demographics"]])
dem_coping1 <- dict_coping1[["COVID_Baseline_Demographics"]]
dict_diff_dem <- dict_compare(dem_coping1, dem_ramp)

dict_merge(dem_coping1, dem_ramp, dict_diff_dem)
