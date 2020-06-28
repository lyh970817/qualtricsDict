devtools::load_all()

qualtrics_api_credentials(
  api_key = "lvajnhOtUPt2PMf1taRfmwKOOsYOzSAfNdZbEzOw",
  base_url = "eu.qualtrics.com",
  install = F,
  overwrite = T
)
surveyID <- "SV_0DrSSOISyMOqN5r"

# How does the progress bar work?
dict <- dict_generate(surveyID, newname = "easyname", block_pattern = "_([^_]+$)")
