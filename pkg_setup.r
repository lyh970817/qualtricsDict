library(devtools)
library(roxygen2)

imports <- c(
  "dplyr", "tibble", "purrr", "stringr", "stringi", "magrittr",
  "qualtRics", "slowraker", "R.utils"
)

for (i in imports) {
  use_package(i)
}

devtools::document()
devtools::load_all()
