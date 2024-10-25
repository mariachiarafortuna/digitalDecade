require(readxl)
require(here)
require(dplyr)
# require(tidyr)
# require(stringr)

mapping <- read_excel(here("inst/data/Mapping cluster e IPA.xlsx"),
                      sheet = "Mapping cluster vs IPA")
