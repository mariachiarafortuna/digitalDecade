require(readxl)
require(here)
require(dplyr)
# require(tidyr)
# require(stringr)


#--- Anno 2023
nat23_res <- read_excel(here("data/6_eGovernment_Benchmark_2023__Final_Results.xlsx"),
                      sheet = "3a. Nat. Services - Results", skip = 6, na = ".") %>%
  filter(Country == "IT")

nat23_data <- read_excel(here("data/6_eGovernment_Benchmark_2023__Final_Results.xlsx"),
                        sheet = "3b. Nat. Services - Data", skip = 5, na = ".") %>%
  filter(Country == "IT")

cb23_res <- read_excel(here("data/6_eGovernment_Benchmark_2023__Final_Results.xlsx"),
                       sheet = "4a. CB Services - Results", skip = 6, na = ".") %>%
  filter(Country == "IT")

cb23_data <- read_excel(here("data/6_eGovernment_Benchmark_2023__Final_Results.xlsx"),
                        sheet = "4b. CB Services - Data", skip = 6, na = ".") %>%
  filter(Country == "IT")

#--- Anno 2024
# Struttura identica a 2023, cambia solo il file

nat24_res <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
                        sheet = "3a. Nat. Services - Results", skip = 6, na = ".") %>%
  filter(Country == "IT")

nat24_data <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
                         sheet = "3b. Nat. Services - Data", skip = 5, na = ".") %>%
  filter(Country == "IT")

cb24_res <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
                        sheet = "4a. CB Services - Results", skip = 6, na = ".") %>%
  filter(Country == "IT")

cb24_data <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
                         sheet = "4b. CB Services - Data", skip = 6, na = ".") %>%
  filter(Country == "IT")

overall_eb_scores <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
                                sheet = "1. Scores 2024", skip = 5, na = ".") %>%
  filter(Country == "IT")

# # Irrilevante per traiettoria nazionale, probabilmente utile per non scored indicators
# nat24_portal <- read_excel(here("data/6_eGovernment_Benchmark_2024__Final_Results.xlsx"),
#                          sheet = "5. Nat. Portals - Data", skip = 5) %>%
#   filter(Country == "IT")


###### FOCUS ONLINE AVAILABILITY


