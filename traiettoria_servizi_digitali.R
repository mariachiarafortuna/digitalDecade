require(readxl)
require(here)
require(dplyr)
# require(tidyr)
require(stringr)

source("src/digitalDecade_funs.R")

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

oa_24 <- nat24_data[,c(1,3:15)] %>%
  arrange(`Life event`, `Service`) %>%
  mutate(A1_info = case_when(str_to_title(`A1: information available online?`) == "Yes" ~ 1, TRUE ~ 0),
         A2_service = case_when(str_to_title(`A2: service available online?`) == "Yes" ~ 1, TRUE ~ 0),
         A3_portal = case_when(str_to_title(`A3: available through portal?`) == "Yes" ~ 1, TRUE ~ 0))

oa_23 <- nat23_data[,c(1,3:15)] %>%
  arrange(`Life event`, `Service`) %>%
  mutate(A1_info = case_when(`A1: information available online?` == "Yes" ~ 1, TRUE ~ 0),
         A2_service = case_when(`A2: service available online?` == "Yes" ~ 1, TRUE ~ 0),
         A3_portal = case_when(`A3: available through portal?` == "Yes" ~ 1, TRUE ~ 0))


#### STATS

# Stats servizi e life events

summary_nat_services <- oa_24 %>%
  group_by(`Life event`, `Service`) %>%
  summarise(Portali = n(),
            `Informazione disponibile` = sum(A1_info)/n()*100,
            `Servizio disponibile` = sum(A2_service)/n()*100,
            `Presenza nel portale` = sum(A3_portal)/n()*100)

summary_nat_life_events <- oa_24 %>%
  group_by(`Life event`) %>%
  summarise(`Portali esaminati` = n(),
            `Informazione disponibile` = round(sum(A1_info)/n()*100,2),
            `Servizio disponibile` = round(sum(A2_service)/n()*100,2),
            `Presenza nel portale` = round(sum(A3_portal)/n()*100,2))

# Stats tipologia attore
summary_nat_provider_24 <- oa_24 %>%
  group_by(`Service Provider`) %>%
  summarise(`Servizi esaminati` = n(),
            `Informazione disponibile` = round(sum(A1_info)/n()*100,2),
            `Servizio disponibile` = round(sum(A2_service)/n()*100,2),
            `Presenza nel portale` = round(sum(A3_portal)/n()*100,2))


# Check campionamento
campionamento24 <- oa_24 %>% select(provider24 = `Service Provider`, servizio24 = `Service`) %>%
  arrange(provider24, servizio24)
campionamento23 <- oa_23 %>% select(provider23 = `Service Provider`, servizio23 = `Service`) %>%
  arrange(provider23, servizio23)

campionamento <- cbind(campionamento24, campionamento23) %>%
  mutate(check_provider = (provider24 == provider23),
         check_servizio = (servizio24 == servizio23))
sum(campionamento$check_provider)
sum(campionamento$check_servizio)

# nessun campionamento




#### DATA CLEANING

oa_24_clean <- oa_24 %>%
  rowwise() %>%
  mutate(Service_Provider = traslate_service_providers(`Service Provider`)) %>%
  ungroup() %>%
  mutate(Score = case_when(
      A1_info + A2_service + A3_portal == 3 ~ 100,
      A1_info + A2_service == 2 ~ 75,
      A1_info + A3_portal == 2 ~ 50,
      A1_info == 1 ~ 25,
      A1_info + A2_service + A3_portal == 0 ~ 0,
      TRUE ~ NA_real_)) %>%
  select(ID, Year = `Year data collection`, Service_Provider, Life_event = `Life event`, Service, Url,
        Score, A1_info, A2_service, A3_portal,
         everything())

summary_service_provider <- oa_24_clean %>%
  group_by(Service_Provider) %>%
  summarise(
    `Servizi esaminati` = n(),
    `Informazione disponibile` = round(sum(A1_info)/n()*100,2),
    `Servizio disponibile` = round(sum(A2_service)/n()*100,2),
    `Presenza nel portale` = round(sum(A3_portal)/n()*100,2)) %>%
  mutate(Target_azioni =
           case_when((`Informazione disponibile`+ `Servizio disponibile` + `Presenza nel portale`) == 300 ~ 0,
                     TRUE ~ 1)) %>%
  arrange(desc(Target_azioni))
