library(dplyr)
library(tidyverse)
# library(conflicted)
library(plotly)
library(readxl)
library(rlang)
library(tidycensus)
library(spdep)
library(sf)
library(leaflet)

# define directories to read data from and output to
content_dir <- "./processing/data/"

output_dir <- "./processing/data/output/"

data_path <- function(..., dir = content_dir){
  paste0(dir, ...)
}

# read in data
traffic_data_all <- read_excel(path = "./processing/data/source/tp_police/trafficData_Public.xlsx",
                               sheet = "Violations",
                               col_names = TRUE,
                               .name_repair = "universal") %>%
  rename_all(tolower) %>%
  rename(outcome = type) %>%
  mutate(age = as.integer(floor(age))) %>%
  select(-c(officer, officer.id))

# write
write.csv(traffic_data_all, file = data_path("tp_data/traffic_data_all.csv", dir = output_dir))

# write to project that uses
write.csv(traffic_data_all, file = data_path("tp_data/traffic_data_all.csv", dir = "./webpages_scattered/traffic_stops_ind/data/"))

openxlsx::write.xlsx(traffic_data_all, "./processing/data/output/tp_data/traffic_data_all_excel.xlsx", asTable = T, keepNA = T)


######## cad data
cad_data <- read_excel(path = "./processing/data/source/tp_police/cadData_COMPLETE.xlsx",
           sheet = "Full Data",
           col_names = TRUE,
           .name_repair = "universal") %>%
  rename_all(tolower) %>%
  select(-officer)

# identify duplicate cad entries (representing multiple responding officers)
cad_data <- cad_data %>%
  group_by(event.no.) %>%
  mutate(freq = n())

cad_dups <- cad_data %>%
  filter(freq > 1)

# confirm that explanation for pullover is same
missing_cr_report <- cad_data %>%
  filter(cr.report == "No") %>%
  pull(event.no.) %>%
  unique() %>%
  length()

cad_data <- cad_data %>%
  select(-cr.report.number)

# write cad data
write.csv(cad_data, data_path("tp_data/cad_data.csv", dir = output_dir))

write.csv(cad_data, file = data_path("tp_data/cad_data.csv", dir = "./webpages_scattered/traffic_stops_ind/data/"))

openxlsx::write.xlsx(cad_data, "./processing/data/output/tp_data/cad_data_excel.xlsx", asTable = T, keepNA = T)

