library(dplyr)
library(tidyverse)
library(readxl)

library(tpfuncts)
library(arrprocess)
library(acsprocess)

# # read in each dataset
crimepublic <- readxl::read_excel("./processing/data/source/tp_police/crimeData_public.xlsx",
                                  sheet = "Complete",
                                  .name_repair = "universal") %>%
  rename_all(tolower) %>%
  mutate(overall_type = "Crime") %>%
  select(-c(officer, officer.id))


trafficpublic <- readxl::read_excel("./processing/data/source/tp_police/trafficStopArrest_public.xlsx",
                                    sheet = "Complete",
                                    .name_repair = "universal")%>%
  rename_all(tolower) %>%
  mutate(overall_type = "Traffic") %>%
  select(-c(officer, officer.id))

# function to confirm year-by-year = each dataset
year_vector <- seq(2015, 2020)


crime_year <- map_dfr(year_vector, ~ readxl::read_excel("./processing/data/source/tp_police/crimeData_public.xlsx",
                                                        sheet = as.character(.x),
                                                        .name_repair = "universal")) %>%
  rename_all(tolower) %>%
  mutate(overall_type = "Crime") %>%
  select(-c(officer, officer.id))


traffic_year <- map_dfr(year_vector, ~ readxl::read_excel("./processing/data/source/tp_police/trafficStopArrest_public.xlsx",
                                                          sheet = as.character(.x),
                                                          .name_repair = "universal")) %>%
  rename_all(tolower) %>%
  mutate(overall_type = "Traffic") %>%
  select(-c(officer, officer.id))

all(traffic_year %>% arrange(report., ._id) == trafficpublic %>% arrange(report., ._id))
all(crime_year %>% arrange(report., ._id) == crimepublic %>% arrange(report., ._id))

all_arrests <- rbind(crimepublic, trafficpublic)

# check if ward matches city
ward_check <- all_arrests %>%
  group_by(a.city, a.ward) %>%
  summarise(freq = n())

tp_ooc <- all_arrests %>%
  filter(grepl("Takoma Park", a.city) & a.ward == "OOC")

tp_ward_check <- all_arrests %>%
  filter(grepl("Takoma Park", a.city)) %>%
  group_by() %>%
  mutate(total = n()) %>%
  group_by(a.ward) %>%
  summarize(freq = n(),
         pct = pct_round(freq, total)) %>%
  distinct()

tp_county_check <- all_arrests %>%
  select(a.county, a.city) %>%
  distinct() %>%
  group_by(a.city) %>%
  mutate(freq = n())

correct_countes <- data.frame(a.city = c("Hyattsville",
                                         "Manassas"),
                              a.county = c("Prince George's",
                                           "OOS"))

tp_wrong_counties <- tp_county_check %>%
  filter(freq > 1) %>%
  anti_join(correct_countes)

# identify number of wrong counties coded
tp_wrong_counties_rows <- tp_wrong_counties %>%
  left_join(all_arrests) %>%
  select(-freq)

tp_state_check <- all_arrests %>%
  select(a.state, a.county) %>%
  distinct %>%
  group_by(a.county) %>%
  mutate(freq = n())

# unique values
map(all_arrests, ~ unique(.x) %>% sort)

# check if county matches city
all_arrests %>%
  group_by(a.ward) %>%
  summarize(freq = n())

unique_offenses <- all_arrests %>%
  pull(offense) %>%
  unique

summary(all_arrests %>%
          mutate(across(where(is.character), .fns = ~ as.factor(.x))))

# arrest recode
arrest_recode <- function(df){
  # browser()
  df %>%
    mutate(type_desc = case_when(type == "APP" ~ "Application for charges",
                                 type == "CA" ~ "Arrest",
                                 type == "CRC" ~ "Criminal citation",
                                 type == "SVC" ~ "Warrant-service arrest",
                                 type == "JVA" ~ "Juvenile arrest",
                                 type == "JVR" ~ "Juvenile referral",
                                 type == "CVC" ~ "Civil citation"))
}

process <- function(df){
  df_ids <- 1:nrow(all_arrests)

  df %>%
    rename_all(tolower) %>%
    arrest_recode() %>%
    mutate(unique_id = df_ids) %>%
    rename(a.gender = a.sex) %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           a.state = toupper(a.state),
           a.gender = case_when(a.gender == "Male" ~ "Man",
                                a.gender == "Female" ~ "Woman"),
           a.ward = ifelse(a.ward == "OOS", "OOC", a.ward),
           # recode city as unincorporated if ward in unincorporated area
           a.city = case_when(a.city == "Glendale" ~ "Glenn Dale",
                              a.city == "Takoma Park" & a.ward == "OOC" ~ "TP unincorporated",
                              TRUE ~ a.city),
           a.city = case_when(a.city == "TP unincorporated" & a.county == "Montgomery" ~ "TP unincorporated (MC)",
                              a.city == "TP unincorporated" & a.county == "Prince George's" ~ "TP unincorporated (PG)",
                              TRUE ~ a.city),
           # a.city = ifelse(a.state == "DC", "DC", a.city),
           # remove decimals from age
           a.age = as.integer(floor(a.age)),
           a.county = case_when(a.state == "DC" ~ "DC",
                                # recode county as virginia if county;s in vriginia
                                a.state == "VA" ~ "VA",
                                # recode county as takoma park if in takoma park
                                grepl("Takoma", a.city) ~ "Takoma Park (MC)",
                                TRUE ~ a.county),
           age_range = age_lookup(a.age, traffic = F)) %>%
    left_join(correct_countes %>%
                rename(correct_county = a.county),
              by = "a.city") %>%
    # fix instances where cities have wrong county associated with them
    mutate(a.county = ifelse(!is.na(correct_county), correct_county, a.county)) %>%
    select(-correct_county)
}

all_arrests_processed <- all_arrests %>%
  process()

# confirm county fix worked
county_fix <- all_arrests_processed %>%
  ungroup %>%
  select(a.county, a.city) %>%
  distinct %>%
  group_by(a.city) %>%
  mutate(freq = n())

as.data.frame(all_arrests_processed)

# prepare before write
all_arrests_final <- all_arrests_processed %>%
  select(unique_id, report., date, year, month, overall_type, category, type, type_desc, offense, i.ward, a.ward, a.city, a.county, a.state, a.race, a.gender, a.age, age_range, initiated)

traffic_processed <- all_arrests_final %>%
  filter(overall_type == "Traffic")

crime_processed <- all_arrests_final %>%
  filter(overall_type == "Crime")

all_arrests_final <- all_arrests_final %>%
  filter(overall_type %in% c("Traffic", "Crime"))

write.csv(all_arrests_final, "./processing/data/output/tp_data/all_arrests_final.csv")
write.csv(traffic_processed, "./processing/data/output/tp_data/traffic_arrests_processed.csv")
write.csv(crime_processed, "./processing/data/output/tp_data/crime_arrests_processed.csv")

openxlsx::write.xlsx(all_arrests_final, "./processing/data/output/tp_data/all_arrests_final_excel.xlsx", asTable = T, keepNA = T)
openxlsx::write.xlsx(traffic_processed, "./processing/data/output/tp_data/traffic_arrests_processed_excel.xlsx", asTable = T, keepNA = T)
openxlsx::write.xlsx(crime_processed, "./processing/data/output/tp_data/crime_arrests_processed_excel.xlsx", asTable = T, keepNA = T)


# save to projects that use these datasets
saveRDS(all_arrests_final, "./webpages_scattered/tparrests/data/tp_police/all_arrests_final.rds")
saveRDS(traffic_processed, "./webpages_scattered/tparrests/data/tp_police/traffic_processed.rds")
saveRDS(crime_processed, "./webpages_scattered/tparrests/data/tp_police/crime_processed.rds")

# save to arrests dashboard
saveRDS(all_arrests_final, "./webpages_scattered/arrests_dashboard/data/all_arrests_final.rds")


### process acs data for pop-share comparison
# read in age data for takoma park and montgomery county
variables <- tidycensus::load_variables("2019", "acs5", cache = TRUE)

race_sex_by_age_vars <- variables %>%
  filter(grepl("B01001[A-Z]", name)) %>%
  pull(name)

acs_age_race_sex_place <- tidycensus::get_acs("place", variables = race_sex_by_age_vars, cache_table = TRUE, year = 2019, state = "Maryland")

acs_race_gender_place <- acs_age_race_sex_place %>%
  rename_all(tolower) %>%
  filter(grepl("Takoma", name))

acs_race_gender_place_processed <- acs_race_gender_place %>%
  left_join(variables, by = c("variable" = "name")) %>%
  acsprocess::race_pull() %>%
  filter(race != "^white alone$") %>%
  separate_label(c(NA, NA, "gender", "age")) %>%
  total_col_add(total_cols = list("race_tot" = "gender", "race_gender_tot" = "age"), join_col = c("race", "gender"), est_col = "estimate") %>%
  select(-c(geoid, name, variable))

acs_gender_total <- acs_race_gender_place_processed %>%
  filter(!grepl("hispanic", race)) %>%
  group_by() %>%
  mutate(pop_total = sum(estimate)) %>%
  group_by(gender) %>%
  mutate(gender_total = sum(estimate)) %>%
  group_by(age) %>%
  mutate(age_total = sum(estimate)) %>%
  group_by(gender, age, age_total, gender_total, pop_total) %>%
  summarise(gender_age_total = sum(estimate)) %>%
  distinct

acs_race_gender_place_processed <- acs_race_gender_place_processed %>%
  left_join(acs_gender_total) %>%
  mutate(gender = ifelse(grepl("Female", gender), "Woman", "Man"))

write_rds(acs_race_gender_place_processed, "./webpages_scattered/tparrests/data/acs/acs_race_gender_place_processed.rds")

