# copy files after produced by projects
library(dplyr)
library(openxlsx)
library(purrr)

# copy base data
base_data <- "./processing/data/output/tp_data/"
base_files <- list.files(base_data)

share_base_folder <- "./datasets_share/base_datasets/"

file.copy(paste0(base_data, base_files), to = share_base_folder, overwrite = T)

# copy processed traffic data
stops_data_dir <- "./webpages_scattered/traffic_stops_ind/data/output/"

stops_nonshp <- c("cad_data.csv", grep("cad_data\\.", dir(stops_data_dir), value = T, invert = T)) %>%
  grep("\\.rds", ., value = T, invert = T)

stops_shp <- grep("cad_data\\.", dir(stops_data_dir), value = T)
stops_shp <- stops_shp[!"cad_data.csv" == stops_shp]

stops_processed_folder <- "./datasets_share/processed_stops_data/"

file.copy(paste0(stops_data_dir, stops_nonshp), to = stops_processed_folder,overwrite = T)

file.copy(paste0(stops_data_dir, stops_shp), to = paste0(stops_processed_folder, "cad_shapefile"), overwrite = T)

# copy lists of dfs
stops_list_dfs <- grep("rds", dir(stops_data_dir), value = T)

stops_dashboard_dir <- "./webpages_scattered/traffic_dashboard/data/stops_dfs_list.rds"

file.copy(paste0(stops_data_dir, stops_list_dfs), stops_dashboard_dir, overwrite = T)

arrests_rds <- "./webpages_scattered/tparrests/data/tp_police/all_arrests_final.rds"

arrests_list_rds <- "./webpages_scattered/tparrests/data/list_arrests.rds"

file.copy(c(arrests_rds, arrests_list_rds), "./webpages_scattered/arrests_dashboard/data/", overwrite = T)

# add intro page to webpages
data_dictionaries_dir <- "./datasets_share/data_dictionaries/"

data_dictionaries_filenames <- dir(data_dictionaries_dir)

share_base_files <- dir(share_base_folder)

base_data_arrests <- grep("arrests", share_base_files, value = T) %>%
  grep("xlsx", ., value = T)

arrests_dictionary <- read.xlsx("./datasets_share/data_dictionaries/data_dictionary_arrests.xlsx", skipEmptyRows = F)

add_dicts <- function(filelist, file_path, dictionary){
  walk(filelist, ~ {
    data <- openxlsx::read.xlsx(paste0(file_path, .x))

    wb <- openxlsx::createWorkbook()

    addWorksheet(wb = wb, "data")

    addWorksheet(wb = wb, sheetName = "dictionary")

    openxlsx::writeDataTable(wb = wb, sheet = "data", x = data)

    openxlsx::writeData(wb = wb, sheet = "dictionary", x = dictionary)

    print(paste0(file_path, .x))

    openxlsx::saveWorkbook(wb, file = paste0(file_path, .x), overwrite = T)
  })
}

add_dicts(base_data_arrests, share_base_folder, dictionary = arrests_dictionary)

traffic_violations_dictionary <- read.xlsx("./datasets_share/data_dictionaries/data_dictionary_trafficviolations.xlsx", skipEmptyRows = F)

# traffic violations base
add_dicts("traffic_data_all_excel.xlsx", share_base_folder, traffic_violations_dictionary)

# # cad base
# add_dicts("cad_data_excel.xlsx", share_base_folder, traffic_violations_dictionary)

share_processed_folder <- "./datasets_share/processed_stops_data/"

# cad processed
cad_dictionary <- read.xlsx("./datasets_share/data_dictionaries/data_dictionary_cad.xlsx", skipEmptyRows = F)

add_dicts("cad_data_excel.xlsx", share_base_folder, cad_dictionary)

add_dicts("cad_data_excel.xlsx", share_processed_folder, cad_dictionary)

# traffic processed
add_dicts("traffic_data_violations_excel.xlsx", share_processed_folder, traffic_violations_dictionary)

#stops processed
traffic_stops_dictionary <- read.xlsx("./datasets_share/data_dictionaries/data_dictionary_trafficstop.xlsx", skipEmptyRows = F)

add_dicts("traffic_data_stops_excel.xlsx", share_processed_folder, traffic_stops_dictionary)

# zip file
zip(zipfile = "./datasets_share.zip", files = "./datasets_share")

# copy word versions of reports
traffic_report <- "./webpages_scattered/traffic_stops_ind/index.docx"
arrests_report <- "./webpages_scattered/tparrests/tparrests.docx"

report_share <- "./report_share/"

file.copy(c(traffic_report, arrests_report), to = report_share, overwrite = T)

file.rename("./report_share/index.docx", "./report_share/tpstops.docx")

zip(zipfile = "./report_share.zip", files = "./report_share")
