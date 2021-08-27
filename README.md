# tp_police_data_webpages

Code/data used to produce webpages and dashboards for Takoma Park's release of policing data, linked here: https://takomaparkmd.gov/government/police/46842-2/.

## r-processing

The scripts used to build the webpages and dashboards were run in this order and do the following:

### R Project: tp_police_data_webpages
1) processing/r/processing01_trafficdata: Light processing of internal-traffic data for visualization in webpage/dashboard. Outputs datasets to processing/data/output folder and webpages_scattered/traffic_stops_ind/data/tp_data folder:
- cad_data: computer-aided dispatch data
- traffic_data_all: traffic-stops data (each row a violation for a stop, not a stop)

2) processing/r/processing02_internaldata: Light processing of internal arrests datasets for visualization in webpage/dashboard, and processing of ACS data for population-shares analysis. Outputs datasets to processing/data/output folder and webpages_scattered/tparrests/data/tp_police folders:
- all_arrests_final: traffic and criminal arrests in one file, each row an arrest.
- crime_arrests_processed: criminal arrests.
- traffic_arrests_processed: arrests in traffic stops.
- acs_race_gender_place_processed: acs data processed for Takoma Park.

### R Project: webpages_scattered/traffic_stops_ind/traffic_stops_ind

3) index: produces traffic stops webpage. Uncomment word and comment out html_output in YAML to output word copy. Also outputs to data/output folder:
- cad_data: processed CAD data, in shapefile and non-shapefile form
- traffic_data_stops: processed traffic stops data, with each row one stop
- traffic_data_violations: processed traffic stops data, with each row one violation. Violations exceed stops because one stop could have multiple violations
- stops_dfs_list.rds: list of summary datasets used in traffic stops webpage

### R Project: webpages_scattered/tparrests/tparrests

4) tparrests: produces arrests webpage. Uncomment word and comment out html_output in YAML to output word copy. Also outputs to data folder:
- list_arrests: list of summary datasets used in arrests webpage

### R Project: tp_police_data_webpages

5) processing/r/processing03_finalize: Copies datasets output by webpages_scattered/traffic_stops_ind/index.r and webpages_scattered/tparrests/tparrests.r to datasets_share directory, word-version reports to report_share directory, and zips both. Also copies webpages_scattered/tparrests/list_arrests.rds to webpages_scattered/arrests_dashboard/data, and webpages_scattered/traffic_stops_ind/data/output/stops_dfs_list.rds to webpages_scattered/traffic_dashboard/data/stops_dfs_list.rds

### R Project: webpages_scattered/traffic_dashboard/traffic_dashboard

6) app: Produces Shiny dashboard for traffic-stops data

### R Project: webpages_scattered/arrests_dashboard/arrests_dashboard

7) app: Produces Shiny dashboard for traffic-stops data.

## Folder descriptions

datasets_share - datasets to share on the city webpage, including data dictionaries

report_share - word versions of each webpage

processing - code to process internal police arrest/traffic stops/computer-assisted dispatch data

webpages_scattered - code to produce two RMarkdown HTML documents and two dashboards visualizing arrest/stops data
