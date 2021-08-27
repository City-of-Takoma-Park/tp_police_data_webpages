# Description

The following folders are included in this zip-file:

- base-datasets: Lightly-processed csv and excel datasets based on internal data. Data dictionaries are attached as a sheet in the Excel versions.
    - all_arrests_final: A dataset of all traffic and criminal arrests between 2015 and 2020.
    - crime_arrests_processed: A dataset of criminal arrests between 2015 and 2020.
    - traffic_arrests_processed: A dataset of traffic arrests between 2015 and 2020.
    - cad_data: A dataset of Computer-Aided Dispatch call responses between 2018 and 2020.
    - traffic_data_all: A dataset of traffic violations issued in stops by the Takoma Park police department between 2015 and 2020. Each row is a violation, not a stop (stops can have multiple violations).
    - TPPD Calls for Service Data 2018-2020: Police Department data on calls for service from 2018 to 2020.
- processed_stops_data: Processed stops datasets used in constructing the traffic stops webpage. Data dictionaries are added as a sheet to the Excel version of each dataset.
    - cad_data: A dataset of Computer-Aided Dispatch call responses between 2018 and 2020.
    - traffic_data_stops: A dataset of traffic stops made by the Takoma Park police department between 2015 and 2020. Each row is a traffic stop, and there are fields on the number/type of violations associated with each stop.
    - traffic_data_violations: A dataset of traffic violations issued by the Takoma Park police department between 2015 and 2020. Each row is a violation, not a stop (stops can have multiple violations).
    - cad_shapefile: A shapefile of the CAD dataset, with a geometry field identifying the location of each stop. 
- data_dictionaries: A folder of data dictionaries for each dataset. 
    - dataDictionary_internalpolicedata: The data dictionary used for the Police Department's internal arrest dataset. Contains detailed descriptions of arrest and traffic offenses used in the public datasets, and the statutory basis for traffic violations. 
    - Other data dictionaries are associated with the datasets described above.