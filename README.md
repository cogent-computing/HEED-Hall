# HEED: Hall

## Data repositories

The raw data for Hall system is available at the project Data Portal (https://heed-data-portal.coventry.ac.uk/sensor/153) under Systems and Sensors: Nyabiheke Hall. The portal is a one-stop-shop for the raw data collected through project's surveys, sensor and energy monitoring systems and photo reportages. The registration on the portal is free and easy.

The system data used for performance analysis for the paper titled 'Performance analysis of standalone solar systems in refugee camps in Rwanda' is deposited on Zenodo https://doi.org/10.5281/zenodo.3949777

## Analysis scripts

The below scripts are used for analysis for the paper titled 'Performance analysis of standalone solar systems in refugee camps in Rwanda'

* Hall_stitching_30Jun.R - to stitch the raw data together for selected variables
* Hall_preprocessing_1Jul.R - to preprocess raw data and evaluate yield; test different imputation techniques
* Hall_imputation_1Jul.R - to impute missing values and correct anomalies using a rule based approach
* Hall_analysis_2Jul.R - to analyse and plot corrected data
* Hall_predicted.R - to analyse and plot predicted data
* Hall_data_upload.R - to prepare files for upload on Zenodo
* Hall_HEED_dataPortal_upload.R - to prepare files for upload on the data portal
