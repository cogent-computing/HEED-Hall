#******************************************************************************************#
# This is the script to prepare data for upload on Zenodo                                  #
# Author: K Bhargava                                                                       #
# Last updated on: 17th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/Zenodo"
#******************************************************************************************#

#******************************************************************************************#
# Read all raw data adn add month, date and time 
cpe <- read_csv(here(filepath,"CPE_all_jun19_to_may20.csv"), col_names = TRUE)
cpe <- as.data.frame(cpe)
cpe <- cpe[,1:4]
cpe <- distinct(cpe)
cpe <- spread(cpe, variable, value)
cpe <- cpe %>% mutate(date = date(timestamp)) 
cpe <- cpe[order(cpe$id, cpe$timestamp),]
cpe <- cpe[cpe$date>="2019-07-20" & cpe$date<="2020-03-31",]
cpe <- cpe[,-6] #remove date
colnames(cpe) <- c("Timestamp (Africa/Kigali)", "ID", "LED1_P (W)", "LED2_P (W)", "LED3_P (W)")

sockets <- read_csv(here(filepath,"sockets_all_jul19_to_may20.csv"), col_names = TRUE) 
sockets <- as.data.frame(sockets)
sockets <- sockets[,1:4]
sockets <- distinct(sockets)
sockets <- sockets[-288861,]
sockets <- spread(sockets, variable, value)
sockets <- sockets %>% mutate(date = date(timestamp)) 
sockets <- sockets[order(sockets$id, sockets$timestamp),]
sockets <- sockets[sockets$date>="2019-07-20" & sockets$date<="2020-03-31",]
sockets <- sockets[,-c(3,5)] #Remove AC day energy session values and date
colnames(sockets) <- c("Timestamp (Africa/Kigali)", "ID", "vRELAY1_LVL (W)")

system <- read_csv(here(filepath,"systemData_jun19_to_mar20.csv"), col_names = TRUE)  
system <- as.data.frame(system)
system <- system[,1:9]
system <- distinct(system)
system <- system %>% mutate(date = date(timestamp)) 
system <- system[order(system$timestamp),]
system <- system[system$date>="2019-07-20" & system$date<="2020-03-31",]
system <- system[,-c(2,10)] #Remove date
colnames(system) <- c("Timestamp (Africa/Kigali)", "Battery Monitor State of Charge (%)",
                      "Battery Monitor Discharged Energy (kWh)", "Battery Monitor Charged Energy (kWh)",
                      "Solar Charger Battery Power (W)", "Solar Charger PV Power (W)", 
                      "System Overview AC Consumption (W)", "System Overview Battery Power (W)")
#******************************************************************************************#

#******************************************************************************************#
# Save data for Hall
output_directory <- "Data/Zenodo/Hall"

# Save system data
write.csv(system, file=here(output_directory,"System_data.csv"), row.names=FALSE)

# Save CPE data - split for each CPE
cpe_sub <- cpe[grepl("Hall CPE1", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE1_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE2", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE2_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE3", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE3_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE4", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE4_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE5", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE5_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE6", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE6_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE7", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE7_data.csv"), row.names=FALSE)

# Save sockets data - split into different sockets
sockets_sub <- sockets[grepl("Hall S1", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Hall_S1_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S2", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Hall_S2_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S3", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Hall_S3_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S4", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Hall_S4_data.csv"), row.names=FALSE)
