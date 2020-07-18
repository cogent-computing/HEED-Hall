#**********************************************************************#
# Script for preparing raw data for upload on data portal              #
# Authors: K Bhargava                                                  #
# Last updated on: 17th July, 2020                                     #
#**********************************************************************#

#**********************************************************************#
# Import libraries for manipulating time series and plotting
library(tidyverse)
library(lubridate)
library(here)
#**********************************************************************#

#**********************************************************************#
# Define MACROS
NUM_CPE = 7
NUM_SOCKETS = 4
#**********************************************************************#

#**********************************************************************#
# Set path for output files
output_directory <- "Data/Data portal"
#**********************************************************************#

#**********************************************************************#
# Read in all CPE files
cpe <- data.frame()
for(k in 1:NUM_CPE) {
  if(k==1) {
    filepath <- "Data/Hall CPE1"
  } else if(k==2) {
    filepath <- "Data/Hall CPE2"
  } else if(k==3) {
    filepath <- "Data/Hall CPE3"
  } else if(k==4) {
    filepath <- "Data/Hall CPE4"
  } else if(k==5) {
    filepath <- "Data/Hall CPE5 (outdoor lights)"
  } else if(k==6) {
    filepath <- "Data/Hall CPE6"
  } else if(k==7) {
    filepath <- "Data/Hall CPE7"
  } 
  file_list <- list.files(here(filepath))
  file_list <- file_list[file.size(here(filepath,file_list))>0]
  
  # Read all files - add ID and bind all data
  df <- map_dfr(file_list, ~ read_csv(here(filepath,.x), col_names = FALSE, col_types = "dcc") %>%
                  mutate(id = substr(filepath,6,nchar(filepath)), 
                         X1 = as.double(X1), X2=as.character(X2), X3=as.character(X3)))
  cpe <- rbind(cpe,df)
}
cpe <- as.data.frame(cpe)
cpe <- cpe[,1:4]
colnames(cpe) <- c("timestamp", "variable", "value","id")
# Convert timestamp to Africa/Kigali time zone (GMT+2)
cpe <- cpe %>% mutate(timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2), 
                      date = date(timestamp))
cpe <- cpe[cpe$date>="2019-07-19" & cpe$date<="2020-03-31",]
cpe <- cpe[complete.cases(cpe),]
cpe <- distinct(cpe)
cpe <- cpe[,-5] #remove date
cpe <- cpe[order(cpe$id, cpe$timestamp),]
cpe <- cpe[!is.na(cpe$id),]
colnames(cpe) <- c("Timestamp (Africa/Kigali)", "Variable", "Value", "ID")
cpe <- cpe[,c(1,4,2,3)] #re-arrange columns

# Save CPE data - split for each CPE
cpe_sub <- cpe[grepl("Hall CPE1", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE1_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE2", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE2_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE3", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE3_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE4", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE4_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE5", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE5_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE6", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE6_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Hall CPE7", cpe$ID, fixed=TRUE), ]
cpe_sub <- cpe_sub[,-2]
write.csv(cpe_sub, file=here(output_directory,"Hall_CPE7_data.csv"), row.names=FALSE)
#**********************************************************************#

#**********************************************************************#
# Read all socket files
sockets <- data.frame()
for(k in 1:NUM_SOCKETS) {
  if(k==1) {
    filepath <- "Data/Hall S1"
  } else if(k==2) {
    filepath <- "Data/Hall S2"
  } else if(k==3) {
    filepath <- "Data/Hall S3"
  } else if(k==4) {
    filepath <- "Data/Hall S4"
  } 
  file_list <- list.files(here(filepath))
  file_list <- file_list[file.size(here(filepath,file_list))>0]
  
  # Read all files - add ID and bind all data
  df <- map_dfr(file_list, ~ read_csv(here(filepath,.x), col_names = FALSE) %>%
                  mutate(id = substr(filepath,6,nchar(filepath)), 
                         X1 = as.double(X1), X2=as.character(X2), X3=as.character(X3)))
  sockets <- rbind(sockets,df)
}
sockets <- as.data.frame(sockets)
sockets <- sockets[,1:4]
colnames(sockets) <- c("timestamp", "variable", "value","id")
# Convert timestamp to Africa/Kigali time zone (GMT+2)
sockets <- sockets %>% mutate(timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2), 
                              date = date(timestamp))
sockets <- sockets[sockets$date>="2019-07-19" & sockets$date<="2020-03-31",]
sockets <- sockets[complete.cases(sockets),]
sockets <- distinct(sockets)
sockets <- sockets[,-5] #remove date
sockets <- sockets[order(sockets$id, sockets$timestamp),]
sockets <- sockets[!is.na(sockets$id),]
colnames(sockets) <- c("Timestamp (Africa/Kigali)", "Variable", "Value", "ID")
sockets <- sockets[,c(1,4,2,3)] #re-arrange columns

# Save sockets data - split into different sockets
sockets_sub <- sockets[grepl("Hall S1", sockets$ID, fixed=TRUE), ]
sockets_sub <- sockets_sub[,-2]
write.csv(sockets_sub, file=here(output_directory,"Hall_S1_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S2", sockets$ID, fixed=TRUE), ]
sockets_sub <- sockets_sub[,-2]
write.csv(sockets_sub, file=here(output_directory,"Hall_S2_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S3", sockets$ID, fixed=TRUE), ]
sockets_sub <- sockets_sub[,-2]
write.csv(sockets_sub, file=here(output_directory,"Hall_S3_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Hall S4", sockets$ID, fixed=TRUE), ]
sockets_sub <- sockets_sub[,-2]
write.csv(sockets_sub, file=here(output_directory,"Hall_S4_data.csv"), row.names=FALSE)
#**********************************************************************#