#**********************************************************************#
# Script for stitching Hall data for paper 5 and 6                     #
# Authors: K Bhargava                                                  #
# Last updated on: 30th June, 2020                                     #
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
MONTHS <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
AREA = 13.094
EFFICIENCY = 0.156
#**********************************************************************#

#**********************************************************************#
# Set path for output files
output_directory <- "Data"
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
colnames(cpe) <- c("timestamp", "variable", "value","id")
cpe <- cpe[cpe$variable=="LED1_P" | cpe$variable=="LED2_P" | cpe$variable=="LED3_P",]
cpe <- cpe[complete.cases(cpe),]
# Convert power to W by dividing by 1000 and timestamp to GMT zone and add two hours
cpe <- cpe %>% mutate(value = as.numeric(value)/1000.0, 
         timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2))
write.csv(cpe, file=here(output_directory,"CPE_all_jun19_to_may20.csv"), row.names=FALSE)
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
colnames(sockets) <- c("timestamp","variable","value","id")
sockets <- sockets[sockets$variable=="vRELAY1_LVL" | sockets$variable=="AC_Day_Energy_session", ]
sockets <- sockets[complete.cases(sockets),]
# Convert timestamp to GMT zone and add two hours
sockets <- sockets %>% mutate(value = as.numeric(value), 
             timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2))
write.csv(sockets, file=here(output_directory,"sockets_all_jul19_to_may20.csv"), row.names=FALSE)
#**********************************************************************#

#**********************************************************************#
# Read system data- FILES IN BERLIN AND KIGALI TIME ZONE
filepath <- "Data/System Data/Jun19_to_Mar20"
file_list <- list.files(here(filepath))
file_list <- file_list[file.size(here(filepath,file_list))>0]
systemData <- data.frame()
for(k in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[k]), col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  # Input data file
  df <- read_csv(here(filepath,file_list[k]), col_names = headers, na="..", skip = 3)
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  # Subset data set - time is in Berlin (GMT+1) and Africa/Kigali time zone (GMT+2)
  df <- df[,c(headers[1], 
              headers[which(grepl("System overview AC Consumption L1 W", headers, fixed=TRUE) | 
                              grepl("Solar Charger PV power ", headers, fixed=TRUE) | 
                              grepl("Battery Monitor State of charge %", headers, fixed=TRUE) |
                              grepl("Battery Monitor Discharged Energy kWh", headers, fixed=TRUE) |
                              grepl("Battery Monitor Charged Energy kWh", headers, fixed=TRUE) )])]
  colnames(df)[1] <- c("timestamp")
  
  # Adjust timestamp if Berlin time
  if(grepl("Berlin", headers[1], fixed=TRUE)) {
    df <- df %>% mutate(timestamp=as.POSIXct(timestamp, format="%d/%m/%Y %H:%M", tz="GMT", 
               origin="1970-01-01") %m+% hours(1))
  } 
  
  systemData <- rbind(systemData, df)
}
systemData <- distinct(systemData)
write.csv(systemData, file=here(output_directory,"systemData_jun19_to_mar20.csv"), row.names=FALSE)
#**********************************************************************#

#**********************************************************************#
# Set working directory to read weather data
filepath <- "Data/Weather data"
file_list <- list.files(here(filepath))
weather <- data.frame()
for(i in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  weather_data <- read_csv(here(filepath,file_list[i]), col_names = headers, na="..", skip = 2)
  weather_data <- weather_data[,-c(1,3)]
  colnames(weather_data) <- c("timestamp","Potential_PV_power_W")
  weather_data <- weather_data %>% 
    mutate(timestamp=as.POSIXct(timestamp, tz="GMT",origin="1970-01-01",format="%d/%m/%Y %H:%M"),
           Potential_PV_power_W = Potential_PV_power_W * EFFICIENCY * AREA * 1000.0)
  
  weather <- rbind(weather, weather_data)
}
weather <- weather %>% mutate(date=date(timestamp))
weather <- weather[weather$date>="2019-07-01" & weather$date<="2020-03-31", ]
weather <- weather[,-3]
write.csv(weather, file=here(output_directory,"weather_jul19_to_mar20.csv"), row.names=FALSE)
#**********************************************************************#
#************************ EOF *****************************************#