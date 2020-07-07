#******************************************************************************************#
# This is the script for analysing predicted system data for Hall                          #
# Author: K Bhargava                                                                       #
# Last updated on: 6th July 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
THEME <- theme(plot.title = element_text(size=9), legend.position = "bottom",
               legend.key.size = unit(0.5, "cm"), 
               legend.margin = margin(t=0,r=0,b=0,l=0), panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), axis.text = element_text(size=10), 
               axis.title = element_text(size=10)) 
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/System predicted"
file_list <- list.files(here(filepath))
plot_dir <- "Plots/Paper 5"
output_dir <- "Data"
#******************************************************************************************#

#******************************************************************************************#
# Read predicted system data and subset for months July to Dec and Jan to Mar
systemData <- data.frame()
for(k in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[k]), col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  # Input data file
  df <- read_csv(here(filepath,file_list[k]), col_names = headers, na="..", skip = 2)
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  #Extract values Time, AC Primary Load kW, Actual PV power, Potential PV power output kW, 
  # Capture losses kW, Charge Power kW, Discharge Power kW, State of Charge
  df <- df[,c(headers[1], 
              headers[which(grepl("AC Primary Load kW", headers, fixed=TRUE) | 
                              grepl("Actual PV power ", headers, fixed=TRUE) | 
                              grepl("Potential PV Power Output kW", headers, fixed=TRUE) |
                              grepl("Capture losses kW", headers, fixed=TRUE) |
                              grepl("Generic 1kWh Li-Ion Charge Power kW", headers, fixed=TRUE) | 
                              grepl("Generic 1kWh Li-Ion Discharge Power kW", headers, fixed=TRUE) |
                              grepl("Generic 1kWh Li-Ion State of Charge %", headers, fixed=TRUE) )])]
  colnames(df)[1] <- "timestamp"
  df <- df %>% mutate(timestamp=as.POSIXct(timestamp, tz="GMT", origin="1970-01-01", 
                                           format="%d/%m/%Y %H:%M") %m+% years(12), 
                      month=as.character(month(timestamp, label=TRUE, abbr=TRUE)))
  
  systemData <- rbind(systemData, df)
}
# Trim data to keep months - Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb, Mar
systemData <- systemData[!(systemData$month %in% c("Apr","May","Jun")),]
write.csv(systemData, file=here(output_dir,"systemData_predicted_jul_mar.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
systemData <- read.csv(here(output_dir,"systemData_predicted_jul_mar.csv"), header=TRUE, 
                       stringsAsFactors=FALSE)
systemData <- systemData %>% mutate(timestamp=as.POSIXct(timestamp, tz="GMT", 
                                    origin="1970-01-01"), timeUse=hour(timestamp))
systemData <- gather(systemData, id, value, 2:8)

typical_pred <- systemData %>% group_by(id, timeUse) %>% 
  summarise(value = mean(value, na.rm=TRUE))
typical_pred <- as.data.frame(typical_pred)
typical_pred <- spread(typical_pred, id, value)
colnames(typical_pred) <- c("timeUse","E_load","E_a","L_c","B_cp","B_dp","SoC","E_p")

ggplot(typical_pred, aes(x=timeUse)) + geom_line(aes(y=B_cp, color="B_cp", linetype="B_cp")) +
  geom_line(aes(y=abs(B_dp), color="B_dp",linetype="B_dp")) + 
  geom_line(aes(y=E_a, color="E_a",linetype="E_a")) +
  geom_line(aes(y=E_load, color="E_load",linetype="E_load")) + 
  geom_line(aes(y=E_p, color="E_p",linetype="E_p")) +
  geom_line(aes(y=L_c, color="L_c",linetype="L_c")) + 
  geom_line(aes(y = SoC/65, color = "SoC",linetype="SoC")) + 
  scale_y_continuous(breaks= seq(0,1.4,0.2), sec.axis = sec_axis(~.*65, 
                                                                 name = "State of Charge (%)")) +
  labs(y="Power (kW)", x = "Time of day", colour="Parameter", linetype="Parameter", 
       title="Actual typical day profile for the Hall between Jul'19 and Mar'20") +
  scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
ggsave(here(plot_dir,"typical_pred_jul19_mar20.png"))
#******************************************************************************************#