#********************************************************************************************#
# Script for preprocessing Hall data from July 2019 to March 2020 to convert to hourly data, #
# analyse yield of hourly data and explore imputation techniques                             #
# Authors: K Bhargava                                                                        #
# Last updated on: 17th July, 2020                                                           #
#********************************************************************************************#

#******************************************************************************************#
# Import libraries for manipulating time series and plotting
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(mgcv) # for gam model-based imputation
library(xts)
library(MLmetrics) #for RMSE
library(timeDate) #for skewness
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all yield plots
MONTHS <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
THEME <- theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
               legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
               legend.title = element_text(size=10, family="Times New Roman"),
               panel.grid.major.y = element_line(colour="grey"), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=9, family="Times New Roman"),
               axis.title = element_text(size=10, family="Times New Roman")) 
#******************************************************************************************#

#******************************************************************************************#
# Set path for input and output files
filepath <- "Data"
plot_dir <- "Plots/Paper 5"
#******************************************************************************************#

#******************************************************************************************#
# Read all raw data adn add month, date and time 
cpe <- read_csv(here(filepath,"CPE_all_jun19_to_may20.csv"), col_names = TRUE)
cpe <- as.data.frame(cpe)
cpe <- cpe[,1:4]
cpe <- distinct(cpe)
cpe <- cpe %>% mutate(date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                      timeUse = hour(timestamp), id=substr(id,6,9)) 
cpe <- cpe[order(cpe$id, cpe$timestamp),]
cpe <- cpe[cpe$date>="2019-07-19" & cpe$date<="2020-03-31",]

sockets <- read_csv(here(filepath,"sockets_all_jul19_to_may20.csv"), col_names = TRUE) 
sockets <- as.data.frame(sockets)
sockets <- sockets[,1:4]
sockets <- distinct(sockets)
sockets <- sockets %>% mutate(date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                              timeUse = hour(timestamp), id=substr(id,6,7)) 
sockets <- sockets[order(sockets$id, sockets$timestamp),]
sockets <- sockets[sockets$date>="2019-07-19" & sockets$date<="2020-03-31",]

system <- read_csv(here(filepath,"systemData_jun19_to_mar20.csv"), col_names = TRUE)  
system <- as.data.frame(system)
system <- system[,1:9]
system <- system %>% mutate(date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                            timeUse = hour(timestamp)) 
system <- system[order(system$timestamp),]
system <- system[system$date>="2019-07-19" & system$date<="2020-03-31",]

weather <- read_csv(here(filepath,"weather_jul19_to_mar20.csv"), col_names = TRUE) 
weather <- weather %>% mutate(date = date(timestamp), 
                              month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                              timeUse = hour(timestamp)) 
weather <- weather[weather$date>="2019-07-19" & weather$date<="2020-03-31",]
weather <- weather[complete.cases(weather),]
#******************************************************************************************#

#******************************************************************************************#
# Plot yield per hour for each day 
system_all <- gather(system, "id", "value", 2:9)

# Count number of readings per hour and calculate yield
# Frequency of data collection for CPE and socket data is 10 mins
cpe_qual <- cpe %>% group_by(id, variable, date, timeUse) %>% summarise(count=length(unique(timestamp)))
cpe_qual <- as.data.frame(cpe_qual)
cpe_qual <- cpe_qual %>% mutate(yield=count*100/6, yield2=ifelse(yield>100,100,yield))

sockets_qual <- sockets %>% group_by(id, variable, date, timeUse) %>%summarise(count=length(unique(timestamp)))
sockets_qual <- as.data.frame(sockets_qual)
sockets_qual <- sockets_qual %>% mutate(yield=count*100/6,yield2=ifelse(yield>100,100,yield))

# Frequency of data collection was 15min
system_qual <- system_all %>% group_by(id, date, timeUse) %>% summarise(count = length(unique(timestamp)))
system_qual <- as.data.frame(system_qual)
system_qual <- system_qual %>% mutate(yield=count*100/4, yield2=ifelse(yield>100,100,yield))

pal <- wes_palette("Zissou1", 100, type = "continuous")
plotYield <- function(df) {
  ggplot(df, aes(date, timeUse)) + geom_tile(aes(fill = yield2)) + 
    scale_fill_gradientn(colours = pal, breaks=c(0,25,50,75,100)) + 
    scale_y_continuous(breaks=seq(0,24,by=4)) + xlab("X axis") + ylab("Y axis") + 
    labs(x = "Day of study", fill="Yield (%)") + THEME + 
    guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5))
  
}
# "Yield per hour for Hall CPE: 19 Jul'19 - 31 Mar'20"
plotYield(cpe_qual[cpe_qual$variable=="LED1_P",]) + facet_wrap(~id, nrow=2) + labs(y="Time of day")
ggsave(here(plot_dir,"yield_cpe.pdf"), width = 8, height = 8, units = "cm")

# "Yield per hour for Hall sockets: 19 Jul'19 - 31 Mar'20"
plotYield(sockets_qual[sockets_qual$variable=="vRELAY1_LVL",]) + facet_wrap(~id, nrow=2) + 
  labs(y="Time of day")
ggsave(here(plot_dir,"yield_sockets.pdf"), width = 8, height = 8, units = "cm")

# "Yield per hour for Hall system data: 19 Jul'19 - 31 Mar'20"
plotYield(system_qual[system_qual$id=="Solar Charger PV power",]) + labs(y="Time of day")
ggsave(here(plot_dir,"yield_system_hall.pdf"), width = 8, height = 8, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Convert CPE data to hourly means 
cpe_hourly <- cpe %>% group_by(id, variable, date, timeUse) %>% summarise(value=mean(value, na.rm=TRUE))
cpe_hourly <- as.data.frame(cpe_hourly)
cpe_hourly <- cpe_hourly %>% mutate(id = paste(id, variable, sep="-"))
cpe_hourly <- cpe_hourly[,-2] # Remove variable

# Convert socket data to hourly means for vRELAY1_LVL data
sockets_hourly_vRelay1LVL <- sockets[sockets$variable=="vRELAY1_LVL",] %>% 
  group_by(id, variable, date, timeUse) %>% summarise(value = mean(value, na.rm=TRUE))
sockets_hourly_vRelay1LVL <- as.data.frame(sockets_hourly_vRelay1LVL)

# Convert socket data to hourly means for AC Day energy session data
# Reset values to 0 at start of each day - then take hourly diff to get hourly values
sockets_hourly_energySession <- data.frame()
for(i in seq_along(unique(sockets$id))) {
  df_id <- sockets[sockets$id == unique(sockets$id)[i] & sockets$variable=="AC_Day_Energy_session", ]
  
  #Correct cumulative total energy if not reset at 0th hour for a day
  df3_clean <- data.frame()
  for(e in seq_along(unique(df_id$date))) {
    df_date <- df_id[df_id$date == unique(df_id$date)[e], ]
    if(df_date$value[1]!=0 | any(diff(df_date$value)<0)) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      if(length(df_date$timestamp)>1) {
        for(l in 2:length(df_date$timestamp)) {
          if(flag==FALSE & df_date$value[l] < df_date$value[l-1]) {
            flag=TRUE
            index <- l
          }
        }
        if(flag==FALSE) { #Means the value never reset to 0
          #Subtract value at 0th index from all values
          df_date$value2 <- df_date$value - df_date$value[1]
        } else {
          df_date$value2 <- df_date$value
          df_date$value2[1:(index-1)] <- (df_date$value[1:(index-1)] - df_date$value[1])
          df_date$value2[index:length(df_date$timestamp)] <- 
            df_date$value[index:length(df_date$timestamp)] + df_date$value2[index-1]
        }
      } else {
        df_date$value2 <- df_date$value
      }
    } else {
      df_date$value2 <- df_date$value
    }
    df3_clean <- rbind(df3_clean, df_date)
  }
  
  #Extract cumulative total energy per hour
  df <- df3_clean %>% group_by(id, variable, date, timeUse) %>%
    summarise(value = value2[length(na.omit(value2))])
  df <- as.data.frame(df)
  # Convert value from Wm to Wh
  df <- df %>% mutate(value=value*0.017)
  
  # Take hourly differences to get energy usage per hour for each day
  df <- df %>% group_by(date) %>% mutate(value=c(value[1], diff(value)))
  df <- as.data.frame(df)
  
  #Create a data frame with the above values and bind to a complete df 
  sockets_hourly_energySession <- rbind(sockets_hourly_energySession, df)
}
# Combine AC energy session and vRelay1LVL data
sockets_hourly <- data.frame()
sockets_hourly <- rbind(sockets_hourly, sockets_hourly_vRelay1LVL,sockets_hourly_energySession)
sockets_hourly <- sockets_hourly %>% mutate(id = paste(id, variable, sep="-"))
sockets_hourly <- sockets_hourly[,-2] # Remove variable

# Convert system data to hourly means for AC consumption, PV power and SoC
system_hourly1 <- system_all[system_all$id=="Battery Monitor Voltage V" | 
                               system_all$id=="Battery Monitor State of charge %" |
                               system_all$id=="Solar Charger Battery watts W" |
                               system_all$id=="Solar Charger PV power" | 
                               system_all$id=="System overview AC Consumption L1 W" | 
                               system_all$id=="System overview Battery Power W",] %>% 
  group_by(id, date, timeUse) %>% summarise(value = mean(value, na.rm = TRUE))
system_hourly1 <- as.data.frame(system_hourly1)

# Convert system data to hourly means for Charged and Discharged energy
system_hourly2 <- system_all[system_all$id=="Battery Monitor Charged Energy kWh" |
                               system_all$id=="Battery Monitor Discharged Energy kWh",] %>%
  group_by(id, date, timeUse) %>% summarise(value = mean(value, na.rm = TRUE))
system_hourly3 <- data.frame()
for(i in seq_along(unique(system_hourly2$id))) {
  df <- system_hourly2[system_hourly2$id == unique(system_hourly2$id)[i], ]
  for(j in 2:length(df$value)) {
    if(is.na(df$value[j])) {
      df$value[j] <- df$value[j-1]
    }
  }
  df <- df %>% mutate(value2=value)
  for(j in 2:length(df$value)) {
      df$value[j] <- df$value2[j] - df$value2[j-1]
  }
  df <- as.data.frame(df)
  df <- df[,c(1:4)]
  df$value[is.na(df$value)] <- 0
  df <- df %>% mutate(value=value*1000.0) #Convert to Wh
  df <- df[-1, ] # Remove the first reading as it is the actual value
  system_hourly3 <- rbind(system_hourly3, df)
}

# Combine all system data
system_hourly <- data.frame()
system_hourly <- rbind(system_hourly, system_hourly1, system_hourly3)

# Combine all hourly data
hall_hourly <- data.frame()
hall_hourly <- rbind(hall_hourly, cpe_hourly, sockets_hourly, system_hourly)
hall_hourly <- spread(hall_hourly, id, value)
colnames(hall_hourly) <- c(colnames(hall_hourly)[1:2], 
                           substr(colnames(hall_hourly)[3:4],1,str_length(colnames(hall_hourly)[3:4])-4),
                           colnames(hall_hourly)[5:39])

# Fill in missing data between 19th July 2019 to 31st Mar 2020 by NA
all_days <- seq(as.Date("2019-07-19"), as.Date("2020-03-31"), by="days")
all_hours <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1),by = "1 hour"), "%H", tz="GMT")
all_hours <- as.numeric(all_hours[-25])

# Create a complete data set for all days - add NA for all missing values
data <- data.frame()
for(i in seq_along(all_days)) {
  data <- rbind(data, data.frame(date = rep(all_days[i], length(all_hours)), timeUse=all_hours))
}
df <- data.frame(matrix(ncol = 37, nrow = length(data$date)))
colnames(df) <- colnames(hall_hourly)[3:39]
df[colnames(df)] <- sapply(df[colnames(df)],as.numeric)
data <- cbind(data, df)

# Include rows from 'data' to get all days not there in system_hourly
hall_hourly <- hall_hourly %>% mutate(id = paste(date,timeUse, sep=" ")) 
data <- data %>% mutate(id=paste(date,timeUse, sep=" "))
hall_hourly <- rbind(hall_hourly, data[!(data$id %in% hall_hourly$id),])
hall_hourly <- hall_hourly[order(hall_hourly$date,hall_hourly$timeUse),]

# Add in weather data - weather data unavailable for April
weather <- weather %>% mutate(id=paste(date,timeUse, sep=" "))
hall_hourly <- merge(hall_hourly, weather[,-c(1,3,4,5)], by="id")
hall_hourly <- hall_hourly[,-1] # Remove id
hall_hourly <- hall_hourly[order(hall_hourly$date, hall_hourly$timeUse),]
# Subset data from 20th July as only half day of 19th is available
hall_hourly <- hall_hourly[hall_hourly$date>="2019-07-20",]
write.csv(hall_hourly, file=here(filepath,"hourly_data_jul19_to_mar20.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data and calculate % missing data for each variable
hall_hourly <- read.csv(here(filepath,"hourly_data_jul19_to_mar20.csv"), header = TRUE, 
                      stringsAsFactors=FALSE)
hall_hourly <- hall_hourly %>% mutate(date = as.Date(date))

# Get summary of the data to get number of NA values for each var
summary(hall_hourly) # ~ 9K values missing for all variables 
# Get percentage missing data for each variable for each SL
system <- gather(hall_hourly,"id","value",3:40)
missingData <- system %>% group_by(id) %>% summarise(missingPercent = sum(is.na(value))*100/length(value))  
missingData <- spread(missingData, id, missingPercent)
write.csv(missingData, file=here(filepath,"missing_sl_data.csv"), row.names=FALSE)

# Plot hourly data to see trend and seasonality - seasonality in data is 1 day as expected
# Add timestamp and month to the data
system <- system %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)),
                            timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                                      paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
                            month2 = factor(month, levels = MONTHS, labels = MONTHS))
# Plot hourly values against time
plotHourly <- function(df) {
  ggplot(df, aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
    geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + THEME +
    labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable")
}
# "Energy profile of CPE1 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(5:7)],])
ggsave(here(plot_dir,"hall_cpe1.png"))
# "Energy profile of CPE2 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(8:10)],]) 
ggsave(here(plot_dir,"hall_cpe2.png"))
# "Energy profile of CPE3 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(11:13)],]) 
ggsave(here(plot_dir,"hall_cpe3.png"))
# "Energy profile of CPE4 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(14:16)],]) 
ggsave(here(plot_dir,"hall_cpe4.png"))
# "Energy profile of CPE5 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(17:19)],])
ggsave(here(plot_dir,"hall_cpe5.png"))
# "Energy profile of CPE6 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(20:22)],])
ggsave(here(plot_dir,"hall_cpe6.png"))
# "Energy profile of CPE7 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(23:25)],]) 
ggsave(here(plot_dir,"hall_cpe7.png"))

# "Energy profile of Sockets 1 and 2 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(26:29)],]) 
ggsave(here(plot_dir,"hall_s1_s2.png"))
# "Energy profile of Sockets 3 and 4 at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(30:33)],]) 
ggsave(here(plot_dir,"hall_s3_s4.png"))

# Hourly plots for system data - all data is cyclic
# "Charged energy at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(1)],])
ggsave(here(plot_dir,"charged_energy.png"))
# "Discharged energy at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(2)],]) 
ggsave(here(plot_dir,"discharged_energy.png"))
# "State of Charge at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(3)],])
ggsave(here(plot_dir,"state_of_charge.png"))
# Battery voltage
plotHourly(system[system$id %in% unique(system$id)[c(4)],])
ggsave(here(plot_dir,"voltage.png"))
# Solar Charger Battery power
plotHourly(system[system$id %in% unique(system$id)[c(34)],])
ggsave(here(plot_dir,"solar_battery_power.png"))
# "PV power at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(35)],]) 
ggsave(here(plot_dir,"pv_power.png"))
# "AC consumption at Hall: 20 Jul'19 to 31 Mar'20"
plotHourly(system[system$id %in% unique(system$id)[c(36)],]) 
ggsave(here(plot_dir,"ac_load.png"))
# System Battery power
plotHourly(system[system$id %in% unique(system$id)[c(37)],])
ggsave(here(plot_dir,"system_battery_power.png"))
#******************************************************************************************#

#******************************************************************************************#
# Impute data for Hall CPE and sockets and system data
methodImpute <- c("interpolation", "mean","kalman","ma")
# Impute missing values for all variables
variables <- c("CPE1.LED1_P", "CPE1.LED2_P", "CPE1.LED3_P", "CPE2.LED1_P", "CPE2.LED2_P",                        
               "CPE2.LED3_P", "CPE3.LED1_P", "CPE3.LED2_P", "CPE3.LED3_P", "CPE4.LED1_P",
               "CPE4.LED2_P", "CPE4.LED3_P", "CPE5.LED1_P", "CPE5.LED2_P", "CPE5.LED3_P",
               "CPE6.LED1_P", "CPE6.LED2_P", "CPE6.LED3_P", "CPE7.LED1_P", "CPE7.LED2_P",
               "CPE7.LED3_P")
variables <- c("S1.AC_Day_Energy_session", "S1.vRELAY1_LVL", "S2.AC_Day_Energy_session",
               "S2.vRELAY1_LVL", "S3.AC_Day_Energy_session", "S3.vRELAY1_LVL",                     
               "S4.AC_Day_Energy_session", "S4.vRELAY1_LVL")
variables <- c("Battery.Monitor.Charged.Energy", "Battery.Monitor.Discharged.Energy",  
               "Battery.Monitor.State.of.charge..", "Battery.Monitor.Voltage.V", 
               "Solar.Charger.Battery.watts.W", "Solar.Charger.PV.power", 
               "System.overview.AC.Consumption.L1.W", "System.overview.Battery.Power.W")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  df <- hall_hourly[c("date","timeUse",variables[k])]
  # Convert data frame into a time series using xts to serve as input to na_seadec function
  # For this data, seasonality is 1 day with a reading every hour
  df.ts <- spread(df, timeUse, variables[k])
  df.ts <- xts(df.ts[,-1], order.by=as.Date(df.ts[,1], "%Y-%m-%d"))
  
  # Impute data using different functions of na_seadec and bind to df
  for(j in seq_along(methodImpute)) {
    df1 <- as.data.frame(na_seadec(df.ts, algorithm=methodImpute[j], find_frequency=TRUE))
    df1 <- df1 %>% mutate(date=row.names(df1))
    df1 <- gather(df1, "timeUse", "value", 1:24)
    df1[is.na(df1)] <- 0
    df1 <- df1[order(df1$date),]
    df <- cbind(df, df1$value)
  }
  colnames(df) <- c(colnames(df)[1:2],paste(variables[k],"original",sep="_"),
                    paste(variables[k],methodImpute,sep="_"))
  df <- gather(df, "variable","value",3:7)
  
  # Bind data for all SL
  na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = hall_hourly$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = MONTHS, labels = MONTHS))

system2 <- gather(na_seadec_imputedData, id, value, 3:27)
plotHourly <- function(df) {
  ggplot(df, aes(timestamp, value)) + facet_wrap(~id, scales = "free") + 
    geom_line(linetype=3) + THEME +
    labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable")
}
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(1:5)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(6:10)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(11:15)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(16:20)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(21:25)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(26:30)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(31:35)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(36:40)],])
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(41:45)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(46:50)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(51:55)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(56:60)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(61:65)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(66:70)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(71:75)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(76:80)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(81:85)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(86:90)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(91:95)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(96:100)],]) 
plotHourly(system2[system2$month=="Nov" & system2$id %in% unique(system2$id)[c(101:105)],]) 
#******************************************************************************************#
