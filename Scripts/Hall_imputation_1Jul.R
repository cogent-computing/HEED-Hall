#******************************************************************************************#
# This is the script for imputing missing data for community Hall                          #
# Author: K Bhargava                                                                       #
# Last updated on: 21st July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(xts) # for converting data into time series
library(timeDate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 5"
MONTHS <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data 
hall_hourly <- read.csv(here(filepath,"hourly_data_jul19_to_mar20.csv"), header = TRUE, 
                          stringsAsFactors=FALSE)
hall_hourly <- hall_hourly %>% mutate(date = as.Date(date))
#******************************************************************************************#

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("ma")
# Impute missing values for all variables
variables <- colnames(hall_hourly)[3:39]
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
  df <- gather(df, "variable","value",3:4)
    
  # Bind data for all SL
  na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = hall_hourly$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_ma_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
#Read in data files and plot data
na_seadec_imputedData <- read.csv(here(filepath,"na_seadec_ma_data.csv"), 
                                    header=TRUE, stringsAsFactors = FALSE)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
         month = factor(month, levels = MONTHS, labels = MONTHS))
na_seadec_sub <- gather(na_seadec_imputedData, "id", "value", c(3:77))

# Plot data for all variables to visualise original and imputed data
plotHourly <- function(df) {
  ggplot(df, aes(timestamp, value)) + facet_wrap(~id, scales = "free", ncol=2) + 
    geom_line(linetype=3) + theme(legend.position = "bottom") + 
    labs(x="Timestamp", y="Energy (Wh)", color="Variable", linetype="Variable") +
    theme(title=element_text(size=10))
}
# "Original and imputed Charged and discharged energy and SoC"
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(1:6)],])
ggsave(here(plot_dir,"imputed_soc.png"))
# "Original and imputed PV power and AC load data"
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(69:72,)],])
ggsave(here(plot_dir,"imputed_pv_ac.png"))
# "Original and imputed solar battery power and system battery power"
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(67:68,73:74)],]) 
ggsave(here(plot_dir,"imputed_bp.png"))

# Plot CPE data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(9:14)],]) +
  labs(title="Original and imputed CPE 1 data")
ggsave(here(plot_dir,"imputed_cpe1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(15:20)],]) +
  labs(title="Original and imputed CPE 2 data")
ggsave(here(plot_dir,"imputed_cpe2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(21:26)],]) +
  labs(title="Original and imputed CPE 3 data")
ggsave(here(plot_dir,"imputed_cpe3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(27:32)],]) +
  labs(title="Original and imputed CPE 4 data")
ggsave(here(plot_dir,"imputed_cpe4.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(33:38)],]) +
  labs(title="Original and imputed CPE 5 data")
ggsave(here(plot_dir,"imputed_cpe5.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(39:44)],]) +
  labs(title="Original and imputed CPE 6 data")
ggsave(here(plot_dir,"imputed_cpe6.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(45:50)],]) +
  labs(title="Original and imputed CPE 7 data")
ggsave(here(plot_dir,"imputed_cpe7.png"))

# Plot socket data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(51:54)],]) +
  labs(title="Original and imputed socket 1 data")
ggsave(here(plot_dir,"imputed_s1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(55:58)],]) +
  labs(title="Original and imputed socket 2 data")
ggsave(here(plot_dir,"imputed_s2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(59:62)],]) +
  labs(title="Original and imputed socket 3 data")
ggsave(here(plot_dir,"imputed_s3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(63:66)],]) +
  labs(title="Original and imputed socket 4 data")
ggsave(here(plot_dir,"imputed_s4.png"))
#******************************************************************************************#

#******************************************************************************************#
# Compute statistics for original and imputed data
stats_na_seadec_sub <- na_seadec_sub %>% group_by(id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), 
            sd = sd(value, na.rm=TRUE), skew = skewness(value, na.rm=TRUE), 
            kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
# Save the statistics file
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Correct data: replace negative values with zero for all variables except System Battery Power
# Compare value ranges of original and imputed data 
value_ranges <- na_seadec_sub %>% group_by(id) %>% 
  summarise(minValue=min(value,na.rm=TRUE), maxValue=max(value,na.rm=TRUE))

# To correct negative values - all values must be positive
na_seadec_sub[!(na_seadec_sub$id=="System.overview.Battery.Power.W_ma" | 
                  na_seadec_sub$id=="System.overview.Battery.Power.W_original"),] <- 
  na_seadec_sub[!(na_seadec_sub$id=="System.overview.Battery.Power.W_ma" | 
                    na_seadec_sub$id=="System.overview.Battery.Power.W_original"),] %>%
  mutate(value=ifelse(value<0,0,value))
na_seadec_correctedData <- spread(na_seadec_sub, id, value)

# Calculate +ve/-ve battery power, +ve/-ve solar battery power
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Positive.Solar.Battery.Power_ma=ifelse(Solar.Charger.Battery.watts.W_ma<0,0,
                                                Solar.Charger.Battery.watts.W_ma),
         Negative.Solar.Battery.Power_ma=ifelse(Solar.Charger.Battery.watts.W_ma>0,0,
                                                Solar.Charger.Battery.watts.W_ma),
         Positive.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original<0,0,
                                                      Solar.Charger.Battery.watts.W_original),
         Negative.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original>0,0,
                                                      Solar.Charger.Battery.watts.W_original),
         Positive.System.Battery.Power_ma=ifelse(System.overview.Battery.Power.W_ma<0,0,
                                                 System.overview.Battery.Power.W_ma),
         Negative.System.Battery.Power_ma=ifelse(System.overview.Battery.Power.W_ma>0,0,
                                                 System.overview.Battery.Power.W_ma),
         Positive.System.Battery.Power_original=ifelse(System.overview.Battery.Power.W_original<0,0,
                                                       System.overview.Battery.Power.W_original),
         Negative.System.Battery.Power_original=ifelse(System.overview.Battery.Power.W_original>0,0,
                                                       System.overview.Battery.Power.W_original))
#******************************************************************************************#

#******************************************************************************************#
#* Estimate power outages based on SoC - CPE readings cut off at 65% SOC; 50% for system and socket data
ggplot(na_seadec_correctedData[na_seadec_correctedData$month=="Feb",], aes(timestamp)) +
  geom_line(aes(y=Battery.Monitor.State.of.charge.._original)) 

test <- na_seadec_correctedData[na_seadec_correctedData$date>="2019-11-23" & 
                                  na_seadec_correctedData$date<="2019-11-26",]
ggplot(test, aes(timestamp, Battery.Monitor.State.of.charge.._original)) + geom_line()
ggplot(test, aes(timestamp, CPE6.LED1_P_original)) + geom_line()
ggplot(test, aes(timestamp)) + geom_line(aes(y=Battery.Monitor.State.of.charge.._original)) + 
  geom_line(aes(y=CPE7.LED3_P_original))
ggplot(test, aes(timestamp)) + geom_line(aes(y=System.overview.AC.Consumption.L1.W_original))
ggplot(test, aes(timestamp)) + geom_line(aes(y=S4.vRELAY1_LVL_original))

# Based on SoC, calculate actual PV power, Solar Battery power, AC consumption,
# System Battery power, +ve/-ve actual solar battery power, +ve/-ve actual battery power
socThresh <- 50 # for system and socket data
cpeThresh <- 65 # for CPE data - all CPE seem to disappear at same time hence and of all missing not checked
na_seadec_correctedData <- na_seadec_correctedData %>%
  mutate(Actual.CPE1.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE1.LED1_P_original), 0, CPE1.LED1_P_ma),
         Actual.CPE1.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE1.LED2_P_original), 0, CPE1.LED2_P_ma),
         Actual.CPE1.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE1.LED3_P_original), 0, CPE1.LED3_P_ma),
         Actual.CPE2.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE2.LED1_P_original), 0, CPE2.LED1_P_ma),
         Actual.CPE2.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE2.LED2_P_original), 0, CPE2.LED2_P_ma),
         Actual.CPE2.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE2.LED3_P_original), 0, CPE2.LED3_P_ma),
         Actual.CPE3.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE3.LED1_P_original), 0, CPE3.LED1_P_ma),
         Actual.CPE3.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE3.LED2_P_original), 0, CPE3.LED2_P_ma),
         Actual.CPE3.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE3.LED3_P_original), 0, CPE3.LED3_P_ma),
         Actual.CPE4.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE4.LED1_P_original), 0, CPE4.LED1_P_ma),
         Actual.CPE4.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE4.LED2_P_original), 0, CPE4.LED2_P_ma),
         Actual.CPE4.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE4.LED3_P_original), 0, CPE4.LED3_P_ma),
         Actual.CPE5.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE5.LED1_P_original), 0, CPE5.LED1_P_ma),
         Actual.CPE5.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE5.LED2_P_original), 0, CPE5.LED2_P_ma),
         Actual.CPE5.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE5.LED3_P_original), 0, CPE5.LED3_P_ma),
         Actual.CPE6.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE6.LED1_P_original), 0, CPE6.LED1_P_ma),
         Actual.CPE6.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE6.LED2_P_original), 0, CPE6.LED2_P_ma),
         Actual.CPE6.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE6.LED3_P_original), 0, CPE6.LED3_P_ma),
         Actual.CPE7.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE7.LED1_P_original), 0, CPE7.LED1_P_ma),
         Actual.CPE7.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE7.LED2_P_original), 0, CPE7.LED2_P_ma),
         Actual.CPE7.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                        is.na(CPE7.LED3_P_original), 0, CPE7.LED3_P_ma),
         Actual.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                    is.na(S1.AC_Day_Energy_session_original), 0, S1.AC_Day_Energy_session_ma),
         Actual.S2.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                    is.na(S2.AC_Day_Energy_session_original), 0, S2.AC_Day_Energy_session_ma),
         Actual.S3.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                    is.na(S3.AC_Day_Energy_session_original), 0, S3.AC_Day_Energy_session_ma),
         Actual.S4.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                    is.na(S4.AC_Day_Energy_session_original), 0, S4.AC_Day_Energy_session_ma),
         Actual.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                           is.na(S1.vRELAY1_LVL_original), 0, S1.vRELAY1_LVL_ma),
         Actual.S2.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                           is.na(S2.vRELAY1_LVL_original), 0, S2.vRELAY1_LVL_ma),
         Actual.S3.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                           is.na(S3.vRELAY1_LVL_original), 0, S3.vRELAY1_LVL_ma),
         Actual.S4.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                           is.na(S4.vRELAY1_LVL_original), 0, S4.vRELAY1_LVL_ma),
         Actual.Solar.Charger.Battery.Power_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh & 
                            is.na(Solar.Charger.Battery.watts.W_original)),0,Solar.Charger.Battery.watts.W_ma),
         Actual.PV.power.W_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh &
                            is.na(Solar.Charger.PV.power_original)), 0, Solar.Charger.PV.power_ma),
         Actual.AC.consumption_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh & 
                          is.na(System.overview.AC.Consumption.L1.W_original)),0,System.overview.AC.Consumption.L1.W_ma),
         Actual.System.Battery.Power_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh &
                          is.na(System.overview.Battery.Power.W_original)), 0,System.overview.Battery.Power.W_ma),
         Positive.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma<0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Negative.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma>0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Positive.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma<0,0,
                                                        Actual.System.Battery.Power_ma),
         Negative.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma>0,0,
                                                        Actual.System.Battery.Power_ma))
#******************************************************************************************#

#*#******************************************************************************************#
# Apply corrections based on component failures 
# Get patterns of missing data (system, CPE, sockets) and information in check sheets
missingData <- na_seadec_correctedData[,c(4, which(grepl("original", 
                                                         colnames(na_seadec_correctedData), fixed=TRUE)))]
missingData <- missingData[,c(1,4,36,37, which(grepl("LED1_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("LED2_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("LED3_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("vRELAY1_LVL", colnames(missingData), fixed=TRUE) ))]
missingData$load <- rowSums(missingData[5:29], na.rm=TRUE) 
missingData <- missingData[,c(1:4,30,which(grepl("LED1_P", colnames(missingData), fixed=TRUE) | 
                                             grepl("LED2_P", colnames(missingData), fixed=TRUE)|
                                             grepl("LED3_P", colnames(missingData), fixed=TRUE)|
                                             grepl("vRELAY1_LVL", colnames(missingData), fixed=TRUE) ))]

ggplot(missingData, aes(x=timestamp)) + 
  geom_line(aes(y=System.overview.AC.Consumption.L1.W_original), color="blue") + geom_line(aes(y=load)) 

missingData <- gather(missingData, id, value, 2:30)
missingData <- missingData %>% mutate(month=month(timestamp, label=TRUE, abbr=TRUE))
missingLevel <- missingData %>% group_by(timestamp, id) %>% summarise(count=ifelse(is.na(value),0,1))
missingLevel <- as.data.frame(missingLevel)
missingLevel <- missingLevel %>% mutate(id=substr(id, 1, str_length(id)-9))
missingLevel <- missingLevel %>% mutate(month=month(timestamp, label=TRUE, abbr=TRUE))
ggplot(missingLevel, aes(timestamp, id)) + geom_tile(aes(fill = count)) + xlab("X axis") + ylab("Y axis") + 
  labs(x = "Day of study", fill="Yield (%)") + THEME + 
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5))

# Apply corrections based on reported component failures - NONE REPORTED CLEARLY IN CHECK SHEETS
# Save corrected data
na_seadec_correctedData <- na_seadec_correctedData %>% mutate(month=as.character(month))
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#