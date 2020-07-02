#******************************************************************************************#
# This is the script for imputing missing data for all Community Hall                      #
# Author: K Bhargava                                                                       #
# Last updated on: 1st July 2020                                                           #
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
variables <- c("CPE1.LED1_P", "CPE1.LED2_P", "CPE1.LED3_P", "CPE2.LED1_P", "CPE2.LED2_P",                        
               "CPE2.LED3_P", "CPE3.LED1_P", "CPE3.LED2_P", "CPE3.LED3_P", "CPE4.LED1_P",
               "CPE4.LED2_P", "CPE4.LED3_P", "CPE5.LED1_P", "CPE5.LED2_P", "CPE5.LED3_P",
               "CPE6.LED1_P", "CPE6.LED2_P", "CPE6.LED3_P", "CPE7.LED1_P", "CPE7.LED2_P",
               "CPE7.LED3_P", "S1.AC_Day_Energy_session", "S1.vRELAY1_LVL", 
               "S2.AC_Day_Energy_session","S2.vRELAY1_LVL", "S3.AC_Day_Energy_session", 
               "S3.vRELAY1_LVL", "S4.AC_Day_Energy_session", "S4.vRELAY1_LVL", 
               "Battery.Monitor.Charged.Energy","Battery.Monitor.Discharged.Energy", 
               "Battery.Monitor.State.of.charge..", 
               "Solar.Charger.PV.power","System.overview.AC.Consumption.L1.W")
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
         month = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
na_seadec_sub <- gather(na_seadec_imputedData, "id", "value", c(3:71))

# Plot data for all variables to visualise original and imputed data
plotHourly <- function(df) {
  ggplot(df, aes(timestamp, value)) + facet_wrap(~id, scales = "free", ncol=2) + 
    geom_line(linetype=3) + theme(legend.position = "bottom") + 
    labs(x="Timestamp", y="Energy (Wh)", color="Variable", linetype="Variable") +
    theme(title=element_text(size=10))
}
# Plot system data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(1:6)],]) +
  labs(title="Original and imputed Charged and discharged energy and SoC")
ggsave(here(plot_dir,"imputed_soc.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(65:68)],]) +
  labs(title="Original and imputed PV power and AC load data")
ggsave(here(plot_dir,"imputed_pv_ac.png"))

# Plot socket data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(49:52)],]) +
  labs(title="Original and imputed socket 1 data")
ggsave(here(plot_dir,"imputed_s1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(53:56)],]) +
  labs(title="Original and imputed socket 2 data")
ggsave(here(plot_dir,"imputed_s2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(57:60)],]) +
  labs(title="Original and imputed socket 3 data")
ggsave(here(plot_dir,"imputed_s3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(61:64)],]) +
  labs(title="Original and imputed socket 4 data")
ggsave(here(plot_dir,"imputed_s4.png"))

# Plot CPE data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(7:12)],]) +
  labs(title="Original and imputed CPE 1 data")
ggsave(here(plot_dir,"imputed_cpe1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(13:18)],]) +
  labs(title="Original and imputed CPE 2 data")
ggsave(here(plot_dir,"imputed_cpe2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(19:24)],]) +
  labs(title="Original and imputed CPE 3 data")
ggsave(here(plot_dir,"imputed_cpe3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(25:30)],]) +
  labs(title="Original and imputed CPE 4 data")
ggsave(here(plot_dir,"imputed_cpe4.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(31:36)],]) +
  labs(title="Original and imputed CPE 5 data")
ggsave(here(plot_dir,"imputed_cpe5.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(37:42)],]) +
  labs(title="Original and imputed CPE 6 data")
ggsave(here(plot_dir,"imputed_cpe6.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(43:48)],]) +
  labs(title="Original and imputed CPE 7 data")
ggsave(here(plot_dir,"imputed_cpe7.png"))

# Compute statistics for original and imputed data
stats_na_seadec_sub <- na_seadec_sub %>% group_by(id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), 
            sd = sd(value, na.rm=TRUE), skew = skewness(value, na.rm=TRUE), 
            kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
stats_na_seadec_sub <- gather(stats_na_seadec_sub, "variable", "value", 2:6)

# Plot statistics to visualise the diff in mean, median and sd of original and imputed data
# System data: 1:6,66:69; sockets: 50:65; cpe: 7:48
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$id %in% unique(stats_na_seadec_sub$id)[c(29:48)] & 
                             stats_na_seadec_sub$variable=="mean",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$id %in% unique(stats_na_seadec_sub$id)[c(29:48)] & 
                             stats_na_seadec_sub$variable=="median",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$id %in% unique(stats_na_seadec_sub$id)[c(29:48)] & 
                             stats_na_seadec_sub$variable=="sd",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))

# Save the statistics file
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)

# Compare value ranges of original and imputed data 
value_ranges <- na_seadec_sub %>% group_by(id) %>% 
  summarise(minValue=min(value,na.rm=TRUE), maxValue=max(value,na.rm=TRUE))

# To correct negative values - all values must be positive
na_seadec_correctedData <- na_seadec_sub %>% mutate(value=ifelse(value<0,0,value))
value_ranges <- na_seadec_correctedData %>% group_by(id) %>% 
  summarise(minValue=min(value,na.rm=TRUE), maxValue=max(value,na.rm=TRUE))
na_seadec_correctedData <- spread(na_seadec_correctedData, id, value)
# Save corrected data
na_seadec_correctedData <- na_seadec_correctedData %>% mutate(month=as.character(month))
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
