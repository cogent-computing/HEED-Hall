library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

#Read in all CPE files
cpe <- data.frame()
for(k in 1:7) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
  if(k==1) {
    setwd("./Hall CPE1/")
  } else if(k==2) {
    setwd("./Hall CPE2/")
  } else if(k==3) {
    setwd("./Hall CPE3/")
  } else if(k==4) {
    setwd("./Hall CPE4/")
  } else if(k==5) {
    setwd("./Hall CPE5 (outdoor lights)/")
  } else if(k==6) {
    setwd("./Hall CPE6/")
  } else if(k==7) {
    setwd("./Hall CPE7/")
  }
  
  #For each CPE - read files for each month - July, Aug, Sep, Oct, Nov and Dec
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec")
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./07 2019/")
    } else if(j==2) {
      setwd("../08 2019/")
    } else if(j==3) {
      setwd("../09 2019/")
    } else if(j==4) {
      setwd("../10 2019/")
    } else if(j==5) {
      setwd("../11 2019/")
    } else if(j==6) {
      setwd("../12 2019/")
    }
    
    #For each month read all files
    file_list <- list.files()
    #Read each file in the list and append to cpe with the correct label
    for(i in 1:length(file_list)) {
      df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
      colnames(df) <- c("timestamp", "variable", "value")
      df$value <- as.character(df$value)
      df$variable <- as.character(df$variable)
      
      #Separate the power consumption variables only
      df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
      df_sub$value <- as.numeric(df_sub$value)/1000.0 #Converting to W
      
      #Add ID to each file
      df_sub$id <- rep(paste("CPE",k, sep=""), length(df_sub$timestamp))
      
      #Add to cpe 
      cpe <- rbind(cpe, df_sub)
    }
  }
}
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe, file="CPE_all.csv", row.names=FALSE)

#############################################################################
#Reading all socket files
sockets <- data.frame()
for(k in 1:4) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
  if(k==1) {
    setwd("./Hall S1/")
  } else if(k==2) {
    setwd("./Hall S2")
  } else if(k==3) {
    setwd("./Hall S3/")
  } else if(k==4) {
    setwd("./Hall S4/")
  }
  
  #For each Socket - read files for each month - July, Aug, Sep, Oct,Nov and Dec
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./07 2019/")
    } else if(j==2) {
      setwd("../08 2019/")
    } else if(j==3) {
      setwd("../09 2019/")
    } else if(j==4) {
      setwd("../10 2019/")
    } else if(j==5) {
      setwd("../11 2019/")
    } else if(j==6) {
      setwd("../12 2019/")
    }
    
    #For each month list all files
    file_list <- list.files()
    #Read each daily file in the list and append to sockets with the correct label
    for(i in 1:length(file_list)) {
      df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
      colnames(df) <- c("timestamp", "variable", "value")
      df$value <- as.character(df$value)
      df$variable <- as.character(df$variable)
      
      #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
      df_sub <- df[df$variable=="vRELAY1_LVL" | 
                  df$variable=="vRELAY1_V" | df$variable=="vRELAY1_I" |
                  df$variable=="AC_Day_Energy_session" | df$variable=="vRELAY1_VA" |
                  df$variable=="AC_Night_Energy_session" | 
                  df$variable=="vRELAY1_PF", ]
      df_sub$value <- as.numeric(df_sub$value)
      
      #Add ID to each file
      df_sub$id <- rep(paste("S",k, sep=""), length(df_sub$timestamp))
      
      #Add to cpe 
      sockets <- rbind(sockets, df_sub)
    }
  }
}
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets, file="Sockets_all.csv", row.names=FALSE)

###############################################################################
#Once the values are all read - analyse the data to get missing days and hours
#Create a vector of all dates in each month
july_2019 <- seq(as.Date("2019-07-19"), as.Date("2019-07-31"), by="days")
aug_2019 <- seq(as.Date("2019-08-01"), as.Date("2019-08-31"), by="days")
sep_2019 <- seq(as.Date("2019-09-01"), as.Date("2019-09-30"), by="days")
oct_2019 <- seq(as.Date("2019-10-01"), as.Date("2019-10-31"), by="days")
nov_2019 <- seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by="days")
dec_2019 <- seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by="days")
all_days <- c(july_2019, aug_2019, sep_2019, oct_2019, nov_2019, dec_2019)
#Create a vector for all 24 hours in a day
all_hours <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                by = "1 hour"), "%H", tz="GMT")
all_hours <- all_hours[-25]

############################################################################
#Analyse the cpe data to extract hourly means for instantaenous power
#Total days of study = 135 (19 July to 30 Nov 2019)
cpe_sub <- data.frame()
for(i in 1:length(unique(cpe$id))) {
  df <- cpe[cpe$id == unique(cpe$id)[i], ]
  
  #Add time - we need to extract hours to get hourly means
  df$time <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01")
  #Get date per file
  df$date <- date(df$time)
  df$timeUse <- format(df$time, format='%H')
  
  #Extract hourly means for each day per variable - led1_p, led2_p, led3_p
  df2 <- df %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = mean(value))
  df2 <- as.data.frame(df2)
  
  cpe_sub <- rbind(cpe_sub, df2)
}
cpe_sub$month <- as.character(month(cpe_sub$date, label=TRUE, abbr=TRUE))

setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe_sub, file="CPE_all_hourly_mean.csv", row.names=FALSE)

################################################################################
#Analyse the sockets data to extract hourly means for instantaenous power and 
#hourly totals for AC_day_energy_session. Total days of study = 166 till Dec
sockets_sub <- data.frame()
for(i in 1:length(unique(sockets$id))) {
  df <- sockets[sockets$id == unique(sockets$id)[i], ]
  
  #Add time - we need to extract hours to get hourly means
  df$time <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01")
  #Get date per file
  df$date <- date(df$time)
  df$timeUse <- format(df$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df[df$variable!="AC_Day_Energy_session",] %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = mean(value))
  df2 <- as.data.frame(df2)
  
  #Correct cumulative total energy if not reset at 0th hour for a day
  df3_sub <- df[df$variable=="AC_Day_Energy_session", ]
  df3_clean <- data.frame()
  for(d in 1:length(unique(df3_sub$id))) {
    df_id <- df3_sub[df3_sub$id==unique(df3_sub$id)[d],]
    for(e in 1:length(unique(df_id$date))) {
      df_date <- df_id[df_id$date == unique(df_id$date)[e], ]
      if(df_date$value[1]!=0) { #If value has not reset
        #find if and where the value has reset
        flag=FALSE
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
      df3_clean <- rbind(df3_clean, df_date)
    }
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_clean %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = value2[length(value2)])
  df3 <- as.data.frame(df3)
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  
  #Create a data frame with the above values and bind to a complete df 
  sockets_sub <- rbind(sockets_sub, df2)
}
sockets_sub$month <- as.character(month(sockets_sub$date, label=TRUE, abbr=TRUE))

setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets_sub, file="Sockets_all_hourly_mean.csv", row.names=FALSE)

##################################################################################
#Number of hours on per day for each id and each variable
cpe_on_hours <- cpe_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(timeUse))
cpe_on_hours <- as.data.frame(cpe_on_hours)
#Once we have hours active for each available day, look for missing dates
cpe_on_hours2 <- data.frame()
for(i in 1:length(unique(cpe_on_hours$id))) {
  df <- cpe_on_hours[cpe_on_hours$id == unique(cpe_on_hours$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    cpe_on_hours2 <- rbind(cpe_on_hours2, df_sub)
  }
}
cpe_on_hours2 <- cpe_on_hours2[order(cpe_on_hours2$date),]
cpe_on_hours2$month <- as.character(month(cpe_on_hours2$date,label=TRUE, abbr=TRUE))

cpe_on_hours2$date <- as.Date(cpe_on_hours2$date)
cpe_on_hours2$id <- as.factor(cpe_on_hours2$id)
cpe_on_hours2$variable <- as.factor(cpe_on_hours2$variable)
cpe_on_hours2[cpe_on_hours2$month=="Jul",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Jul 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_on_hours2[cpe_on_hours2$month=="Aug",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Aug 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_on_hours2[cpe_on_hours2$month=="Sep",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Sep 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_on_hours2[cpe_on_hours2$month=="Oct",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Oct 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_on_hours2[cpe_on_hours2$month=="Nov",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Nov 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_on_hours2[cpe_on_hours2$month=="Dec",] %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED in Dec 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

###############################################################################
##Getting missing values for V*I*PF and VA*PF
sockets_on_hours <- sockets_v_i %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(timeUse))
sockets_on_hours <- as.data.frame(sockets_on_hours)

sockets_on_hours2 <- data.frame()
for(i in 1:length(unique(sockets_on_hours$id))) {
  df <- sockets_on_hours[sockets_on_hours$id == unique(sockets_on_hours$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    sockets_on_hours2 <- rbind(sockets_on_hours2, df_sub)
  }
}
sockets_on_hours2 <- sockets_on_hours2[order(sockets_on_hours2$date), ]

sockets_on_hours2$month <- as.character(month(sockets_on_hours2$date,label=TRUE, 
                                              abbr=TRUE))
sockets_on_hours2$date <- as.Date(sockets_on_hours2$date)
sockets_on_hours2$id <- as.factor(sockets_on_hours2$id)
sockets_on_hours2$variable <- as.factor(sockets_on_hours2$variable)
sockets_on_hours2[sockets_on_hours2$month=="Jul",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Jul 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_on_hours2[sockets_on_hours2$month=="Aug",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Aug 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_on_hours2[sockets_on_hours2$month=="Sep",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Sep 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_on_hours2[sockets_on_hours2$month=="Oct",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Oct 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_on_hours2[sockets_on_hours2$month=="Nov",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Nov 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_on_hours2[sockets_on_hours2$month=="Dec",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket in Dec 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

##############################################################################
#Max hour on per day for each id and each variable
cpe_sub$time <- as.numeric(cpe_sub$timeUse)
cpe_max_time <- cpe_sub %>%
  group_by(id, date, variable) %>%
  summarise(lastHour = max(time))
cpe_max_time <- as.data.frame(cpe_max_time)
#Once we have hours active for each available day, look for missing dates
cpe_max_time2 <- data.frame()
for(i in 1:length(unique(cpe_max_time$id))) {
  df <- cpe_max_time[cpe_max_time$id == unique(cpe_max_time$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], lastHour=-1))
      }
    }
    cpe_max_time2 <- rbind(cpe_max_time2, df_sub)
  }
}
cpe_max_time2 <- cpe_max_time2[order(cpe_max_time2$date),]
cpe_max_time2$month <- as.character(month(cpe_max_time2$date,label=TRUE, abbr=TRUE))

cpe_max_time2$date <- as.Date(cpe_max_time2$date)
cpe_max_time2$id <- as.factor(cpe_max_time2$id)
cpe_max_time2$variable <- as.factor(cpe_max_time2$variable)
cpe_max_time2[cpe_max_time2$month=="Jul",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Jul 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_max_time2[cpe_max_time2$month=="Aug",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Aug 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_max_time2[cpe_max_time2$month=="Sep",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Sep 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_max_time2[cpe_max_time2$month=="Oct",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Oct 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_max_time2[cpe_max_time2$month=="Nov",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Nov 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

cpe_max_time2[cpe_max_time2$month=="Dec",] %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED in Dec 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1)) +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

##############################################################################
#Max hour on per day for each id and each variable
##Getting missing values for V*I*PF and VA*PF
sockets_v_i$time <- as.numeric(sockets_v_i$timeUse)
sockets_max_time <- sockets_v_i %>%
  group_by(id, date, variable) %>%
  summarise(onHours = max(time))
sockets_max_time <- as.data.frame(sockets_max_time)
#Once we have hours active for each available day, look for missing dates
sockets_max_time2 <- data.frame()
for(i in 1:length(unique(sockets_max_time$id))) {
  df <- sockets_max_time[sockets_max_time$id == unique(sockets_max_time$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=-1))
      }
    }
    sockets_max_time2 <- rbind(sockets_max_time2, df_sub)
  }
}
sockets_max_time2 <- sockets_max_time2[order(sockets_max_time2$date), ]
sockets_max_time2$month <- as.character(month(sockets_max_time2$date,label=TRUE, 
                                              abbr=TRUE))
sockets_max_time2$date <- as.Date(sockets_max_time2$date)
sockets_max_time2$id <- as.factor(sockets_max_time2$id)
sockets_max_time2$variable <- as.factor(sockets_max_time2$variable)
sockets_max_time2[sockets_max_time2$month=="Jul",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Jul 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_max_time2[sockets_max_time2$month=="Aug",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Aug 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_max_time2[sockets_max_time2$month=="Sep",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Sep 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_max_time2[sockets_max_time2$month=="Oct",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Oct 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_max_time2[sockets_max_time2$month=="Nov",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Nov 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

sockets_max_time2[sockets_max_time2$month=="Dec",] %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket in Dec 2019" ,
       y="Last hour of data collection",
       x = "Date" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day")

#############################################################################
#Calculate typical day values for instantaneous power each month ############
# Cpe_sub and sockets_sub have hourly means for hours available only ########
###### For each month, take hourly averages #################################

######### Typical day for CPE ##############################################
cpe_typical_day <- cpe_sub %>%
  group_by(month, id, variable, timeUse) %>%
  summarise(value=mean(value))
cpe_typical_day <- as.data.frame(cpe_typical_day)

setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe_typical_day, file="CPE_all_typical_day.csv", row.names=FALSE)

#Typical day values for CPE in July
cpe_typical_day[cpe_typical_day$month=="Jul",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Jul 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

cpe_typical_day[cpe_typical_day$month=="Aug",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable))  + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Aug 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

cpe_typical_day[cpe_typical_day$month=="Sep",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable))  + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Sep 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

cpe_typical_day[cpe_typical_day$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable))  + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Oct 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

cpe_typical_day[cpe_typical_day$month=="Nov",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable))  + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Nov 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

cpe_typical_day[cpe_typical_day$month=="Dec",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable))  + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical day power consumption of CPE in Dec 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="CPE", 
       shape="Parameter",
       linetype="Parameter")

############################################################################
######## Typical day for sockets - only the LVL values######################
sockets_typical_day <- sockets_sub[sockets_sub$variable=="vRELAY1_LVL",] %>%
  group_by(month, id, variable, timeUse) %>%
  summarise(value=mean(value))
sockets_typical_day <- as.data.frame(sockets_typical_day)

setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets_typical_day, file="Sockets_all_typical_day.csv", row.names=FALSE)

#Typical day values for sockets in July
sockets_typical_day[sockets_typical_day$month=="Jul",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Jul 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

sockets_typical_day[sockets_typical_day$month=="Aug",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Aug 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

sockets_typical_day[sockets_typical_day$month=="Sep",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Sep 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

sockets_typical_day[sockets_typical_day$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Oct 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

sockets_typical_day[sockets_typical_day$month=="Nov",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Nov 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

sockets_typical_day[sockets_typical_day$month=="Dec",] %>%
  ggplot(aes(as.numeric(timeUse), value, color=id)) + 
  geom_line(aes(linetype=variable)) +
  geom_point(aes(shape=variable)) + 
  labs(title="Typical day power consumption of sockets in Dec 2019" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Sockets", 
       shape="Parameter",
       linetype="Parameter")

############################################################################
### Adding missing data for hourly values using typical day profiles for each day
####### Adding missing cpe data #################################
cpe_full <- data.frame()
for(i in 1:length(unique(cpe_sub$id))) {
  df <- cpe_sub[cpe_sub$id == unique(cpe_sub$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      typ_data <- cpe_typical_day$value[cpe_typical_day$id==unique(cpe_sub$id)[i]
                  & cpe_typical_day$variable==unique(df$variable)[j]
                  & cpe_typical_day$month==as.character(month(all_days[k], 
                                                        label=TRUE, abbr=TRUE))]
      df2 <- df_sub[df_sub$date==all_days[k], ]
      for(l in 1:length(all_hours)) {
        if(!(all_hours[l] %in% df2$timeUse)) {
          df2 <- rbind(df2, 
                       data.frame(id=unique(cpe_sub$id)[i], date=all_days[k],
                       timeUse=all_hours[l], variable=unique(df$variable)[j],
                       value=typ_data[l], month=as.character(month(all_days[k], 
                                                                   label=TRUE, abbr=TRUE))))
        }
      }
      df2 <- df2[order(df2$timeUse),]
      cpe_full <- rbind(cpe_full, df2)
    }
  }
}
cpe_write <- cpe_full
cpe_write$id <- paste(cpe_full$id, cpe_full$variable) #Combine id and variable
cpe_write <- cpe_write[,-4] #Remove variable
cpe_write <- spread(cpe_write, id, value)
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe_write, file="CPE_all_full_hourly.csv", row.names=FALSE)

##### Adding missing sockets data - only for vRELAY1_LVL ###############################
sockets_sub <- sockets_sub[,7]
sockets_power <- sockets_sub[sockets_sub$variable=="vRELAY1_LVL",]
sockets_full <- data.frame()
for(i in 1:length(unique(sockets_power$id))) {
  df <- sockets_power[sockets_power$id == unique(sockets_power$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      typ_data <- sockets_typical_day$value[sockets_typical_day$id==unique(sockets_power$id)[i]
                  & sockets_typical_day$variable==unique(df$variable)[j]
                  & sockets_typical_day$month==as.character(month(all_days[k], 
                                               label=TRUE, abbr=TRUE))]
      df2 <- df_sub[df_sub$date==all_days[k], ]
      for(l in 1:length(all_hours)) {
        if(!(all_hours[l] %in% df2$timeUse)) {
          df2 <- rbind(df2, 
                       data.frame(id=unique(sockets_power$id)[i], date=all_days[k],
                                  timeUse=all_hours[l], variable=unique(df$variable)[j],
                                  value=typ_data[l], month=as.character(month(all_days[k], 
                                                                label=TRUE, abbr=TRUE))))
        }
      }
      df2 <- df2[order(df2$timeUse),]
      sockets_full <- rbind(sockets_full, df2)
    }
  }
}
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets_full, file="Sockets_all_full_hourly_power.csv", row.names=FALSE)

##############################################################################
#### Read in sockets_full and cpe_full files
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
cpe_full <- read.csv("CPE_all_full_hourly.csv", header = TRUE,
                     stringsAsFactors = FALSE)
cpe_full$date <- as.Date(cpe_full$date)
cpe_full$timeUse <- as.character(cpe_full$timeUse)

sockets_full <- read.csv("Sockets_all_full_hourly_power.csv", header=TRUE,
                         stringsAsFactors = FALSE)
sockets_full$date <- as.Date(sockets_full$date)
sockets_full$timeUse <- as.character(sockets_full$timeUse)

############################################################################
#Hourly total (lights and sockets), sockets, indoor lights and outdoor lights
#power consumption from the hall for weekdays in Jul 2019
########## Answering RQ1 without adding missing data #######################
#Function to calculate confidence interval
confidence_interval <- function(df) {
  confidence <- df %>%
    group_by(timeUse) %>%
    summarise(upper95=mean(value)+1.96*(sd(value)/sqrt(length(value))))
  return(confidence$upper95)
}

#Function to calculate hourly sum per day
hourly_sum <- function(df) {
  pc_sum <- df %>%
    group_by(date, timeUse) %>%
    summarise(value=sum(value))
  pc_sum <- as.data.frame(pc_sum)
  #Add days of week to separate weekends from weekdays
  pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
  #Add month to separate plots per month
  pc_sum$month <- as.character(month(pc_sum$date, label=TRUE, abbr=TRUE))
  return(pc_sum)
}

#Function to return weekdays
hourly_sum_weekdays <- function(df) {
  df <- df[df$wday!="Sat" & df$wday!="Sun", ]
  df$time <- as.factor(df$timeUse)
  return(df) 
}
#Function to return weekend days
hourly_sum_weekenddays <- function(df) {
  df <- df[df$wday=="Sat" | df$wday=="Sun", ]
  df$time <- as.factor(df$timeUse)
  return(df) 
}

############### A. Combine hourly power consumption data for CPE and sockets#####
pc <- data.frame()
pc <- rbind(pc, cpe_full, sockets_full)
pc <- pc[order(pc$date),]
#Total energy consumption per hour per day
pc_sum <- hourly_sum(pc)
#Separating weekdays from weekend days
pc_sum_wday <- hourly_sum_weekdays(pc_sum)
pc_sum_weday <- hourly_sum_weekenddays(pc_sum)

#Plotting total energy consumption per hour for weedkdays in Jul 2019
#Calculate confidence interval
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Jul",])
pc_sum_wday[pc_sum_wday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Jul'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 75))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Jul",])
pc_sum_weday[pc_sum_weday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Jul'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 75))

#Plotting total energy consumption per hour for weedkdays in Aug 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Aug",])
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Aug'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 60))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Aug",])
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Aug'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 60))

#Plotting total energy consumption per hour for weedkdays in Sep 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Sep",])
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Sep'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")   +
  scale_y_continuous(limits=c(0, 75))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Sep",])
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Sep'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 75))

#Plotting total energy consumption per hour for weedkdays in Oct 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Oct",])
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Oct'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 100))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Oct",])
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Oct'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0,100))

#Plotting total energy consumption per hour for weedkdays in Nov 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Nov",])
pc_sum_wday[pc_sum_wday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Nov'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 300))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Nov",])
pc_sum_weday[pc_sum_weday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Nov'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 300))

#Plotting total energy consumption per hour for weedkdays in Dec 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Dec",])
pc_sum_wday[pc_sum_wday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE and sockets at Hall on week days in Dec'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 500))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Dec",])
pc_sum_weday[pc_sum_weday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all CPE & sockets at Hall on weekend days in Dec'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 500))

#############################################################################
##Hourly socket consumption at the hall for a weekend/week day in (month) 
#Total energy consumption for all sockets per hour per day
pc_sum <- hourly_sum(sockets_full)
#Separating weekdays from weekend days
pc_sum_wday <- hourly_sum_weekdays(pc_sum)
pc_sum_weday <- hourly_sum_weekenddays(pc_sum)

#Plotting sockets energy consumption per hour for weedkdays in Jul 2019
#Calculate confidence interval
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Jul",])
pc_sum_wday[pc_sum_wday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Jul'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0,75))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Jul",])
pc_sum_weday[pc_sum_weday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Jul'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 75))

#Plotting total energy consumption per hour for weedkdays in Aug 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Aug",])
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Aug'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 60))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Aug",])
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Aug'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 60))

#Plotting total energy consumption per hour for weedkdays in Sep 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Sep",])
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Sep'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")   +
  scale_y_continuous(limits=c(0, 75))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Sep",])
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Sep'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")+
  scale_y_continuous(limits=c(0, 75))

#Plotting total energy consumption per hour for weedkdays in Oct 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Oct",])
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Oct'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 90))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Oct",])
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Oct'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")+
  scale_y_continuous(limits=c(0, 90))

#Plotting total energy consumption per hour for weedkdays in Nov 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Nov",])
pc_sum_wday[pc_sum_wday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Nov'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 250))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Nov",])
pc_sum_weday[pc_sum_weday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Nov'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")+
  scale_y_continuous(limits=c(0, 250))

#Plotting total energy consumption per hour for weedkdays in Dec 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Dec",])
pc_sum_wday[pc_sum_wday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on week days in Dec'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 500))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Dec",])
pc_sum_weday[pc_sum_weday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all sockets at Hall on weekend days in Dec'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")+
  scale_y_continuous(limits=c(0, 500))

################################################################################
##Hourly indoor light consumption at the hall for a weekend/week day in (month) 
#Total energy consumption per hour for all indoor lights per day
pc_sum <- hourly_sum(cpe_full[cpe_full$id!="CPE5",])
#Separating weekdays from weekend days
pc_sum_wday <- hourly_sum_weekdays(pc_sum)
pc_sum_weday <- hourly_sum_weekenddays(pc_sum)

#Plotting sockets energy consumption per hour for weedkdays in Jul 2019
#Calculate confidence interval
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Jul",])
pc_sum_wday[pc_sum_wday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Jul'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 20))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Jul",])
pc_sum_weday[pc_sum_weday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Jul'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 20))

#Plotting total energy consumption per hour for weedkdays in Aug 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Aug",])
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Aug'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 10))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Aug",])
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Aug'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 10))

#Plotting total energy consumption per hour for weedkdays in Sep 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Sep",])
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Sep'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")   +
  scale_y_continuous(limits=c(0, 20))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Sep",])
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Sep'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 20))

#Plotting total energy consumption per hour for weedkdays in Oct 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Oct",])
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Oct'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 12))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Oct",])
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Oct'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 12))

#Plotting total energy consumption per hour for weedkdays in Nov 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Nov",])
pc_sum_wday[pc_sum_wday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Nov'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 15))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Nov",])
pc_sum_weday[pc_sum_weday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Nov'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 15))

#Plotting total energy consumption per hour for weedkdays in Dec 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Dec",])
pc_sum_wday[pc_sum_wday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on week days in Dec'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0,12.5))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Dec",])
pc_sum_weday[pc_sum_weday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all indoor lights at Hall on weekend days in Dec'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 12.5))

#################################################################################
##Hourly outdoor light consumption at the hall for a weekend/week day in (month) 
#Total energy consumption per hour for all indoor lights per day
pc_sum <- hourly_sum(cpe_full[cpe_full$id=="CPE5",])
#Separating weekdays from weekend days
pc_sum_wday <- hourly_sum_weekdays(pc_sum)
pc_sum_weday <- hourly_sum_weekenddays(pc_sum)

#Plotting sockets energy consumption per hour for weedkdays in Jul 2019
#Calculate confidence interval
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Jul",])
pc_sum_wday[pc_sum_wday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Jul'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 2))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Jul",])
pc_sum_weday[pc_sum_weday$month=="Jul",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Jul'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 2))

#Plotting total energy consumption per hour for weedkdays in Aug 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Aug",])
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Aug'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 12.5))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Aug",])
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Aug'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 12.5))

#Plotting total energy consumption per hour for weedkdays in Sep 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Sep",])
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Sep'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none")   +
  scale_y_continuous(limits=c(0, 11.5))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Sep",])
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Sep'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 11.5))

#Plotting total energy consumption per hour for weedkdays in Oct 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Oct",])
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Oct'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 12))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Oct",])
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Oct'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 12))

#Plotting total energy consumption per hour for weedkdays in Nov 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Nov",])
pc_sum_wday[pc_sum_wday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Nov'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") +
  scale_y_continuous(limits=c(0, 11))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Nov",])
pc_sum_weday[pc_sum_weday$month=="Nov",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Nov'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 11))

#Plotting total energy consumption per hour for weedkdays in Dec 2019
confidence <- confidence_interval(pc_sum_wday[pc_sum_wday$month=="Dec",])
pc_sum_wday[pc_sum_wday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on week days in Dec'19 (95% CI)" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none") +
  scale_y_continuous(limits=c(0, 12.5))

confidence <- confidence_interval(pc_sum_weday[pc_sum_weday$month=="Dec",])
pc_sum_weday[pc_sum_weday$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title="Mean hourly power consumed by all outdoor lights at Hall on weekend days in Dec'19 (95% CI)" ,
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=9), legend.position = "none")+
  scale_y_continuous(limits=c(0, 12.5))

################################################################################
#################### RQ 2 ###################################################
#Daily total energy consumption of Hall per month and total energy consumption since commissioning.
daily_sum <- function(df) {
  daily <- df %>%
    group_by(date) %>%
    summarise(value=sum(value))
  daily <- as.data.frame(daily)
  return(daily)
}

#Combine daily power consumed by all CPE 
cpe_total <- daily_sum(cpe_full) #sum of mean power consumed per hour for 24 hrs (Wh)
cpe_total$id <- rep("CPE", length(cpe_total$date))

#Socket energy consumption - for each day get max value
s_total <- daily_sum(sockets_full)
s_total$id <- rep("S", length(s_total$date))

#Total energy consumed per day by both lights and sockets
cpe_s_daily <- data.frame() 
cpe_s_daily <- rbind(cpe_s_daily, cpe_total, s_total)
cpe_s_daily <- cpe_s_daily[order(cpe_s_daily$date), ]
energy_total <- cpe_s_daily %>%
  group_by(date) %>%
  summarise(value=sum(value))

energy_total$month <- as.character(month(energy_total$date, label = TRUE, 
                                         abbr=TRUE))
startDate <- as.Date("2019-07-18")
energy_total$days <- as.numeric(energy_total$date - startDate)

#Total daily consumption in Jul 2019
energy_total[energy_total$month=="Jul",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Jul'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total daily consumption in Aug 2019
energy_total[energy_total$month=="Aug",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Aug'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total daily consumption in Sep 2019
energy_total[energy_total$month=="Sep",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Sep'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total daily consumption in Oct 2019
energy_total[energy_total$month=="Oct",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Oct'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total daily consumption in Nov 2019
energy_total[energy_total$month=="Nov",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Nov'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total daily consumption in Dec 2019
energy_total[energy_total$month=="Dec",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Total energy consumed by lights and sockets per day in Dec'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

#Total energy consumption since commissioning
energy_total %>%
  ggplot(aes(x=days, y=value, color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Total energy consumed by lights and sockets since commissioning (2019-07-18) " , 
       y="Total energy consumption (Wh)",
       x = "Days since commissioning",
       colour="") + 
  theme(plot.title = element_text(size=11),legend.position = "none") +
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180),
                     labels=c("0","20","40","60","80","100","120","140","160","180"))

#Getting daily average per month
energy_month <- energy_total %>%
  group_by(month) %>%
  summarise(value=mean(value))
energy_month$id <- rep("Actual", length(energy_month$month))
energy_month$month2 <- factor(energy_month$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                              labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
energy_month$id <- as.factor(energy_month$id)
energy_month %>%
  ggplot(aes(x=month2, y=value, fill=id)) +
  geom_bar(stat="identity", width=.5, position="dodge") + 
  labs(title="Average daily energy consumed by lights and sockets at Hall for each month" , 
       y="Average daily energy consumption (Wh)",
       x = "Month", 
       fill="") + 
  theme(plot.title = element_text(size=11),legend.position = "none")

#########################################################################
##### Daily total user consumption profile for the hall in (month).######
daily_sum_user <- function(df) {
  daily <- df %>%
    group_by(date, id) %>%
    summarise(value=sum(value))
  daily <- as.data.frame(daily)
  return(daily)
}

#Combine daily power consumed per day by each cpe
cpe_total <- daily_sum_user(cpe_full) #sum of mean power consumed per hour for 24 hrs (Wh)

#Outdoor lights per day
cpe_outdoor <- cpe_total[cpe_total$id=="CPE5",]
cpe_outdoor$id <- rep("Outdoor lights", length(cpe_outdoor$date))

#Indoor lights per day
cpe_indoor <- cpe_total[cpe_total$id!="CPE5",] %>%
  group_by(date) %>%
  summarise(value=sum(value))
cpe_indoor$id <- rep("Indoor lights", length(cpe_indoor$date))
cpe_indoor <- data.frame("date"=cpe_indoor$date, "id"=cpe_indoor$id, 
                         "value"=cpe_indoor$value)
cpe_indoor$id <- as.character(cpe_indoor$id)

#Socket energy consumption - for each socket per day
s_total <- daily_sum_user(sockets_full)

### Combine indoor and outdoor lights with sockets data
all_users <- data.frame()
all_users <- rbind(all_users, cpe_outdoor, cpe_indoor, s_total)
all_users <- all_users[order(all_users$date), ]
all_users$month <- as.character(month(all_users$date, label=TRUE, abbr=TRUE))

##Daily energy consumption for each user in Jul
all_users[all_users$month=="Jul", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Jul'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

##Daily energy consumption for each user in Aug
all_users[all_users$month=="Aug", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Aug'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

##Daily energy consumption for each user in Sep
all_users[all_users$month=="Sep", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Sep'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

##Daily energy consumption for each user in Oct
all_users[all_users$month=="Oct", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Oct'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

##Daily energy consumption for each user in Nov
all_users[all_users$month=="Nov", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Nov'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

##Daily energy consumption for each user in Dec
all_users[all_users$month=="Dec", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user energy consumption at the hall in Dec'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

###############################################################################
############################ Read in system data #############################
#Extract values AC Consumption L1 (W), PV Power (W), State of charge (%), 
#Discharged Energy (kWh), Charged Energy (kWh) 

#One file under full data from June to Nov - europe time zone
#filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/System data/Full data/June to Dec/HEEDNyabihekeHall_log_20190531-2300_to_20200101-0000.csv"
filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/System data/Full data/June to Nov/HEEDNyabihekeHall_log_20190531-2300_to_20191130-1033.csv"
#Concatenate the headers spread across first 3 rows
headers <- read_csv(filename, col_names = FALSE, na="..", n_max = 3)
#Replace NA in header with "" for missing row 3 values
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))

#Read data without the first three rows
system_data <- read_csv(filename, col_names = headers, na="..", skip = 3)

#Replace NA in data frame with "" for missing values as in raw file
system_data[is.na(system_data)] <- ""

#Extract the columns we require
#Extract system overview data for AC Consumption L1 - W
#Find all column names with System overview
columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
#Find all column names with AC Consumption L1
colNames <- columns[which(grepl("AC Consumption L1", columns, fixed=TRUE))]
colNames <- c(headers[1], colNames) 
sysOverview <- system_data[,colNames]

#Extract Solar Charger data to get PV power - W
#Find all column names with Solar Charger
columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
#Find all column names with PV power
colNames <- columns[which(grepl("PV power", columns, fixed=TRUE))]
colNames <- c(headers[1], colNames) 
solarCharger <- system_data[,colNames]

#Extract Battery Monitor data to get State of charge (%), Discharged Energy(kWh), Charged Energy(kWh)
#Find all column names with Solar Charger
columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
#Find all column names with State of charge
colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
#Find all column names with Discharged Energy
colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
#Find all column names with Charged Energy
colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
colNames <- c(headers[1], colNames) 
batteryMonitor <- system_data[,colNames]

systemData <- data.frame()
systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
systemData$`System overview [0] AC Consumption L1 W` <- 
  as.numeric(systemData$`System overview [0] AC Consumption L1 W`)
systemData$`Solar Charger [260] PV power ` <- 
  as.numeric(systemData$`Solar Charger [260] PV power `)
systemData$`Battery Monitor [258] Discharged Energy kWh` <- 
  as.numeric(systemData$`Battery Monitor [258] Discharged Energy kWh`)
systemData$`Battery Monitor [258] Charged Energy kWh` <- 
  as.numeric(systemData$`Battery Monitor [258] Charged Energy kWh`) 
#Converting time in Rwandan time zone by adding an hour
colnames(systemData) <- c("timestamp","AC_consumption_W","PV_power_W",
                          "State_of_charge","Discharged_energy_kWh",
                          "Charged_energy_kWh")
#Trim time from 19 July to Dec and convert in Rwandan time
systemData <- systemData[systemData$timestamp>="2019-07-19 00:00:01", ]
systemData$timestamp <- systemData$timestamp + 1*60*60
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(systemData, file="systemData_june_nov.csv", row.names=FALSE)

######Analyse data to get hourly means #######################################
system_gather <- gather(systemData, "id", "value", 2:6)
system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
system_gather$date <- date(system_gather$timestamp)
  
system_hourly_mean <- system_gather %>%
  group_by(date,timeUse,id) %>%
  summarise(value=mean(value,na.rm = TRUE))
system_hourly_mean <- as.data.frame(system_hourly_mean)
system_hourly_mean$month <- as.character(month(system_hourly_mean$date, abbr=TRUE,
                                               label=TRUE))

####Look for NA values for any variable except charged and discharged energy
system_hourly_mean_spread <- spread(system_hourly_mean, 
                                    id, value)
any(is.na(system_hourly_mean_spread$AC_consumption_W)==TRUE)
any(is.na(system_hourly_mean_spread$Charged_energy_kWh)==TRUE)
any(is.na(system_hourly_mean_spread$Discharged_energy_kWh)==TRUE)
any(is.na(system_hourly_mean_spread$PV_power_W)==TRUE)
any(is.na(system_hourly_mean_spread$State_of_charge)==TRUE)
#Only found in charged and discharged energy - to be replaced by 0
system_hourly_mean[is.na(system_hourly_mean)] <- 0

#########Look for missing hours########################################
#Number of hours on per day for each id and each variable
system_on_hours <- system_hourly_mean %>%
  group_by(id, date) %>%
  summarise(onHours = length(timeUse))
system_on_hours <- as.data.frame(system_on_hours)

#Once we have hours active for each available day, look for missing dates
system_on_hours2 <- data.frame()
for(i in 1:length(unique(system_on_hours$id))) {
  df_sub <- system_on_hours[system_on_hours$id == unique(system_on_hours$id)[i], ]
  for(k in 1:length(all_days)) {
    if(!(all_days[k] %in% df_sub$date)) {
      df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                         onHours=0))
    }
  }
  system_on_hours2 <- rbind(system_on_hours2, df_sub)
}
system_on_hours2 <- system_on_hours2[order(system_on_hours2$date),]
system_on_hours2$month <- as.character(month(system_on_hours2$date,
                                             label=TRUE, abbr=TRUE))

system_on_hours2$date <- as.Date(system_on_hours2$date)
system_on_hours2$id <- as.factor(system_on_hours2$id)
system_on_hours2[system_on_hours2$month=="Jul",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Jul 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

system_on_hours2[system_on_hours2$month=="Aug",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Aug 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

system_on_hours2[system_on_hours2$month=="Sep",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Sep 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

system_on_hours2[system_on_hours2$month=="Oct",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Oct 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

system_on_hours2[system_on_hours2$month=="Nov",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Nov 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")
#30th Nov is missing some data - all the rest is fine.

system_on_hours2[system_on_hours2$month=="Dec",] %>%
  ggplot(aes(date, onHours, color=id)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4")) +
  labs(title="Hours of data collection per day for each variable in Dec 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Variable") +
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

###############################################################################
##### User and system daily consumption per month
#Auxiliary load is AC Consumption L1 - all socket and CPE consumption values

#Daily per user consumption is stored in all_users
#Calculate total user consumption
sum_users <- all_users %>%
  group_by(month, date) %>%
  summarise(value = sum(value))
sum_users <- as.data.frame(sum_users)
sum_users <- sum_users[order(sum_users$date),]
sum_users$id <- rep("User", length(sum_users$month))

#Using system_hourly_mean to calculate the daily AC consumption over 24 hours
system_sub <- system_hourly_mean[system_hourly_mean$id=="AC_consumption_W",]
#daily AC consumption values
sum_ac <- system_sub %>%
  group_by(month, date) %>%
  summarise(value = sum(value))
sum_ac <- as.data.frame(sum_ac)
sum_ac$id <- rep("AC", length(sum_ac$month))

#Combine user and AC data to calculate aux value
sum_users_ac <- data.frame()
sum_users_ac <- rbind(sum_users_ac, sum_users, sum_ac)
sum_users_ac <- spread(sum_users_ac, id, value, fill=NA)

#Subset data to include from 19th July to 29th Nov - 30th Nov is incomplete
sum_users_ac <- sum_users_ac[sum_users_ac$date>="2019-07-19" & 
                               sum_users_ac$date<"2019-11-30", ]
sum_users_ac <- sum_users_ac[order(sum_users_ac$date), ]
sum_users_ac$Aux <- sum_users_ac$AC - sum_users_ac$User
sum_users_ac_gather <- gather(sum_users_ac, "id", "value", 3:5)
daily_aux <- sum_users_ac_gather[sum_users_ac_gather$id=="Aux", ]

#Subset all users to 19th July to 29th Nov 
all_users_sub <- all_users[all_users$date<"2019-11-30", ]
all_users_sub <- data.frame("month"=all_users_sub$month, 
                            "date"=all_users_sub$date,
                            "id"=all_users_sub$id,
                            "value"=all_users_sub$value)

#Plot daily values for user and system consumption per month
daily_users_system <- data.frame()
daily_users_system <- rbind(daily_users_system, all_users_sub, daily_aux)
daily_users_system$id <- as.character(daily_users_system$id)
daily_users_system <- daily_users_system[order(daily_users_system$date), ]

daily_users_system[daily_users_system$month=="Jul", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Jul'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

daily_users_system[daily_users_system$month=="Aug", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Aug'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

daily_users_system[daily_users_system$month=="Sep", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Sep'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

daily_users_system[daily_users_system$month=="Oct", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Oct'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

daily_users_system[daily_users_system$month=="Nov", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Nov'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

daily_users_system[daily_users_system$month=="Dec", ] %>%
  ggplot(aes(x = date, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system energy consumption at the hall in Dec'19" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_date(date_minor_breaks = "1 day")

####Monthly user profile data
monthly_profile <- daily_users_system %>%
  group_by(month, id) %>%
  summarise(value=sum(value))
monthly_profile <- as.data.frame(monthly_profile) 
monthly_profile$month2 <- factor(monthly_profile$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov"),
                              labels = c("Jul","Aug","Sep","Oct","Nov"))
monthly_profile %>%
  ggplot(aes(x = month2, y= value/1000.0, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Total monthly energy consumption values at Hall in 2019" , 
       y="Total monthly energy consumption (kWh)",
       x = "Month of study") 

############################################################################
#Reading in weather data - hourly data is stored
filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/Weather data/weather_data_dec.csv"
#headers <- read_excel(filename, col_names = FALSE, na="..", n_max = 2)
#Replace NA in header with "" for missing row 3 values
#headers[is.na(headers)] <- ""
#column_labels <- headers %>% summarize_all(str_c, collapse = " ")
#headers = unname(unlist(column_labels[1,]))
#weather_data <- read_excel(filename, col_names = headers, na="..", skip = 2,
#                           col_types=c("date","numeric","numeric"))
weather_data <- read.csv(filename, header=TRUE, as.is=TRUE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, 
                                     format = "%Y-%m-%d %H:%M")
##Data is 12 years behind - change year to 2019
weather_data$timestamp <- weather_data$timestamp %m+% years(12)
#Replace NA in data frame with "" for missing values as in raw file
#weather_data[is.na(weather_data)] <- ""
#Data is 12 years behind - change year to 2019
#year(weather_data$`Time (2019!) `) <- 2019

###########################################################################
################ Typical day profile for all system variables ##########
#Typical day profile is mean of all hourly values across a month
#Pot PV power output (kW/m2) - multiply by area (13.094m2) and efficiency (15.6%)
pot_pv_hourly <- weather_data[,-2]
colnames(pot_pv_hourly) <- c("timestamp", "value")
pot_pv_hourly$timeUse <- format(pot_pv_hourly$timestamp, format='%H') 
pot_pv_hourly$date <- date(pot_pv_hourly$timestamp)
#Select data from 19th July
pot_pv_hourly <- pot_pv_hourly[pot_pv_hourly$date>="2019-07-19", ]
pot_pv_hourly <- na.omit(pot_pv_hourly)
pot_pv_hourly <- data.frame("timestamp"=pot_pv_hourly$timestamp,
                             "value"=pot_pv_hourly$value,
                             "timeUse"=pot_pv_hourly$timeUse,
                             "date"=pot_pv_hourly$date)
pot_pv_hourly$timeUse <- as.character(pot_pv_hourly$timeUse)
pot_pv_hourly$value <- pot_pv_hourly$value * 13.094 * 0.156 #in kW
pot_pv_hourly$month <- as.character(month(pot_pv_hourly$date,label=TRUE,abbr=TRUE))
pot_pv_hourly$id <- rep("Pot_PV_Power", length(pot_pv_hourly$timestamp))
pot_pv_hourly <- pot_pv_hourly[,-1]
pot_pv_hourly <- data.frame("date"=pot_pv_hourly$date,
                            "timeUse"=pot_pv_hourly$timeUse,
                            "id"=pot_pv_hourly$id,
                            "value"=pot_pv_hourly$value,
                            "month"=pot_pv_hourly$month)
pot_pv_hourly$timeUse <- as.character(pot_pv_hourly$timeUse)
pot_pv_hourly$id <- as.character(pot_pv_hourly$id)
pot_pv_hourly$month <- as.character(pot_pv_hourly$month)
#Combining all variables
all_system_data_hourly <- data.frame()
all_system_data_hourly <- rbind(all_system_data_hourly, system_hourly_mean,
                                pot_pv_hourly)
all_system_data_hourly <- all_system_data_hourly[order(all_system_data_hourly$date),]
all_system_data_hourly <- spread(all_system_data_hourly, id, value)
#Subset till 29th Nov
all_system_data_hourly <- all_system_data_hourly[all_system_data_hourly$date<"2019-11-30",]
##System hourly mean
#Act. PV Power Output: [PV Power] (W) - convert to kW
all_system_data_hourly$PV_power_W <- all_system_data_hourly$PV_power_W / 1000.0
#AC load, E: [AC consumption L1] (W) - convert to kW
all_system_data_hourly$AC_consumption_W <- all_system_data_hourly$AC_consumption_W / 1000.0
#Capture losses, Lc = Pot. PV power output – Act. PV power output 
all_system_data_hourly$losses <- all_system_data_hourly$Pot_PV_Power - 
  all_system_data_hourly$PV_power_W

#Battery charge and discharge energy needs to computed again for each hour
#This is taken by taking differences from original data
# Battery Charge Power: The [Charged Energy] column gives this value in kWh.
#You can convert to kW and then take a mean for the hour.
battery_charge <- systemData[,c(1,6)]
colnames(battery_charge) <- c("timestamp", "Charged_energy")
#Calculate hourly mean
battery_charge$timestamp <- strptime(battery_charge$timestamp, "%Y-%m-%d %H:%M:%S")
battery_charge$date <- as.Date(format(battery_charge$timestamp, format='%Y-%m-%d'))
battery_charge$time <- format(battery_charge$timestamp, format='%H')
#The battery charge values are missing in the morning and evening
#Some values in between are also missing in the middle.
#We remove the NA values
battery_charge <- na.omit(battery_charge)
#Extract hourly values by taking the max value each hour 
battery_charge_hours <- battery_charge %>%
  group_by(date, time) %>%
  summarise(value = Charged_energy[length(Charged_energy)])
battery_charge_hours <- as.data.frame(battery_charge_hours)
#Converting kWh to kW per hour
#battery_charge_hours$value <- battery_charge_hours$value / 60.0
a <- diff(battery_charge_hours$value)
battery_charge_hours <- battery_charge_hours[-1,]
battery_charge_hours$value <- a
battery_charge_hours$month <- as.character(month(battery_charge_hours$date, label=TRUE, abbr=TRUE))
battery_charge_hours$id <- rep("Charged_energy_kWh", length(battery_charge_hours$date))
battery_charge_hours <- data.frame("date"=battery_charge_hours$date,
                                   "timeUse"=battery_charge_hours$time,
                                   "month"=battery_charge_hours$month,
                                   "id"=battery_charge_hours$id,
                                   "value"=battery_charge_hours$value, 
                                   stringsAsFactors = FALSE)

###Calculating discharged battery energy
# Battery Discharge Power: The [Discharge Energy] column gives this value in
#kWh. You can convert to kW and then take a mean for the hour.
battery_discharge <- systemData[,c(1,5)]
colnames(battery_discharge) <- c("timestamp", "Discharged_energy")
#Calculate hourly mean
battery_discharge$timestamp <- strptime(battery_discharge$timestamp, "%Y-%m-%d %H:%M:%S")
battery_discharge$date <- as.Date(format(battery_discharge$timestamp, format='%Y-%m-%d'))
battery_discharge$time <- format(battery_discharge$timestamp, format='%H')
#The battery discharge values are missing in the morning and evening
#Some values in between are also missing in the middle.
#We remove the NA values
battery_discharge <- na.omit(battery_discharge)
#Extract hourly values by taking the max value each hour 
battery_discharge_hours <- battery_discharge %>%
  group_by(date, time) %>%
  summarise(value = Discharged_energy[length(Discharged_energy)])
battery_discharge_hours <- as.data.frame(battery_discharge_hours)
#Converting kWh to kW per hour
#battery_charge_hours$value <- battery_charge_hours$value / 60.0
a <- diff(battery_discharge_hours$value)
battery_discharge_hours <- battery_discharge_hours[-1,]
battery_discharge_hours$value <- a
battery_discharge_hours$month <- as.character(month(battery_discharge_hours$date, label=TRUE, abbr=TRUE))
battery_discharge_hours$id <- rep("Discharged_energy_kWh", length(battery_discharge_hours$date))
battery_discharge_hours <- data.frame("date"=battery_discharge_hours$date,
                                      "timeUse"=battery_discharge_hours$time,
                                      "month"=battery_discharge_hours$month,
                                      "id"=battery_discharge_hours$id,
                                      "value"=battery_discharge_hours$value,
                                      stringsAsFactors = FALSE)

#####Calculating typical day for each month
all_system <- gather(all_system_data_hourly, "id", "value", 4:10)
all_system <- all_system[all_system$id!="Charged_energy_kWh", ]
all_system <- all_system[all_system$id!="Discharged_energy_kWh", ]
all_system <- rbind(all_system, battery_charge_hours)
all_system <- rbind(all_system, battery_discharge_hours)
typical_system_data <- all_system %>%
  group_by(month, timeUse, id) %>%
  summarise(value = mean(value))
typical_system_data <- as.data.frame(typical_system_data)
typical_system_data <- typical_system_data[order(typical_system_data$month),]
typical_system_data <- spread(typical_system_data, id, value)
typical_system_data[is.na(typical_system_data)] = 0
###Plotting data for a typical day in July
#Plotting all variables for Aug 2019
typical_system_data$timeUse <- as.numeric(typical_system_data$timeUse)
typical_system_data[typical_system_data$month=="Jul", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = AC_consumption_W, color = "AC Load", group="AC Load"), linetype=1) + 
  geom_point(aes(y = AC_consumption_W, color = "AC Load"), shape=1) + 
  geom_line(aes(y = PV_power_W, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
  geom_point(aes(y = PV_power_W, color = "Actual PV Power"), shape=2) + 
  geom_line(aes(y = losses, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = losses, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = Charged_energy_kWh, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = Charged_energy_kWh, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = Discharged_energy_kWh, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = Discharged_energy_kWh, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = Pot_PV_Power, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
  geom_point(aes(y = Pot_PV_Power, color = "Pot. PV Power"), shape=6) + 
  geom_line(aes(y = State_of_charge/100.0, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = State_of_charge/100.0, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "State of charge (%)")) +
  labs(title="Actual Hall power profile for a typical day in Jul 2019" , 
       y="Energy (kWh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

typical_system_data[typical_system_data$month=="Aug", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = AC_consumption_W, color = "AC Load", group="AC Load"), linetype=1) + 
  geom_point(aes(y = AC_consumption_W, color = "AC Load"), shape=1) + 
  geom_line(aes(y = PV_power_W, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
  geom_point(aes(y = PV_power_W, color = "Actual PV Power"), shape=2) + 
  geom_line(aes(y = losses, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = losses, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = Charged_energy_kWh, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = Charged_energy_kWh, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = Discharged_energy_kWh, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = Discharged_energy_kWh, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = Pot_PV_Power, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
  geom_point(aes(y = Pot_PV_Power, color = "Pot. PV Power"), shape=6) + 
  geom_line(aes(y = State_of_charge/100.0, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = State_of_charge/100.0, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "State of charge (%)")) +
  labs(title="Actual Hall power profile for a typical day in Aug 2019" , 
       y="Energy (kWh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

typical_system_data[typical_system_data$month=="Sep", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = AC_consumption_W, color = "AC Load", group="AC Load"), linetype=1) + 
  geom_point(aes(y = AC_consumption_W, color = "AC Load"), shape=1) + 
  geom_line(aes(y = PV_power_W, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
  geom_point(aes(y = PV_power_W, color = "Actual PV Power"), shape=2) + 
  geom_line(aes(y = losses, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = losses, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = Charged_energy_kWh, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = Charged_energy_kWh, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = Discharged_energy_kWh, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = Discharged_energy_kWh, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = Pot_PV_Power, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
  geom_point(aes(y = Pot_PV_Power, color = "Pot. PV Power"), shape=6) + 
  geom_line(aes(y = State_of_charge/100.0, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = State_of_charge/100.0, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "State of charge (%)")) +
  labs(title="Actual Hall power profile for a typical day in Sep 2019" , 
       y="Energy (kWh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

typical_system_data[typical_system_data$month=="Oct", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = AC_consumption_W, color = "AC Load", group="AC Load"), linetype=1) + 
  geom_point(aes(y = AC_consumption_W, color = "AC Load"), shape=1) + 
  geom_line(aes(y = PV_power_W, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
  geom_point(aes(y = PV_power_W, color = "Actual PV Power"), shape=2) + 
  geom_line(aes(y = losses, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = losses, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = Charged_energy_kWh, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = Charged_energy_kWh, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = Discharged_energy_kWh, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = Discharged_energy_kWh, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = Pot_PV_Power, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
  geom_point(aes(y = Pot_PV_Power, color = "Pot. PV Power"), shape=6) + 
  geom_line(aes(y = State_of_charge/100.0, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = State_of_charge/100.0, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "State of charge (%)")) +
  labs(title="Actual Hall power profile for a typical day in Oct 2019" , 
       y="Energy (kWh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

typical_system_data[typical_system_data$month=="Nov", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = AC_consumption_W, color = "AC Load", group="AC Load"), linetype=1) + 
  geom_point(aes(y = AC_consumption_W, color = "AC Load"), shape=1) + 
  geom_line(aes(y = PV_power_W, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
  geom_point(aes(y = PV_power_W, color = "Actual PV Power"), shape=2) + 
  geom_line(aes(y = losses, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = losses, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = Charged_energy_kWh, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = Charged_energy_kWh, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = Discharged_energy_kWh, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = Discharged_energy_kWh, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = Pot_PV_Power, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
  geom_point(aes(y = Pot_PV_Power, color = "Pot. PV Power"), shape=6) + 
  geom_line(aes(y = State_of_charge/100.0, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = State_of_charge/100.0, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "State of charge (%)")) +
  labs(title="Actual Hall power profile for a typical day in Nov 2019" , 
       y="Energy (kWh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

#########################################################################
#### Daily capture losses : Pot PV power (kWh) - Actual PV power (kWh)
#pot_pv_hourly has hourly means of Pot PV Power in kWh
#Get hourly means of PV power in kWh
act_pv_hourly <- system_hourly_mean[system_hourly_mean$id=="PV_power_W", ]
act_pv_hourly$value <- act_pv_hourly$value / 1000.0 #kWh
#Get data till 29th Nov
act_pv_hourly <- act_pv_hourly[act_pv_hourly$date<"2019-11-30", ]
pot_pv_hourly <- pot_pv_hourly[pot_pv_hourly$date<"2019-11-30", ]
losses_hourly <- data.frame()
losses_hourly <- rbind(losses_hourly, pot_pv_hourly, act_pv_hourly)
losses_hourly <- spread(losses_hourly, id, value)
losses_hourly$CL <- losses_hourly$Pot_PV_Power - losses_hourly$PV_power_W
losses_hourly <- gather(losses_hourly, "id", "value", 4:6)
#Calculating daily values
losses_daily <- losses_hourly %>%
  group_by(month, date, id) %>%
  summarise(value = sum(value))
losses_daily <- as.data.frame(losses_daily)
losses_daily$month2 <- factor(losses_daily$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov"),
                              labels = c("Jul","Aug","Sep","Oct","Nov"))
losses_daily[losses_daily$id=="CL",] %>%
  ggplot(aes(x=month2, y=value)) + 
  geom_errorbar(aes(ymin=min(value),ymax=max(value)),linetype = 1,width = 0.5) +
  geom_boxplot(fill="lightblue")  +
  labs(title="Excess Electrical Production Monthly Averages in 2019" , 
       y="Total daily capture loss (kWh) ",
       x = "Month")

########################################################################
####### Monthly utilised PV yield, PV capture losses and system losses
#Monthly capture losses - contains capture losses, pot pv power and act pv power
losses_monthly <- losses_daily %>%
  group_by(month, id) %>%
  summarise(value=sum(value))
losses_monthly <- as.data.frame(losses_monthly)

#Utilised PV yield, E, is calculated from the AC load consumption 
#[AC consumption L1], which needs to be converted into kWh 
#Get hourly means of AC consumption and convert in kWh
ac_load_hourly <- system_hourly_mean[system_hourly_mean$id=="AC_consumption_W", ]
ac_load_hourly$value <- ac_load_hourly$value / 1000.0 #kWh
#Get data till 29th Nov
ac_load_hourly <- ac_load_hourly[ac_load_hourly$date<"2019-11-30", ]

#Get daily values 
ac_load_daily <- ac_load_hourly %>%
  group_by(month, date, id) %>%
  summarise(value = sum(value))
ac_load_daily <- as.data.frame(ac_load_daily)

#Get monthly values
ac_load_monthly <- ac_load_daily %>%
  group_by(month, id) %>%
  summarise(value=sum(value))
ac_load_monthly <- as.data.frame(ac_load_monthly)

#Actual PV output (kWh for the same period) - utilised PV yield gives the 
#system losses, Ls (kWh). 
#Combine losses_monthly and ac_load_monthly
system_yield <- data.frame()
system_yield <- rbind(system_yield, losses_monthly, ac_load_monthly)
system_yield <- spread(system_yield, id, value)
system_yield$SL <- system_yield$PV_power_W - system_yield$AC_consumption_W
colnames(system_yield) <- c("month", "Utilised PV yield", "Capture Losses",
                            "Pot_PV_Power","PV_power_W","System Losses")
system_yield <- gather(system_yield, "id", "value", 2:6)
system_yield$month2 <- factor(system_yield$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov"),
                              labels = c("Jul","Aug","Sep","Oct","Nov"))
system_yield[system_yield$id!="Pot_PV_Power" & system_yield$id!="PV_power_W", ] %>%
  ggplot(aes(x = month2, y= value, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Total monthly electrical energy values at Nyabiheke hall in 2019" ,
       y="Consumed and potential electrical energy (kWh)",
       x = "Month",
       fill="Parameter") 
