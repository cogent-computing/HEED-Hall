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
                     df$variable=="AC_Day_Energy_session", ]
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
    summarise(value = mean(value),
              valueRaw = mean(value))
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
  df4 <- df3_clean %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(valueRaw = value[length(value)])
  df3 <- as.data.frame(df3)
  df4 <- as.data.frame(df4)
  df3$valueRaw <- df4$valueRaw
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

###########################################################################
##Getting missing values for socket data
sockets_on_hours <- sockets_sub %>%
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
sockets_sub$time <- as.numeric(sockets_sub$timeUse)
sockets_max_time <- sockets_sub %>%
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
cpe_sub <- cpe_sub[,-7]
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
cpe_write$time <- as.POSIXct(paste(paste(cpe_write$date, 
                                         cpe_write$timeUse), ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe_write, file="CPE_all_full_hourly.csv", row.names=FALSE)

##### Adding missing sockets data - only for vRELAY1_LVL ###############################
sockets_sub <- sockets_sub[,-8]
sockets_power <- sockets_sub[sockets_sub$variable=="vRELAY1_LVL",]
sockets_power <- sockets_power[,-6]
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

## Adding missing sockets data - only for AC Day Energy session #################
sockets_power <- sockets_sub[sockets_sub$variable=="AC_Day_Energy_session",]
#First we need to get typical day proportions of energy consumption for each hour
#This is calculated for each socket for each month
#Use subset of data where days are complete
sockets_power_sub <- data.frame()
for(i in 1:length(unique(sockets_power$id))) {
  df <- sockets_power[sockets_power$id==unique(sockets_power$id)[i], ]
  for(j in 1:length(unique(df$date))) {
    df_sub <- df[df$date==unique(df$date)[j], ]
    if(length(df_sub$id)==24) {
      sockets_power_sub <- rbind(sockets_power_sub, df_sub)
    }
  }
}

#Calculate diff for energy consumed per hour for each day
sockets_power_sub <- sockets_power_sub %>%
  group_by(month, id, date) %>%
  mutate(diff = c(value[1], diff(value)))
sockets_power_sub <- as.data.frame(sockets_power_sub)
#Calculate proportion of energy consumed per hour for each day
sockets_power_sub <- sockets_power_sub %>%
  group_by(month, id, date) %>%
  mutate(proportion = diff/value[length(value)])
sockets_power_sub <- as.data.frame(sockets_power_sub)
#Find typical day for each id for each month
typical_energy_session <- sockets_power_sub %>%
  group_by(month, id, variable,timeUse) %>%
  summarise(proportion = mean(proportion,na.rm=TRUE))
typical_energy_session <- as.data.frame(typical_energy_session)

#Once we find typical day data for each id/month, we fill in missing values 
sockets_power$time <- as.POSIXct(paste(paste(sockets_power$date, 
                      sockets_power$timeUse), ":00:01",sep=""),
                      format="%Y-%m-%d %H:%M:%S", tz="GMT")
sockets_full2 <- data.frame()
for(i in 1:length(unique(sockets_power$id))) {
  df <- sockets_power[sockets_power$id == unique(sockets_power$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    df_sub$cumEnergy <- df_sub$valueRaw
    for(k in 2:length(df_sub$id)) {
      if(df_sub$valueRaw[k] - df_sub$valueRaw[k-1]<0) {
        diff <- df_sub$valueRaw[k]
      } else {
        diff <- df_sub$valueRaw[k] - df_sub$valueRaw[k-1]
      }
      df_sub$cumEnergy[k] <- df_sub$cumEnergy[k-1] + diff
    }
    sockets_full2 <- rbind(sockets_full2, df_sub[1,])
    for(k in 2:length(df_sub$date)) {
      if(as.numeric(difftime(df_sub$time[k], df_sub$time[k-1], units="hours"))==1) {
        sockets_full2 <- rbind(sockets_full2, df_sub[k,])
      } else { #Fill in the missing values if time[k] - time[k-1] > 1
        #Find gap in hours to split the energy by
        hoursGap <- as.numeric(difftime(df_sub$time[k], df_sub$time[k-1], units="hours"))
        missingHours <- vector(mode="character", length=0L)
        #Find the missing hours
        for(q in 1:hoursGap) {
          missingHours <- append(missingHours, as.character(df_sub$time[k-1]+
                                                              q*60*60))
        }
        missingHours <- as.POSIXct(missingHours, format="%Y-%m-%d %H:%M:%S", tz="GMT")
        
        proportions <- vector(mode="numeric",length=0L)
        #Get ratios of energy distribution for each missing hour
        for(q in 1:length(missingHours)) {
          monthStamp <- as.character(month(missingHours[q], abbr=TRUE, label=TRUE))
          typ_data <- typical_energy_session$proportion[typical_energy_session$id==df$id[1]
                                    & typical_energy_session$month==monthStamp]
          timeStampMissing <- hour(missingHours[q])
          proportions <- append(proportions, typ_data[timeStampMissing+1])
        }
        
        #Find the energy consumed in the total gap including last hour
        energy_diff <- df_sub$cumEnergy[k] - df_sub$cumEnergy[k-1]
        
        #Get energy consumed per hour and add to previous value
        for(q in 1:length(missingHours)) {
          if(hour(missingHours[q]) < 10) {
            val <- paste("0",as.character(hour(missingHours[q])), sep="")
          } else {
            val <- as.character(hour(missingHours[q]))
          }
        sockets_full2 <- rbind(sockets_full2, data.frame("id"=df$id[1], 
         date=date(missingHours[q]), timeUse=val, variable=df_sub$variable[1], 
         value=sockets_full2$value[length(sockets_full2$id)] + 
           (proportions[q]*energy_diff/sum(proportions)), 
         valueRaw=sockets_full2$valueRaw[length(sockets_full2$id)] + 
           (proportions[q]*energy_diff/sum(proportions)), 
         month=as.character(month(missingHours[q], abbr=TRUE, label=TRUE)),
         time=missingHours[q], 
         cumEnergy=sockets_full2$cumEnergy[length(sockets_full2$id)] + 
           (proportions[q]*energy_diff/sum(proportions))))
        }
      }
    }
  }
}

#Use cumulative energy values to find hourly session values for a day
sockets_full_new <- data.frame()
for(i in 1:length(unique(sockets_full2$id))) {
  df <- sockets_full2[sockets_full2$id==unique(sockets_full2$id)[i], ]
  df$raw <- df$cumEnergy - df$cumEnergy[1]
  
  df_sub <- df[df$date==unique(df$date)[1], ]
  df_sub$clean <- df_sub$raw
  sockets_full_new <- rbind(sockets_full_new, df_sub)
  prev <- df_sub$raw[length(df_sub$id)]
  
  for(j in 2:length(unique(df$date))) {
    df_sub <- df[df$date==unique(df$date)[j], ]
    df_sub$clean <- df_sub$raw
    df_sub$clean[1] <- df_sub$raw[1] - prev
    for(k in 2:length(df_sub$id)) {
      df_sub$clean[k] <- df_sub$clean[k-1] + (df_sub$raw[k] - df_sub$raw[k-1])
    }
    sockets_full_new <- rbind(sockets_full_new, df_sub)
    prev <- df_sub$raw[length(df_sub$id)]
  }
}
sockets_full_new$value <- sockets_full_new$clean
sockets_full_new <- sockets_full_new[,-c(6,8,9,10,11)]

#Fill in zeroes for hours 0-10 for 19th July 2019
sockets_full_2 <- data.frame()
for(i in 1:length(unique(sockets_full_new$id))) {
  df <- sockets_full_new[sockets_full_new$id==unique(sockets_full_new$id)[i], ]
  for(j in 0:10) {
    if(j<10) {
      val <- paste("0",as.character(j),sep="")
    } else {
      val <- as.character(j)
    }
    df <- rbind(df, data.frame("id"=df$id[1], "date"=df$date[1],
                "timeUse"=val, "variable"=df$variable[1], 
                "value"=0, month="Jul"))
  }
  df <- df[order(df$date, df$timeUse),]
  sockets_full_2 <- rbind(sockets_full_2, df)
}

###Combine files for vRELAY1_LVL and AC_Day_Energy_Session
sockets_all <- data.frame()
sockets_all <- rbind(sockets_all, sockets_full, sockets_full_2)
sockets_all$id <- paste(sockets_all$id, sockets_all$variable) #Combine id and variable
sockets_all <- sockets_all[,-4] #Remove variable
sockets_all <- spread(sockets_all, id, value)
sockets_all$`S1 AC_Day_Energy_session` <- sockets_all$`S1 AC_Day_Energy_session` * 0.017 #Wm to Wh
sockets_all$`S2 AC_Day_Energy_session` <- sockets_all$`S2 AC_Day_Energy_session` * 0.017
sockets_all$`S3 AC_Day_Energy_session` <- sockets_all$`S3 AC_Day_Energy_session` * 0.017
sockets_all$`S4 AC_Day_Energy_session` <- sockets_all$`S4 AC_Day_Energy_session` * 0.017
sockets_write <- data.frame()
#Calculating diff of energy session to get consumption per hour
for(i in 1:length(unique(sockets_all$date))) {
  df <- sockets_all[sockets_all$date == unique(sockets_all$date)[i], ]
  s1 <- df$`S1 AC_Day_Energy_session`
  s2 <- df$`S2 AC_Day_Energy_session`
  s3 <- df$`S3 AC_Day_Energy_session`
  s4 <- df$`S4 AC_Day_Energy_session`
  for(j in 2:length(df$date)) {
    df$`S1 AC_Day_Energy_session`[j] <- s1[j] - s1[j-1]
    df$`S2 AC_Day_Energy_session`[j] <- s2[j] - s2[j-1]
    df$`S3 AC_Day_Energy_session`[j] <- s3[j] - s3[j-1]
    df$`S4 AC_Day_Energy_session`[j] <- s4[j] - s4[j-1]
  }
  sockets_write <- rbind(sockets_write, df)
}
#Store data in a file
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets_write, file="Sockets_all_full_hourly_power.csv", row.names=FALSE)

############################################################################################
######Reading in system data - 31-05-2019 23:00 to 29-11-2019 23:00 & 30-11-2019 00:00 to 01-01-2020 00:00
#Extract values AC Consumption L1 (W), PV Power (W), State of charge (%), 
#Discharged Energy (kWh), Charged Energy (kWh) 
filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/System data/Full data/June to Nov/HEEDNyabihekeHall_log_20190531_2300_to_20191129_2300.csv"
headers <- read_csv(filename, col_names = FALSE, na="..", n_max = 3)
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
system_data <- read_csv(filename, col_names = headers, na="..", skip = 3)
system_data[is.na(system_data)] <- ""

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
#Time is in Rwandan time zone already - do not need to add any hours as file starts from 31-05-2019 23:00
colnames(systemData) <- c("timestamp","AC_consumption_W","PV_power_W",
                          "State_of_charge","Discharged_energy_kWh",
                          "Charged_energy_kWh")
#Trim time from 19 July to Dec and convert in Rwandan time
systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01", 
                                   format="%d/%m/%Y %H:%M")
systemData <- systemData[systemData$timestamp>="2019-07-19 01:00:00 GMT", ]
###############

filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/System data/Full data/June to Nov/HEEDNyabihekeHall_log_20191130-0000_to_20200101-0000.csv"
headers <- read_csv(filename, col_names = FALSE, na="..", n_max = 3)
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
system_data <- read_csv(filename, col_names = headers, na="..", skip = 3)
system_data[is.na(system_data)] <- ""

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

systemData2 <- data.frame()
systemData2 <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
systemData2$`System overview [0] AC Consumption L1 W` <- 
  as.numeric(systemData2$`System overview [0] AC Consumption L1 W`)
systemData2$`Solar Charger [260] PV power ` <- 
  as.numeric(systemData2$`Solar Charger [260] PV power `)
systemData2$`Battery Monitor [258] Discharged Energy kWh` <- 
  as.numeric(systemData2$`Battery Monitor [258] Discharged Energy kWh`)
systemData2$`Battery Monitor [258] Charged Energy kWh` <- 
  as.numeric(systemData2$`Battery Monitor [258] Charged Energy kWh`) 
#Time is in Rwandan time zone already - do not need to add any hours as file starts from 31-05-2019 23:00
colnames(systemData2) <- c("timestamp","AC_consumption_W","PV_power_W",
                          "State_of_charge","Discharged_energy_kWh",
                          "Charged_energy_kWh")

###Add systemdata2 to systemData - 19th July to 31st Dec
systemData <- rbind(systemData, systemData2)
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(systemData, file="systemData_june_dec.csv", row.names=FALSE)

######Analyse data to get hourly values #######################################
system_gather <- gather(systemData, "id", "value", 2:6)
system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
system_gather$date <- date(system_gather$timestamp)

#Hourly means can be calculated for AC consumption and PV power
system_hourly <- system_gather[system_gather$id=="AC_consumption_W" | 
                                      system_gather$id=="PV_power_W",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=mean(value,na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)

#Calculate the last value in an hour for state of charge
system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=value[length(value)])
system_soc <- as.data.frame(system_soc)
system_hourly <- rbind(system_hourly, system_soc)

#Calculate hourly values for discharged and charged energy by taking hourly differences
battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
battery_charge <- battery_charge[complete.cases(battery_charge), ]
#Extract hourly values by taking the last value for each hour 
battery_charge_hours <- battery_charge %>%
  group_by(date, timeUse, id) %>%
  summarise(value = value[length(value)])
battery_charge_hours <- as.data.frame(battery_charge_hours)
a <- diff(battery_charge_hours$value)
battery_charge_hours <- battery_charge_hours[-1,]
battery_charge_hours$value <- a
battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
system_hourly <- rbind(system_hourly, battery_charge_hours)

battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
#Extract hourly values by taking the max value each hour 
battery_discharge_hours <- battery_discharge %>%
  group_by(date, timeUse, id) %>%
  summarise(value = value[length(value)])
battery_discharge_hours <- as.data.frame(battery_discharge_hours)
a <- diff(battery_discharge_hours$value)
battery_discharge_hours <- battery_discharge_hours[-1,]
battery_discharge_hours$value <- a
battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
system_hourly <- rbind(system_hourly, battery_discharge_hours)

system_hourly$month <- as.character(month(system_hourly$date, abbr=TRUE,
                                               label=TRUE))
system_write <- spread(system_hourly, id, value)
system_write[is.na(system_write)] <- 0 #Replace NA for charged and discharged energy with 0
colnames(system_write) <- c("date","timeUse","month","AC_consumption_W","Charged_energy_W",
                            "Discharged_energy_W", "PV_power_W","State_of_charge")
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(system_write, file="system_all_hourly_jul_dec.csv", row.names=FALSE)

###########################################################################################
#Reading in weather data - hourly data is stored
filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/Weather data/weather_data_dec.csv"
weather_data <- read.csv(filename, header=TRUE, as.is=TRUE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, 
                                     format = "%Y-%m-%d %H:%M")
##Data is 12 years behind - change year to 2019
weather_data$timestamp <- weather_data$timestamp %m+% years(12)
weather_data <- weather_data[,-2]
weather_data <- weather_data[weather_data$timestamp>="2019-07-19 00:00:00 GMT", ]
weather_data <- weather_data[complete.cases(weather_data), ]
weather_data$Pot.PV.Power..kW.m2. <- weather_data$Pot.PV.Power..kW.m2. * 13.094 * 0.156 * 1000.0 #W
colnames(weather_data) <- c("timestamp","Pot_PV_power_W")
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(system_write, file="weather_hourly_jul_dec.csv", row.names=FALSE)

###########################################################################################
##Combining weather, system, cpe and socket hourly data for Hall
hall_data <- weather_data
hall_data <- cbind(hall_data, system_write[,c(4:8)])
hall_data <- cbind(hall_data, sockets_write[,c(4:11)])
hall_data <- cbind(hall_data, cpe_write[,c(4:24)])
colnames(hall_data) <- c("timestamp", "Pot_PV_power_W", "AC_consumption_W","Charged_energy_W",
                         "Discharged_energy_W","PV_power_W","State_of_charge(%)", 
                         "S1_AC_Day_Energy_session_Wh","S1_vRELAY1_LVL_W",
                         "S2_AC_Day_Energy_session_Wh","S2_vRELAY1_LVL_W",
                         "S3_AC_Day_Energy_session_Wh","S3_vRELAY1_LVL_W",
                         "S4_AC_Day_Energy_session_Wh","S4_vRELAY1_LVL_W", "CPE1_LED1_P_W",
                         "CPE1_LED2_P_W","CPE1_LED3_P_W","CPE2_LED1_P_W","CPE2_LED2_P_W","CPE2_LED3_P_W",
                         "CPE3_LED1_P_W","CPE3_LED2_P_W","CPE3_LED3_P_W","CPE4_LED1_P_W","CPE4_LED2_P_W",
                         "CPE4_LED3_P_W","CPE5_LED1_P_W","CPE5_LED2_P_W","CPE5_LED3_P_W","CPE6_LED1_P_W",
                         "CPE6_LED2_P_W","CPE6_LED3_P_W","CPE7_LED1_P_W","CPE7_LED2_P_W","CPE7_LED3_P_W")
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(hall_data, file="hall_data_hourly_jul_dec.csv", row.names=FALSE)

#################################################################################################
##Plotting hourly socket consumption using energy session values for weekdays and weekends per month

