#Analysis for paper 5 on MG and Hall
#Analysis of Hall data

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

#Reading in the CPE data
cpe <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/CPE_all.csv",
                stringsAsFactors = FALSE)

#Reading in the socket data
sockets <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/Sockets_all.csv",
                    stringsAsFactors = FALSE)

#Total days of study = 166 (19 July to 31 Dec 2019)
cpe$time <- as.POSIXct(cpe$timestamp, tz="GMT", origin="1970-01-01")
#Add 2 hours to change time to african time zone
cpe$time <- cpe$time %m+% hours(2)
cpe$date <- date(cpe$time)
cpe$timeUse <- format(cpe$time, format='%H')
cpe_sub <- cpe %>%
  group_by(id, date, timeUse, variable) %>%
  summarise(value=mean(value, na.rm=TRUE))
cpe_sub <- as.data.frame(cpe_sub)
cpe_sub <- cpe_sub[cpe_sub$date>="2019-07-19",]
cpe_sub$month <- as.character(month(cpe_sub$date, label=TRUE, abbr=TRUE))
cpe_write <- cpe_sub
cpe_write$id <- paste(cpe_write$id, cpe_write$variable) #Combine id and variable
cpe_write <- cpe_write[,-4] #Remove variable
cpe_write <- spread(cpe_write, id, value)
cpe_write$time <- as.POSIXct(paste(paste(cpe_write$date, 
                                         cpe_write$timeUse), ":00:00",sep=""),
                             format="%Y-%m-%d %H:%M:%S", tz="GMT")
cpe_write <- cpe_write[,-c(1,2,3)] #Remove date, time and month
cpe_write <- cpe_write[,c(22,1:21)] #Rearrange columns
#Select data from 19th July
cpe_write <- cpe_write[cpe_write$time>="2019-07-19 00:00:00 GMT", ]
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(cpe_write, file="CPE_all_hourly_mean.csv", row.names=FALSE)

################################################################################
#Analyse the sockets data to extract hourly means for instantaenous power and 
#hourly totals for AC_day_energy_session. Total days of study = 166 till Dec
sockets_sub <- data.frame()
for(i in 1:length(unique(sockets$id))) {
  df <- sockets[sockets$id == unique(sockets$id)[i], ]
  
  #Add time - we need to extract hours to get hourly means
  df$time <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01")
  #Add 2 hours to change time to african time zone
  df$time <- df$time %m+% hours(2)
  #Get date per file
  df$date <- date(df$time)
  df$timeUse <- format(df$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df[df$variable!="AC_Day_Energy_session",] %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = mean(value, na.rm=TRUE))
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
    summarise(value = value2[length(na.omit(value2))])
  df3 <- as.data.frame(df3)
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  
  #Create a data frame with the above values and bind to a complete df 
  sockets_sub <- rbind(sockets_sub, df2)
}
sockets_sub$month <- as.character(month(sockets_sub$date, label=TRUE, abbr=TRUE))
sockets_sub$id <- paste(sockets_sub$id, sockets_sub$variable) #Combine id and variable
sockets_sub <- sockets_sub[,-4] #Remove variable
sockets_sub <- sockets_sub[sockets_sub$date>="2019-07-19",]
sockets_all <- spread(sockets_sub, id, value)
sockets_all$`S1 AC_Day_Energy_session` <- sockets_all$`S1 AC_Day_Energy_session` * 0.017 #Wm to Wh
sockets_all$`S2 AC_Day_Energy_session` <- sockets_all$`S2 AC_Day_Energy_session` * 0.017
sockets_all$`S3 AC_Day_Energy_session` <- sockets_all$`S3 AC_Day_Energy_session` * 0.017
sockets_all$`S4 AC_Day_Energy_session` <- sockets_all$`S4 AC_Day_Energy_session` * 0.017
sockets_write <- data.frame()
#Calculating diff of energy session between hours recorded
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
sockets_write$time <- as.POSIXct(paste(paste(sockets_write$date, 
                                         sockets_write$timeUse), ":00:00",sep=""),
                             format="%Y-%m-%d %H:%M:%S", tz="GMT")
sockets_write <- sockets_write[,-c(1,2,3)] #Remove date, time and month
sockets_write <- sockets_write[,c(9,1:8)] #Rearrange columns
#Select data from 19th July
sockets_write <- sockets_write[sockets_write$time>="2019-07-19 00:00:00 GMT", ]
#Store data in a file
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(sockets_write, file="Sockets_all_hourly_mean.csv", row.names=FALSE)

##################################################################################
#Number of on hours per day for each id and each variable
cpe_on_hours <- cpe_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
cpe_on_hours <- as.data.frame(cpe_on_hours)
#Once we have hours on for each available day, look for missing dates
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
cpe_on_hours2 %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each LED at Hall since Jul 2019" ,
       y="Hours of data collection in a day",
       x = "Day of study" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1,5,2)) +
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     labels=c("0","2","4","6","8","10","12","14","16","18","20",
                              "22","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day") +
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

###########################################################################
##Number of on hours for socket data
sockets_sub$variable <- substr(sockets_sub$id,4,str_length(sockets_sub$id))
sockets_sub$id <- substr(sockets_sub$id,1,2)
sockets_on_hours <- sockets_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
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
sockets_on_hours2 %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Hours of data collection per day for each socket at Hall since Jul 2019" ,
       y="Hours of data collection in a day",
       x = "Day of study" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     labels=c("0","2","4","6","8","10","12","14","16","18","20",
                              "22","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day") +
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

#Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
cpe_on_hours2$yield <- cpe_on_hours2$onHours * 100.0 / 24.0
cpe_on_hours2$id2 <- paste(as.character(cpe_on_hours2$id), 
                           as.character(cpe_on_hours2$variable))
sockets_on_hours2$yield <- sockets_on_hours2$onHours * 100.0 / 24.0
sockets_on_hours2$id2 <- paste(as.character(sockets_on_hours2$id),
                               as.character(sockets_on_hours2$variable))
library(wesanderson)
#Plotting a heat map for CPE
pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot(cpe_on_hours2, aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for CPE data at Hall: 19th Jul'19 - 31st Dec'19" , 
       y="ID",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8, angle=45), 
        plot.title = element_text(size=10))

ggplot(sockets_on_hours2, aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for sockets data at Hall: 19th Jul'19 - 31st Dec'19" , 
       y="ID",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8, angle=45),
        plot.title = element_text(size=10))

##############################################################################
#Last hour on per day for each id and each variable
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
cpe_max_time2 %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Last hour of data collection per day for each LED at Hall in Jul 2019" ,
       y="Last hour of data collection",
       x = "Day of study" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1,5,2)) +
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,1,3,5,7,9,11,13,15,17,19,21,23), 
                     labels=c("0","1","3","5","7","9","11","13","15","17",
                              "19","21","23"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day") + 
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

##############################################################################
#Last hour on per day for each id and each variable
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
sockets_max_time2 %>%
  ggplot(aes(date, onHours, color=variable, shape=id)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket at Hall in Jul 2019" ,
       y="Last hour of data collection",
       x = "Day of study" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,1,3,5,7,9,11,13,15,17,19,21,23), 
                     labels=c("0","1","3","5","7","9","11","13","15","17",
                              "19","21","23"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day") + 
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

#################################################################################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/System data/")
systemData <- data.frame()
for(k in 1:2) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/System data/")
  if(k==1) {
    setwd("./Full data/June to Nov/") ##Time in Berline time
  } else if(k==2) {
    setwd("./12 2019/") ##Time in Africa time
  } 
  
  #For each month list all files
  file_list <- list.files()
  
  headers <- read_csv(file_list[1], col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  system_data <- read_csv(file_list[1], col_names = headers, na="..", skip = 3)
  system_data[is.na(system_data)] <- "" 
  
  #Extract values AC Consumption L1 (W), PV Power (W), State of charge (%), 
  #Discharged Energy (kWh), Charged Energy (kWh) 
  
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
  
  df <- data.frame()
  df <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  df$`System overview [0] AC Consumption L1 W` <- 
    as.numeric(df$`System overview [0] AC Consumption L1 W`)
  df$`Solar Charger [260] PV power ` <- 
    as.numeric(df$`Solar Charger [260] PV power `)
  df$`Battery Monitor [258] Discharged Energy kWh` <- 
    as.numeric(df$`Battery Monitor [258] Discharged Energy kWh`)
  df$`Battery Monitor [258] Charged Energy kWh` <- 
    as.numeric(df$`Battery Monitor [258] Charged Energy kWh`) 
  #Time is in Berlin time zone for data till Nov otherwise Africa zone
  colnames(df) <- c("timestamp","AC_consumption_W","PV_power_W",
                            "State_of_charge","Discharged_energy_kWh",
                            "Charged_energy_kWh")
  if(k==1) {
    df$timestamp <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01", 
                               format="%d/%m/%Y %H:%M")
    df$timestamp <- df$timestamp %m+% hours(1)
  }
  systemData <- rbind(systemData, df)
}
#Trim time from 19 July
systemData$date <- date(systemData$timestamp)
systemData <- systemData[systemData$date>="2019-07-19",]
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(systemData, file="systemData_june_dec.csv", row.names=FALSE)

#############################################################################################
######Analyse data to get hourly values #######################################
system_gather <- gather(systemData, "id", "value", 2:6)
system_gather$timeUse <- format(system_gather$timestamp, format='%H')  

#Hourly means can be calculated for AC consumption and PV power
system_hourly <- system_gather[system_gather$id=="AC_consumption_W" | 
                                 system_gather$id=="PV_power_W",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=mean(value,na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)

#Calculate the last value in an hour for state of charge
system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=value[length(na.omit(value))])
system_soc <- as.data.frame(system_soc)
system_hourly <- rbind(system_hourly, system_soc)

#Calculate hourly values for discharged and charged energy by taking hourly differences
battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
battery_charge <- battery_charge[complete.cases(battery_charge), ]
#Extract hourly values by taking the last value for each hour 
battery_charge_hours <- battery_charge %>%
  group_by(date, timeUse, id) %>%
  summarise(value = value[length(na.omit(value))])
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
  summarise(value = value[length(na.omit(value))])
battery_discharge_hours <- as.data.frame(battery_discharge_hours)
a <- diff(battery_discharge_hours$value)
battery_discharge_hours <- battery_discharge_hours[-1,]
battery_discharge_hours$value <- a
battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
system_hourly <- rbind(system_hourly, battery_discharge_hours)

system_hourly$month <- as.character(month(system_hourly$date, abbr=TRUE,
                                          label=TRUE))
system_hourly$time <- as.POSIXct(paste(paste(system_hourly$date, 
                                             system_hourly$timeUse),
                                       ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
system_hourly <- system_hourly[,-c(1,2,5)] #Remove date, time and month
system_hourly <- system_hourly[,c(3,1:2)] #Rearrange columns

system_write <- spread(system_hourly, id, value)
#Replace NA for charged and discharged energy with 0
system_write$Charged_energy_kWh[is.na(system_write$Charged_energy_kWh)] <- 0
system_write$Discharged_energy_kWh[is.na(system_write$Discharged_energy_kWh)] <- 0

colnames(system_write) <- c("time","AC_consumption_W","Charged_energy_W",
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
colnames(weather_data) <- c("time","Pot_PV_power_W")
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(weather_data, file="weather_hourly_jul_dec.csv", row.names=FALSE)

##################################################################################
##CPE, socket, system and weather data has been analysed to get hourly values#####
##Use the hourly values to analyse the data for plots#############################
weather_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/weather_hourly_jul_dec.csv",
                         header = TRUE,stringsAsFactors = FALSE)
weather_data$time <- as.POSIXct(weather_data$time, 
                                format = "%Y-%m-%d %H:%M")
weather_data$date <- date(weather_data$time)
weather_data$timestamp <- format(weather_data$time, format='%H')
weather_data <- weather_data[,-1]
weather_gather <- gather(weather_data, "id", "value", 1)

cpe_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/CPE_all_hourly_mean.csv",
                     header=TRUE, stringsAsFactors = FALSE)
cpe_data$time <- as.POSIXct(cpe_data$time, format="%Y-%m-%d %H:%M")
cpe_data$date <- date(cpe_data$time)
cpe_data$timestamp <- format(cpe_data$time, format='%H')
cpe_data <- cpe_data[,-1]
cpe_gather <- gather(cpe_data, "id", "value", 1:21)

sockets_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/Sockets_all_hourly_mean.csv",
                         header = TRUE, stringsAsFactors = FALSE)
sockets_data$time <- as.POSIXct(sockets_data$time, format="%Y-%m-%d %H:%M")
sockets_data$date <- date(sockets_data$time)
sockets_data$timestamp <- format(sockets_data$time, format='%H')
sockets_data <- sockets_data[,-1]
sockets_gather <- gather(sockets_data, "id", "value", 1:8)

system_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/system_all_hourly_jul_dec.csv",
                        header=TRUE, stringsAsFactors = FALSE)
system_data$time <- as.POSIXct(system_data$time, format="%Y-%m-%d %H:%M")
system_data$date <- date(system_data$time)
system_data$timestamp <- format(system_data$time, format='%H')
system_data <- system_data[,-1]
system_gather <- gather(system_data, "id", "value", 1:5)

###########################################################################################
##Combining weather, system, cpe and socket hourly data for Hall
hall_gather <- data.frame()
hall_gather <- rbind(hall_gather, weather_gather, cpe_gather, sockets_gather,
                     system_gather)
hall_data <- spread(hall_gather, id, value)
hall_data <- hall_data[hall_data$date <= "2019-12-31",]
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(hall_data, file="hall_data_hourly_jul_dec.csv", row.names=FALSE)

###############################################################################################
#Plotting Hall data for paper 5
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
hall_data <- read.csv("hall_data_hourly_jul_dec.csv", header=TRUE)
hall_data$date <- as.Date(hall_data$date)

#Predicted load 
hall_predicted <- data.frame(month=rep("Predicted",24),timestamp = c(0:23), 
                             avgLoad=c(0,0,0,0,0,0,0.2,0.2,0.1,0.1,0.1,0.6,0.5,0.5,
                                  0.6,0.1,0.7,0.7,0.2,0.2,0.2,0,0,0), 
                             stringsAsFactors = FALSE)
hall_predicted$avgLoad <- hall_predicted$avgLoad * 1000.0 #Wh

#For plot 1 - hourly avg to show typical data since commissioning (all sockets and lights)
#Load 1 - sum CPE_P values with vRELAY_LVL values for sockets
hall_users1 <- hall_data[,c("date","timestamp", "AC_consumption_W",
                            colnames(hall_data)[grepl("CPE",colnames(hall_data),fixed=TRUE) | 
                            grepl("LVL",colnames(hall_data),fixed=TRUE)])]
#hall_users1 <- hall_users1[complete.cases(hall_users1), ]

#Get monthly totals
hall_users1_monthly <- hall_data[,c("date","timestamp", "AC_consumption_W",
                      colnames(hall_data)[grepl("CPE",colnames(hall_data),fixed=TRUE) | 
                              grepl("session",colnames(hall_data),fixed=TRUE)])]
#Get cumulative session values
hall_users1_monthly$cum_S1 <- hall_users1_monthly$S1.AC_Day_Energy_session
hall_users1_monthly$cum_S2 <- hall_users1_monthly$S2.AC_Day_Energy_session
hall_users1_monthly$cum_S3 <- hall_users1_monthly$S3.AC_Day_Energy_session
hall_users1_monthly$cum_S4 <- hall_users1_monthly$S4.AC_Day_Energy_session
hall_monthly <- data.frame()
for(i in 1:length(unique(hall_users1_monthly$date))) {
  df <- hall_users1_monthly[hall_users1_monthly$date==hall_users1_monthly$date[i],]
  for(k in 2:length(df$date)) {
    df$cum_S1[k] <- sum(df$cum_S1[k-1], df$S1.AC_Day_Energy_session[k], na.rm=TRUE)
    df$cum_S2[k] <- sum(df$cum_S2[k-1], df$S2.AC_Day_Energy_session[k], na.rm=TRUE)
    df$cum_S3[k] <- sum(df$cum_S3[k-1], df$S3.AC_Day_Energy_session[k], na.rm=TRUE)
    df$cum_S4[k] <- sum(df$cum_S4[k-1], df$S4.AC_Day_Energy_session[k], na.rm=TRUE)
  }
  hall_monthly <- rbind(hall_monthly, df)
}



#Total load at nurseries - select all CPE and sockets at Nur except Nur1AS1
test <- hall_users1_monthly[,c(4:28)]
hall_users1_monthly$load <- rowSums(test)

#Subset date, time, AC consumption and laod data
hall_users1_monthly <- hall_users1_monthly[,c(1:3,29)]
hall_users1_monthly$month <- as.character(month(hall_users1_monthly$date, label=TRUE, abbr=TRUE))

#hall_cpe_session <- hall_data[,c("date","timestamp", "AC_consumption_W",
#                         colnames(hall_data)[grepl("CPE",colnames(hall_data),fixed=TRUE) | 
#                                               grepl("session",colnames(hall_data),fixed=TRUE)])]
#test <- hall_cpe_session[,c(4:28)]
#hall_cpe_session$load <- rowSums(test)
#full_days <- hall_cpe_session %>%
#  dplyr::group_by(date) %>%
#  dplyr::summarise(hours = length(na.omit(load)))
  
  
monthly_load <- hall_users1_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(load = sum(load, na.rm=TRUE))

colnames(monthly_load) <- c("Month","Hall_load_W")
write.csv(monthly_load, file="~/OneDrive - Coventry University/HEED_analysis/Hall/Hall_total_monthly.csv", 
          row.names=FALSE)

############################################################################################
#Get typical load for each device
hall_users1 <- gather(hall_users1, "id","value", 3:28)
hall_users1$month <- as.character(month(hall_users1$date, abbr=TRUE, label=TRUE))
typical_user <- hall_users1 %>%
  group_by(month, timestamp, id) %>%
  summarise(value=mean(value, na.rm=TRUE))
typical_user <- as.data.frame(typical_user)
user_load1 <- spread(typical_user, id, value) 

#Total load - sum typical load at each socket and CPE
test <- user_load1[,4:28]
user_load1$avgLoad <- rowSums(test)

#################################################################################
###Daily average load per month
daily_avg_hall <- user_load1[,c(1:3,29)]
colnames(daily_avg_hall) <- c("Month","Hour","AC_consumption_W","User_load_W")
write.csv(daily_avg_hall, file="~/OneDrive - Coventry University/HEED_analysis/Hall/hall_daily_avg.csv", row.names=FALSE)
################################################################################

#For plot 1 - hourly avg to show typical data since commissioning (all sockets and lights)
user_load1 <- cbind(user_load1, "predicted"=rep(hall_predicted$avgLoad,6))
user_load1 %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=avgLoad, color=month, linetype=month)) + 
  geom_line(aes(y = predicted)) +
  labs(title="Typical day socket and light load at Community Hall: 19 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##Plotting typical AC consumption values
user_load1 %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=AC_consumption_W, color=month, linetype=month)) + 
  geom_line(aes(y = predicted)) +
  labs(title="Typical day AC consumption at Community Hall: 19 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

####Separating out hall load 
hallLoad <- user_load1[,c(1:2,29)]
hallLoad <- spread(hallLoad, month, avgLoad)
hallLoad$Predicted <- hall_predicted$avgLoad
hallLoad <- gather(hallLoad, "id", "value", 2:8)
hallLoad$id2 <- factor(hallLoad$id, 
                          levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"),
                          labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"))
#For plot 1 - hourly avg to show typical load at Hall
hallLoad %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=value, color=id2, linetype=id2)) + 
  labs(title="Typical day socket and light load at the Community Hall: 19 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_linetype_manual(values=c(2,3,4,5,6,7,1)) +
  scale_color_manual(values=c("darkgreen", "darkmagenta", "deepskyblue",
                              "darkorange","firebrick1","darkorchid","gray48"))+
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

####Separating out hall load 
achallLoad <- user_load1[,c(1:3)]
achallLoad <- spread(achallLoad, month, AC_consumption_W)
achallLoad$Predicted <- hall_predicted$avgLoad
achallLoad <- gather(achallLoad, "id", "value", 2:8)
achallLoad$id2 <- factor(achallLoad$id, 
                       levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"),
                       labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"))
#For plot 1 - hourly avg to show typical AC consumption at Hall
achallLoad %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=value, color=id2, linetype=id2)) + 
  labs(title="Typical day AC consumption at the Community Hall: 19 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_linetype_manual(values=c(2,3,4,5,6,7,1)) +
  scale_color_manual(values=c("darkgreen", "darkmagenta", "deepskyblue",
                              "darkorange","firebrick1","darkorchid","gray48"))+
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##############################################################################
##Plot 2 - Typical day for system data
hall_system_weather <- hall_data[,c("date","timestamp","AC_consumption_W", "Charged_energy_W",
                                    "Discharged_energy_W","Pot_PV_power_W","PV_power_W",
                                    "State_of_charge")]
hall_system_weather <- hall_system_weather[complete.cases(hall_system_weather),]
hall_system_weather$losses <- hall_system_weather$Pot_PV_power_W - 
  hall_system_weather$PV_power_W
hall_system_weather$month <- as.character(month(hall_system_weather$date, abbr=TRUE,
                                                label=TRUE))
hall_system <- gather(hall_system_weather, "id", "value", 3:9)
typical_system_data <- hall_system %>%
  group_by(month, timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_system_data <- as.data.frame(typical_system_data)
typical_system_data <- typical_system_data[order(typical_system_data$month),]
typical_system_data <- spread(typical_system_data, id, value)

###Plotting data for a typical day in each month
plotTypical <- function(df,p) {
  df %>%
    ggplot(aes(x=timestamp)) +
    geom_line(aes(y = AC_consumption_W/1000.0, color = "AC Load", group="AC Load"), linetype=1) + 
    geom_point(aes(y = AC_consumption_W/1000.0, color = "AC Load"), shape=1) + 
    geom_line(aes(y = PV_power_W/1000.0, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
    geom_point(aes(y = PV_power_W/1000.0, color = "Actual PV Power"), shape=2) + 
    geom_line(aes(y = losses/1000.0, color = "Capture losses", group="Capture losses"), linetype=3) + 
    geom_point(aes(y = losses/1000.0, color = "Capture losses"), shape=3) + 
    geom_line(aes(y = Charged_energy_W/1000.0, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
    geom_point(aes(y = Charged_energy_W/1000.0, color = "Charged Energy"), shape=4) + 
    geom_line(aes(y = Discharged_energy_W/1000.0, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
    geom_point(aes(y = Discharged_energy_W/1000.0, color = "Discharged Energy"), shape=5) + 
    geom_line(aes(y = Pot_PV_power_W/1000.0, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
    geom_point(aes(y = Pot_PV_power_W/1000.0, color = "Pot. PV Power"), shape=6) + 
    geom_line(aes(y = State_of_charge/p, color = "State of charge", group="State of charge"), linetype=7) + 
    geom_point(aes(y = State_of_charge/p, color = "State of charge"), shape=7) +
    scale_y_continuous(sec.axis = sec_axis(~.*p, name = "State of charge (%)")) +
    labs(y="Energy (kWh)",
         x = "Time of day (00-23 hours)", 
         colour="Parameter") +
    scale_x_continuous(labels=c("0","2","4","6","8","10","12","14","16","18",
                                "20","22","24"),
                       breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
    theme(plot.title = element_text(size=10), legend.position = "bottom",
          legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0)) 
}

p <- plotTypical(typical_system_data[typical_system_data$month=="Jul", ], 55.0)
p + labs(title = "Actual Hall power profile for a typical day in Jul 2019")

p <- plotTypical(typical_system_data[typical_system_data$month=="Aug", ], 55.0)
p + labs(title = "Actual Hall power profile for a typical day in Aug 2019")

p <- plotTypical(typical_system_data[typical_system_data$month=="Sep", ], 55.0)
p + labs(title = "Actual Hall power profile for a typical day in Sep 2019")

p <- plotTypical(typical_system_data[typical_system_data$month=="Oct", ], 80.0)
p + labs(title = "Actual Hall power profile for a typical day in Oct 2019")

p <- plotTypical(typical_system_data[typical_system_data$month=="Nov", ], 75.0)
p + labs(title = "Actual Hall power profile for a typical day in Nov 2019")

p <- plotTypical(typical_system_data[typical_system_data$month=="Dec", ], 75.0)
p + labs(title = "Actual Hall power profile for a typical day in Dec 2019")

#########Combining data for first and next 3 months ####################
hall_system$month2 <- month(hall_system$date)
hall_system$phase <- ifelse(hall_system$month2<10, 1, 2)
typical_phase <- hall_system %>%
  group_by(phase, timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_phase <- as.data.frame(typical_phase)
typical_phase <- spread(typical_phase, id, value)

p <- plotTypical(typical_phase[typical_phase$phase==1, ], 62.0)
p + labs(title = "Actual Hall power profile for a typical day in Jul-Sep 2019")

p <- plotTypical(typical_phase[typical_phase$phase==2, ], 80.0)
p + labs(title = "Actual Hall power profile for a typical day in Oct-Dec 2019")

####Combining all months #############################################
typical_hall <- hall_system %>%
  group_by(timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_hall <- as.data.frame(typical_hall)
typical_hall <- spread(typical_hall, id, value)

p <- plotTypical(typical_hall, 70.0)
p + labs(title = "Actual Hall power profile for a typical day in Jul-Dec 2019")

###########################################################################
###Plot 3 - total energy consumption per day since commissioning
##Calculating aux for hours that are complete
hall_users2 <- hall_data[,c("date","timestamp", "AC_consumption_W")]
hall_users2 <- hall_users2[complete.cases(hall_users2), ]
complete_days <- hall_users2 %>%
  group_by(date) %>%
  summarise(len = length(na.omit(AC_consumption_W)))
complete_days <- as.data.frame(complete_days)
hall_users2 <- hall_users2[hall_users2$date %in% 
                             complete_days$date[which(complete_days$len==24)],]
#Daily consumption
hall_daily <- hall_users2 %>%
  group_by(date) %>%
  summarise(consumption = sum(AC_consumption_W))
startDate <- as.Date("2019-07-18")
hall_daily$days <- as.numeric(hall_daily$date - startDate)

hall_daily %>%
  ggplot(aes(x=days, y=consumption/1000.0, color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Daily AC consumption at the Hall since commissioning on 19th Jul'19 to 31st Dec'19 " , 
       y="Total energy consumption (kWh)",
       x = "Days since commissioning",
       colour="") + 
  theme(plot.title = element_text(size=10),legend.position = "none") +
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180),
                     labels=c("0","20","40","60","80","100","120","140","160","180")) +
  scale_y_continuous(limits = c(0.5,5.5))

##############################################################################
#####Plot 4 - final yield, capture losses, system losses
#Yield = AC_consumption
#System loss = PV_power - Yield 
#Capture loss = Pot_PV_power - PV_power
hall_users3 <- hall_data[,c("date","timestamp","AC_consumption_W", 
                            "Pot_PV_power_W","PV_power_W")]
hall_users3 <- hall_users3[complete.cases(hall_users3),]

complete_days <- hall_users3 %>%
  group_by(date) %>%
  summarise(len = length(timestamp))
complete_days <- as.data.frame(complete_days)
hall_users3 <- hall_users3[hall_users3$date %in% 
                             complete_days$date[which(complete_days$len==24)],]
#Daily consumption
hall_users3 <- gather(hall_users3, "id", "value", 3:5)
hall_perf <- hall_users3 %>%
  group_by(date, id) %>%
  summarise(value = sum(value))
hall_perf <- as.data.frame(hall_perf)
hall_perf <- spread(hall_perf, id, value)
startDate <- as.Date("2019-07-18")
hall_perf$days <- as.numeric(hall_perf$date - startDate)

#Capture losses
hall_perf$captureLoss <- hall_perf$Pot_PV_power_W - hall_perf$PV_power_W
hall_perf$systemLoss <- hall_perf$PV_power_W - hall_perf$AC_consumption_W

#Daily average per month
hall_perf <- gather(hall_perf, "id","value",c(2,3,4,6,7))
hall_perf$month <- as.character(month(hall_perf$date, abbr=TRUE, label=TRUE))
hall_monthly <- hall_perf %>%
  group_by(month, id) %>%
  summarise(value=mean(value))
hall_monthly <- as.data.frame(hall_monthly)
hall_monthly$month2 <- factor(hall_monthly$month, 
                       levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                       labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
hall_monthly[hall_monthly$id!="Pot_PV_power_W" & hall_monthly$id!="PV_power_W", ] %>%
  ggplot(aes(x = month2, y= value/1000.0, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily average electrical energy values at the Hall in 2019" ,
       y="Consumed and potential electrical energy (kWh)",
       x = "Month",
       fill="Parameter")  +
  scale_fill_manual(labels = c("Final yield", "Capture losses","System losses"), 
                    values = wes_palette("GrandBudapest1", n = 3)) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

#####Plot 6 - to assess performance in comparison to the predicted behaviour
#Over and under prediction
hall_pred <- hall_data[,c("date","timestamp",
                            colnames(hall_data)[grepl("CPE",colnames(hall_data),fixed=TRUE) | 
                                                  grepl("LVL",colnames(hall_data),fixed=TRUE)])]
test <- hall_pred[,3:27]
hall_pred$userLoad <- rowSums(test) #NA is ok as only consider rows with complete data
#Bind the predicted values for hall
hall_pred <- cbind(hall_pred, "predicted"=rep(hall_predicted$avgLoad,166))
#Remove NA values
hall_pred <- hall_pred[complete.cases(hall_pred),]

#Difference between Pred and actual
hall_pred$diff <- hall_pred$predicted - hall_pred$userLoad
hall_pred$diff_range <- hall_pred$diff
for(i in 1:length(hall_pred$date)) {
  if(abs(hall_pred$diff[i])<=50) {
    hall_pred$diff_range[i] = 1
  } else if(abs(hall_pred$diff[i])>=50 & abs(hall_pred$diff[i])<100) {
    hall_pred$diff_range[i] = 2
  } else if(abs(hall_pred$diff[i])>=100 & abs(hall_pred$diff[i])<250) {
    hall_pred$diff_range[i] = 3
  } else if(abs(hall_pred$diff[i])>=250 & abs(hall_pred$diff[i])<500) {
    hall_pred$diff_range[i] = 4
  } else if(abs(hall_pred$diff[i])>=500) {
    hall_pred$diff_range[i] = 5
  } 
}

#Rounding to the nearest 100th value
hall_pred$consumed_roundUp <- round(hall_pred$userLoad, digits=-2)
hall_pred$diff_roundUp <- hall_pred$predicted - hall_pred$consumed_roundUp

hall_pred %>%
  ggplot(aes(diff_roundUp)) +
  geom_histogram(aes(y=(..count..)*100/sum(..count..)), binwidth=100,colour="black", fill="white") +
  labs(title="Distribution of energy-use over and under predictions at the Hall: 19th Jul'19 to 31st Dec'19" , 
       x = "Error (Wh)",
       y="Occurrences (%)") +
  theme(plot.title = element_text(size=10)) +
  geom_vline(aes(xintercept=median(diff_roundUp)),
             color="red") +
  geom_vline(aes(xintercept=quantile(hall_pred$diff_roundUp)[[2]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(hall_pred$diff_roundUp)[[4]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(hall_pred$diff_roundUp)[[1]]),
             color="darkgreen", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(hall_pred$diff_roundUp)[[5]]),
             color="darkgreen", linetype="dashed") 

##Plotting over and use across hours
hall_pred$diff_range <- as.factor(hall_pred$diff_range)
#install.packages("wesanderson")
library(wesanderson)
hall_pred %>%
  ggplot(aes(timestamp, diff, color=diff_range)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Hall: 19th Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Absolute error (Wh)") +
  scale_color_manual(labels = c("<50", ">=50 & <100",">=100 & <250",
                                ">=250 & <500",">=500"),
                     values = c("lightpink",wes_palette("GrandBudapest1", n = 4))) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
       legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##Plotting over and use across hours for each month
hall_pred$month <- as.character(month(hall_pred$date, abbr=TRUE, label=TRUE))
hall_pred$month2 <- factor(hall_pred$month, 
                           levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                           labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
library(wesanderson)
hall_pred %>%
  ggplot(aes(timestamp, diff, color=month2, shape=month2)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Hall: 19th Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Month",
       shape="Month") +
  scale_shape_manual(values=c(1,4,1,4,1,4)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange","firebrick1","darkorchid")) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 
 
#############################################################################
#####Typical system load using predicted values#################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/System predicted/")
systemData <- data.frame()
file_list <- list.files()
for(k in 1:length(file_list)) {
  #Read headers spread across 2 rows
  headers <- read_csv(file_list[k], col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  system_data <- read_csv(file_list[k], col_names = headers, na="..", skip = 2)
  system_data[is.na(system_data)] <- "" 
  
  #Extract values Time, AC Primary Load kW, Actual PV power, 
  #Potential PV power output kW, Capture losses kW, Charge Power kW, 
  #Discharge Power kW, State of Charge
  
  #Extract AC Primary Load
  colNames <- headers[which(grepl("AC Primary Load", headers, fixed=TRUE))]
  #Extract Actual PV power
  colNames <- c(colNames, 
                headers[which(grepl("Actual PV power", headers, fixed=TRUE))])
  #Extract Potential PV Power Output
  colNames <- c(colNames, 
                headers[which(grepl("Potential PV Power Output", headers, fixed=TRUE))])
  #Extract Capture losses 
  colNames <- c(colNames, 
                headers[which(grepl("Capture losses", headers, fixed=TRUE))])
  #Extract Charge Power
  colNames <- c(colNames, 
                headers[which(grepl("Li-Ion Charge Power", headers, fixed=TRUE))])
  #Extract Discharge Power
  colNames <- c(colNames, 
                headers[which(grepl("Li-Ion Discharge Power", headers, fixed=TRUE))])
  #Extract State of Charge
  colNames <- c(colNames, 
                headers[which(grepl("State of Charge", headers, fixed=TRUE))])
  #Extract time
  colNames <- c(headers[1], colNames) 
  
  #Subset the system data
  df <- system_data[,colNames]

  #Time is in Africa zone but 12 hours behind
  colnames(df) <- c("timestamp","AC_consumption_kW","PV_power_kW",
                    "Potential_PV_power_kW", "Capture_losses_kW",
                    "Charged_energy_kW", "Discharged_energy_kW",
                    "State_of_charge")
  df$timestamp <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01", 
                             format="%d/%m/%Y %H:%M")
  df$timestamp <- df$timestamp %m+% years(12)
  systemData <- rbind(systemData, df)
}
#Trim time from 19 July
systemData$date <- date(systemData$timestamp)
systemData <- systemData[systemData$date>="2019-07-19",]
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
write.csv(systemData, file="systemData_predicted_june_dec.csv", row.names=FALSE)

#####Read the predicted data
system_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Hall/systemData_predicted_june_dec.csv",
                        header=TRUE, stringsAsFactors = FALSE)
system_data$timestamp <- as.POSIXct(system_data$timestamp, format="%Y-%m-%d %H:%M")
system_data$date <- as.Date(system_data$date)
system_data$time <- format(system_data$timestamp, format='%H')
#Consider only hours where all values are available
system_data <- system_data[complete.cases(system_data),]
system_data$month <- as.character(month(system_data$date, abbr=TRUE,label=TRUE))
system_gather <- gather(system_data, "id", "value", 2:8)

typical_hall_pred <- system_gather %>%
  group_by(month, time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_hall_pred <- as.data.frame(typical_hall_pred)
typical_hall_pred <- spread(typical_hall_pred, id, value)

###Plotting data for a typical day in each month
plotTypical <- function(df,p) {
  df %>%
    ggplot(aes(x=as.numeric(time))) +
    geom_line(aes(y = AC_consumption_kW, color = "AC Load", group="AC Load"), linetype=1) + 
    geom_point(aes(y = AC_consumption_kW, color = "AC Load"), shape=1) + 
    geom_line(aes(y = PV_power_kW, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
    geom_point(aes(y = PV_power_kW, color = "Actual PV Power"), shape=2) + 
    geom_line(aes(y = Capture_losses_kW, color = "Capture losses", group="Capture losses"), linetype=3) + 
    geom_point(aes(y = Capture_losses_kW, color = "Capture losses"), shape=3) + 
    geom_line(aes(y = Charged_energy_kW, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
    geom_point(aes(y = Charged_energy_kW, color = "Charged Energy"), shape=4) + 
    geom_line(aes(y = Discharged_energy_kW, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
    geom_point(aes(y = Discharged_energy_kW, color = "Discharged Energy"), shape=5) + 
    geom_line(aes(y = Potential_PV_power_kW, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
    geom_point(aes(y = Potential_PV_power_kW, color = "Pot. PV Power"), shape=6) + 
    geom_line(aes(y = State_of_charge/p, color = "State of charge", group="State of charge"), linetype=7) + 
    geom_point(aes(y = State_of_charge/p, color = "State of charge"), shape=7) +
    scale_y_continuous(sec.axis = sec_axis(~.*p, name = "State of charge (%)")) +
    labs(y="Energy (kWh)",
         x = "Time of day (00-23 hours)", 
         colour="Parameter") +
    scale_x_continuous(labels=c("0","2","4","6","8","10","12","14","16","18",
                                "20","22","24"),
                       breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
    theme(plot.title = element_text(size=10), legend.position = "bottom",
          legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0)) 
}

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Jul", ], 55.0)
p + labs(title = "Predicted Hall power profile for a typical day in Jul 2019")

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Aug", ], 55.0)
p + labs(title = "Predicted Hall power profile for a typical day in Aug 2019")

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Sep", ], 55.0)
p + labs(title = "Predicted Hall power profile for a typical day in Sep 2019")

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Oct", ], 80.0)
p + labs(title = "Predicted Hall power profile for a typical day in Oct 2019")

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Nov", ], 75.0)
p + labs(title = "Predicted Hall power profile for a typical day in Nov 2019")

p <- plotTypical(typical_hall_pred[typical_hall_pred$month=="Dec", ], 75.0)
p + labs(title = "Predicted Hall power profile for a typical day in Dec 2019")

#########Combining data for first and next 3 months ####################
system_gather$month2 <- month(system_gather$date)
system_gather$phase <- ifelse(system_gather$month2<10, 1, 2)
typical_phase_pred <- system_gather %>%
  group_by(phase, time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_phase_pred <- as.data.frame(typical_phase_pred)
typical_phase_pred <- spread(typical_phase_pred, id, value)

p <- plotTypical(typical_phase_pred[typical_phase_pred$phase==1, ], 62.0)
p + labs(title = "Predicted Hall power profile for a typical day in Jul-Sep 2019")

p <- plotTypical(typical_phase_pred[typical_phase_pred$phase==2, ], 80.0)
p + labs(title = "Predicted Hall power profile for a typical day in Oct-Dec 2019")

####Combining all months #############################################
typical_hall <- system_gather %>%
  group_by(time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_hall <- as.data.frame(typical_hall)
typical_hall <- spread(typical_hall, id, value)

p <- plotTypical(typical_hall, 70.0)
p + labs(title = "Predicted Hall power profile for a typical day in Jul-Dec 2019")

########################################################################
# Calculating AC load and user load values for Mesh Power
library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)
setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/")
data <- read.csv("hall_data_hourly_jul_dec.csv", header=TRUE)
data$date <- as.Date(data$date)
data <- data[,c(1:3,5:25,30,32,34,36)]
data$User_Load_W <- rowSums(data[,c(4:28)])
# Only keep complete cases
data <- data[complete.cases(data),]
data$timestamp <- ifelse(data$timestamp<10, paste(data$date, paste("0",data$timestamp,":00:00",sep="")), 
                            paste(data$date,paste(data$timestamp,":00:00", sep=""), sep=" "))
data <- data[,-1] #remove date
write.csv(data,"./hall_hourly_load_jul_dec.csv", row.names=FALSE)
