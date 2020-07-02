library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

#Extract hourly means for LED[1,2,3]_P values (mW) from the CPE1-CPE7 files and convert to W
cpe1 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE1/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE1/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE1/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0 #Converting to W
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe1 <- rbind(cpe1, df)
  }
}
cpe1$id <- rep("CPE1", length(cpe1$value))

cpe2 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE2/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE2/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE2/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe2 <- rbind(cpe2, df)
  }
}
cpe2$id <- rep("CPE2", length(cpe2$value))

cpe3 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE3/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE3/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE3/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe3 <- rbind(cpe3, df)
  }
}
cpe3$id <- rep("CPE3", length(cpe3$value))

cpe4 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE4/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE4/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE4/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe4 <- rbind(cpe4, df)
  }
}
cpe4$id <- rep("CPE4", length(cpe4$value))

cpe5 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE5 (outdoor lights)/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE5 (outdoor lights)/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE5 (outdoor lights)/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe5 <- rbind(cpe5, df)
  }
}
cpe5$id <- rep("CPE5", length(cpe5$value))

cpe6 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE6/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE6/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE6/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe6 <- rbind(cpe6, df)
  }
}
cpe6$id <- rep("CPE6", length(cpe6$value))

cpe7 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE7/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE7/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall CPE7/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption variables only - mW
    df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
    df_sub$value <- as.numeric(df_sub$value)/1000.0
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for each variable - led1_p, led2_p, led3_p
    df2 <- df_sub %>%
      group_by(variable, timeUse) %>%
      summarise(value = mean(value))
    
    #Add date from the file name
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    cpe7 <- rbind(cpe7, df)
  }
}
cpe7$id <- rep("CPE7", length(cpe7$value))

#Combining all CPEs
cpe_all <- data.frame()
cpe_all <- rbind(cpe_all, cpe1, cpe2, cpe3, cpe4, cpe5, cpe6, cpe7)
cpe_all <- cpe_all[order(cpe_all$date),]

#Extract hourly mean for vRELAY1_VL (W) and hourly total for AC_Day_Energy_session (Wm) values from S1-S4 files 
s1 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S1/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S1/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S1/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
    df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
    df_sub$value <- as.numeric(df_sub$value)
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for instantaneous power
    df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
      group_by(timeUse) %>%
      summarise(value = mean(value))
    df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
    
    #Correct cumulative total energy if not reset
    df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
    if(df3_sub$value[1]!=0) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      for(l in 2:length(df3_sub$timestamp)) {
        if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
          flag=TRUE
          index <- l
        }
      }
      if(flag==FALSE) { #Means the value never reset to 0
        #Subtract value at 0th index from all values
        df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
      } else {
        df3_sub$value2 <- df3_sub$value
        df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
        df3_sub$value2[index:length(df3_sub$timestamp)] <- 
          df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
      }
    } else {
      df3_sub$value2 <- df3_sub$value
    }
    
    #Extract cumulative total energy per hour
    df3 <- df3_sub %>%
      group_by(timeUse) %>%
      summarise(value = value2[length(value2)])
    df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
    
    #Bind the 2 data frames and add date from the file name
    df2 <- rbind(df2, df3)
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    s1 <- rbind(s1, df)
  }
}
s1$id <- rep("S1", length(s1$value))

s2 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S2/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S2/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S2/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
    df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
    df_sub$value <- as.numeric(df_sub$value)
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for instantaneous power
    df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
      group_by(timeUse) %>%
      summarise(value = mean(value))
    df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
    
    #Correct cumulative total energy if not reset
    df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
    if(df3_sub$value[1]!=0) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      for(l in 2:length(df3_sub$timestamp)) {
        if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
          flag=TRUE
          index <- l
        }
      }
      if(flag==FALSE) { #Means the value never reset to 0
        #Subtract value at 0th index from all values
        df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
      } else {
        df3_sub$value2 <- df3_sub$value
        df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
        df3_sub$value2[index:length(df3_sub$timestamp)] <- 
          df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
      }
    } else {
      df3_sub$value2 <- df3_sub$value
    }
    
    #Extract cumulative total energy per hour
    df3 <- df3_sub %>%
      group_by(timeUse) %>%
      summarise(value = value2[length(value2)])
    df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
    
    #Bind the 2 data frames and add date from the file name
    df2 <- rbind(df2, df3)
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    s2 <- rbind(s2, df)
  }
}
s2$id <- rep("S2", length(s2$value))

s3 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S3/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S3/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S3/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
    df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
    df_sub$value <- as.numeric(df_sub$value)
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for instantaneous power
    df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
      group_by(timeUse) %>%
      summarise(value = mean(value))
    df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
    
    #Correct cumulative total energy if not reset
    df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
    if(df3_sub$value[1]!=0) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      for(l in 2:length(df3_sub$timestamp)) {
        if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
          flag=TRUE
          index <- l
        }
      }
      if(flag==FALSE) { #Means the value never reset to 0
        #Subtract value at 0th index from all values
        df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
      } else {
        df3_sub$value2 <- df3_sub$value
        df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
        df3_sub$value2[index:length(df3_sub$timestamp)] <- 
          df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
      }
    } else {
      df3_sub$value2 <- df3_sub$value
    }
    
    #Extract cumulative total energy per hour
    df3 <- df3_sub %>%
      group_by(timeUse) %>%
      summarise(value = value2[length(value2)])
    df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
    
    #Bind the 2 data frames and add date from the file name
    df2 <- rbind(df2, df3)
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    s3 <- rbind(s3, df)
  }
}
s3$id <- rep("S3", length(s3$value))

s4 <- data.frame()
for(j in 1:3) {
  if(j==1) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S4/08 2019/")
  } else if(j==2) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S4/09 2019/")
  } else if(j==3) {
    setwd("~/OneDrive - Coventry University/HEED_analysis/Hall/Hall S4/10 2019/")
  }
  file_list <- list.files()
  for(i in 1:length(file_list)) {
    df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
    colnames(df) <- c("timestamp", "variable", "value")
    df$value <- as.character(df$value)
    df$variable <- as.character(df$variable)
    
    #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
    df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
    df_sub$value <- as.numeric(df_sub$value)
    
    #Add time - we need to extract hours to get hourly means
    df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
    df_sub$timeUse <- format(df_sub$time, format='%H')
    
    #Extract hourly means for instantaneous power
    df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
      group_by(timeUse) %>%
      summarise(value = mean(value))
    df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
    
    #Correct cumulative total energy if not reset
    df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
    if(df3_sub$value[1]!=0) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      for(l in 2:length(df3_sub$timestamp)) {
        if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
          flag=TRUE
          index <- l
        }
      }
      if(flag==FALSE) { #Means the value never reset to 0
        #Subtract value at 0th index from all values
        df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
      } else {
        df3_sub$value2 <- df3_sub$value
        df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
        df3_sub$value2[index:length(df3_sub$timestamp)] <- 
          df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
      }
    } else {
      df3_sub$value2 <- df3_sub$value
    }
    
    #Extract cumulative total energy per hour
    df3 <- df3_sub %>%
      group_by(timeUse) %>%
      summarise(value = value2[length(value2)])
    df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
    
    #Bind the 2 data frames and add date from the file name
    df2 <- rbind(df2, df3)
    df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                        format = "%d-%m-%Y")
    
    #Create a data frame with the above values and bind to a complete df 
    df <- data.frame(date=df2$date, hour=df2$timeUse, 
                     variable=df2$variable, value=df2$value)
    df$hour <- as.character(df$hour)
    df$variable <- as.character(df$variable)
    s4 <- rbind(s4, df)
  }
}
s4$id <- rep("S4", length(s4$value))

#Combining all 4 sockets
s_all <- data.frame()
s_all <- rbind(s_all, s1, s2, s3, s4)
s_all <- s_all[order(s_all$date),]

#Extract hourly means for AC Consumption L1 (W), PV Power (W), State of charge (%), Discharged Energy (kWh), Charged Energy (kWh) for system data

#One file under full data from June to Nov - europe time zone
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
systemData$`timestamp Europe/Berlin (+01:00) ` <- systemData$`timestamp Europe/Berlin (+01:00) ` +
  1*60*60

#Reading in weather data - hourly values
filename <- "~/OneDrive - Coventry University/HEED_analysis/Hall/Weather data/weather_data.xlsx"
headers <- read_excel(filename, col_names = FALSE, na="..", n_max = 2)
#Replace NA in header with "" for missing row 3 values
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
weather_data <- read_excel(filename, col_names = headers, na="..", skip = 2,
                           col_types=c("date","numeric","numeric"))
#Replace NA in data frame with "" for missing values as in raw file
weather_data[is.na(weather_data)] <- ""
#Data is 12 years behind - change year to 2019
year(weather_data$`Time (2019!) `) <- 2019

###############################################################################
############################### RQ1 ###########################################

#Hourly energy consumption (all lights + sockets) from the hall for a week/weekend 
#day in (month)

#Combine hourly power consumption data for CPE and sockets
pc <- data.frame()
pc <- rbind(pc, cpe_all, s_all[s_all$variable=="vRELAY1_LVL",])
#Total energy consumption per hour per day
pc_sum <- pc %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
#Add month to separate plots per month
pc_sum$month <- as.character(month(pc_sum$date, label=TRUE, abbr=TRUE))

pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for weedkdays in Aug 2019
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekday in Aug'19" , 
       y="Power consumption (W)",
       x = "Hour of day")

pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekend day in Aug'19" ,
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for weedkdays in Sep 2019
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekday in Sep'19" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day in Sep 2019
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekend day in Sep'19" ,
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for weedkdays in Oct 2019
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekday in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day in Oct 2019
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of microgrid on a weekend day in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day")


###Hourly socket consumption at the hall for a weekend/week day in (month)

#Combine hourly power consumption data for sockets
pc_s <- data.frame()
pc_s <- rbind(pc_s, s1[s1$variable=="vRELAY1_LVL",],
              s2[s2$variable=="vRELAY1_LVL",],
              s3[s3$variable=="vRELAY1_LVL",],
              s4[s4$variable=="vRELAY1_LVL",])

#Adding energy consumption per hour per day
pc_sum <- pc_s %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
#Add month to separate plots per month
pc_sum$month <- as.character(month(pc_sum$date, label=TRUE, abbr=TRUE))

pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting sockets energy consumption per hour for a weekday in Aug 2019
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a week day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting sockets energy consumption per hour for a weekend day in Aug 2019
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting sockets energy consumption per hour for a weekday in Sep 2019
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a weekday in Sep 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting sockets energy consumption per hour for a weekend day in Sep 2019
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a weekend day in Sep 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting sockets energy consumption per hour for a weekday in Oct 2019
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a week day in Oct 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting sockets energy consumption per hour for a weekend day in Oct 2019
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of sockets on a weekend day in Oct 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

####Hourly indoor light consumption at the hall for a weekend/week day in (month)
#Combine hourly power consumption data for indoor lights: cpe1-cpe4; cpe6-cpe7
pc_indoor_cpe <- data.frame()
pc_indoor_cpe <- rbind(pc_indoor_cpe, cpe1, cpe2, cpe3, cpe4, cpe6, cpe7)

#Adding energy consumption per hour per day
pc_sum <- pc_indoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
#Add month to separate plots per month
pc_sum$month <- as.character(month(pc_sum$date, label=TRUE, abbr=TRUE))

pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting indoor lights energy consumption per hour for a weekday in Aug 2019
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a week day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting indoor lights energy consumption per hour for a weekend day in Aug 2019
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting indoor lights energy consumption per hour for a weekday in Sep 2019
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a weekday in Sep 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting indoor lights energy consumption per hour for a weekend day in Sep 2019
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a weekend day in Sep 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting indoor lights energy consumption per hour for a weekday in Oct 2019
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a weekday in Oct 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting indoor lights energy consumption per hour for a weekend day in Oct 2019
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of indoor lights on a weekend day in Oct 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

### Hourly outdoor light consumption at the hall for a weekend/week day in (month)
#Combine hourly power consumption data for indoor lights: cpe5
pc_outdoor_cpe <- data.frame()
pc_outdoor_cpe <- rbind(pc_outdoor_cpe, cpe5)

#Adding energy consumption per hour per day - same as cpe5 
pc_sum <- pc_outdoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
#Add month to separate plots per month
pc_sum$month <- as.character(month(pc_sum$date, label=TRUE, abbr=TRUE))

pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting outdoor lights energy consumption per hour for a weekday in Aug 2019
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday[pc_sum_wday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a week day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting outdoor lights energy consumption per hour for a weekend day in Aug 2019
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday[pc_sum_weday$month=="Aug",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a weekend day in Aug 2019" ,
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting outdoor lights energy consumption per hour for a weekday in Sep 2019
pc_sum_wday[pc_sum_wday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a week day in Sep 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting outdoor lights energy consumption per hour for a weekend day in Sep 2019
pc_sum_weday[pc_sum_weday$month=="Sep",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a weekend day in Sep 2019",
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting outdoor lights energy consumption per hour for a weekday in Oct 2019
pc_sum_wday[pc_sum_wday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a week day in Oct 2019", 
       y="Power consumption (W)",
       x = "Hour of day")
#Plotting outdoor lights energy consumption per hour for a weekend day in Oct 2019
pc_sum_weday[pc_sum_weday$month=="Oct",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of outdoor lights on a weekend day in Oct 2019",
       y="Power consumption (W)",
       x = "Hour of day")

#############################################################################
#################### RQ 2 ###################################################

#Daily total energy consumption of Hall per month and total energy consumption since commissioning.

#Daily consumption for CPE - sum of mean power consumed per hour for 24 hrs (Wh)
#Combine power consumed daily by each cpe
cpe_consumption <- cpe_all #This is hourly means
cpe_daily <- cpe_consumption %>%
  group_by(date, id) %>%
  summarise(value=sum(value))
#Combine power consumed daily by all cpe
cpe_total <- cpe_daily %>%
  group_by(date) %>%
  summarise(value=sum(value))
cpe_total$id <- rep("CPE", length(cpe_total$date))

#Socket energy consumption - for each day get max value
s_consumption <- s_all[s_all$variable=="AC_Day_Energy_session", ]
s_consumption$value <- s_consumption$value * 0.017 #Converting Wm to Wh
#Daily consumption by each socket 
s_hours <- s_consumption %>%
  group_by(date,id) %>%
  summarise(value = value[length(value)])
#Combine daily consumption for all sockets
s_total <- s_hours %>%
  group_by(date) %>%
  summarise(value = sum(value))
s_total$id <- rep("S", length(s_total$date))

#Total energy consumed per day by both lights and sockets
cpe_s_daily <- data.frame() 
cpe_s_daily <- rbind(cpe_s_daily, cpe_total, s_total)
cpe_s_daily <- cpe_s_daily[order(cpe_s_daily$date), ]

energy_total <- cpe_s_daily %>%
  group_by(date) %>%
  summarise(value=sum(value))

energy_total$month <- month(energy_total$date, label = TRUE, abbr=TRUE)
startDate <- as.Date("2019-07-01")
energy_total$days <- as.numeric(energy_total$date - startDate)

#Total daily consumption in Aug 2019
energy_total$date <- as.factor(energy_total$date)
energy_total[energy_total$month=="Aug",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Daily total energy consumption in Aug 2019" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=6, angle=45))

#Total daily consumption in Sep 2019
energy_total[energy_total$month=="Sep",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Daily total energy consumption in Sep 2019" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=6, angle=45))

#Total daily consumption in Oct 2019
energy_total[energy_total$month=="Oct",] %>%
  ggplot(aes(x = date, y= value)) +
  geom_bar(stat="identity", width=.5,fill="orange") + 
  labs(title="Daily total energy consumption in Oct 2019" , 
       y="Daily energy consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=6, angle=45))

#Total energy consumption since commissioning
energy_total %>%
  ggplot(aes(x=days, y=value, color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Total energy consumption since commissioning " , 
       y="Total consumption (Wh)",
       x = "Days since commissioning",
       colour="") 

#Getting daily average per month
energy_month <- energy_total %>%
  group_by(month) %>%
  summarise(value=mean(value))
energy_month$id <- rep("Actual", length(energy_month$month))

energy_month$id <- as.factor(energy_month$id)
energy_month %>%
  ggplot(aes(x=month, y=value, fill=id)) +
  geom_bar(stat="identity", width=.5, position="dodge") + 
  labs(title="Average total daily energy consumption" , 
       y="Daily average total energy consumption (Wh)",
       x = "Month", 
       fill="") 

# Daily total user consumption loads for the hall in (month).