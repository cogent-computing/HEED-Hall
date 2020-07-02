#Uploading data on data portal

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
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
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
    } else if(j==7) {
      setwd("../01 2020/")
    } else if(j==8) {
      setwd("../02 2020/")
    } else if(j==9) {
      setwd("../03 2020/")
    }
    
    #For each month read all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0] #Ignore empty files
    #Read each file in the list and append to cpe with the correct label
    if(length(file_list)>0) { #If directory is empty
      for(i in 1:length(file_list)) {
        df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
        colnames(df) <- c("timestamp", "variable", "value")
        df$value <- as.character(df$value)
        df$variable <- as.character(df$variable)
        
        #Add ID to each file
        df$id <- rep(paste("CPE",k, sep=""), length(df$timestamp))
        
        #Add to cpe 
        cpe <- rbind(cpe, df)
      }
    }
  }
}
setwd("~/OneDrive - Coventry University/Data portal/Data uploads/Hall/")
write.csv(cpe, file="CPE_all_raw.csv", row.names=FALSE)

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
  
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
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
    } else if(j==7) {
      setwd("../01 2020/")
    } else if(j==8) {
      setwd("../02 2020/")
    } else if(j==9) {
      setwd("../03 2020/")
    }
    
    #For each month list all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0] #Ignore empty files
    #Read each file in the list and append to cpe with the correct label
    if(length(file_list)>0) { #If directory is empty
      for(i in 1:length(file_list)) {
        df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
        colnames(df) <- c("timestamp", "variable", "value")
        df$value <- as.character(df$value)
        df$variable <- as.character(df$variable)
        
        #Add ID to each file
        df$id <- rep(paste("S",k, sep=""), length(df$timestamp))
        
        #Add to cpe 
        sockets <- rbind(sockets, df)
      }
    }
  }
}
setwd("~/OneDrive - Coventry University/Data portal/Data uploads/Hall/")
write.csv(sockets, file="Sockets_all_raw.csv", row.names=FALSE)

#########################################################################################

################################ MICROGIRD #############################################
#Read in all CPE files
cpe <- data.frame()
for(k in 1:20) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/")
  if(k==1) {
    str = "Nur 1A CPE1"
    setwd("./Nur 1A CPE1/")
  } else if(k==2) {
    str = "Nur 1A CPE2"
    setwd("./Nur 1A CPE2/")
  } else if(k==3) {
    str = "Nur 1B CPE3"
    setwd("./Nur 1B CPE3/")
  } else if(k==4) {
    str = "Nur 1B CPE4"
    setwd("./Nur 1B CPE4/")
  } else if(k==5) {
    str = "Nur 1C CPE5"
    setwd("./Nur 1C CPE5/")
  } else if(k==6) {
    str = "Nur 1C CPE6"
    setwd("./Nur 1C CPE6/")
  } else if(k==7) {
    str = "Nur 2A CPE7"
    setwd("./Nur 2A CPE7/")
  } else if(k==8) {
    str = "Nur 2A CPE8"
    setwd("./Nur 2A CPE8/")
  } else if(k==9) {
    str = "Nur 2B CPE9"
    setwd("./Nur 2B CPE9/")
  } else if(k==10) {
    str = "Nur 2B CPE10"
    setwd("./Nur 2B CPE10/")
  } else if(k==11) {
    str = "Nur 2C CPE11"
    setwd("./Nur 2C CPE11/")
  } else if(k==12) {
    str = "Nur 2C CPE12"
    setwd("./Nur 2C CPE12/")
  } else if(k==13) {
    str = "Playground CPE1"
    setwd("./Playground CPE1/")
  } else if(k==14) {
    str = "Playground CPE2"
    setwd("./Playground CPE2/")
  } else if(k==15) {
    str = "Playground CPE3"
    setwd("./Playground CPE3/")
  } else if(k==16) {
    str = "Playground CPE4"
    setwd("./Playground CPE4/")
  } else if(k==17) {
    str = "Playground CPE5"
    setwd("./Playground CPE5/")
  } else if(k==18) {
    str = "Streetlight 1 CPE"
    setwd("./Streetlight 1 CPE/")
  } else if(k==19) {
    str = "Streetlight 2 CPE"
    setwd("./Streetlight 2 CPE/")
  } else if(k==20) {
    str = "Streetlight 3 CPE"
    setwd("./Streetlight 3 CPE/")
  }
  
  #For each CPE - read files for each month - July, Aug, Sep, Oct, Nov, Dec, Jan, Feb, Mar
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
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
    } else if(j==7) {
      setwd("../01 2020/")
    } else if(j==8) {
      setwd("../02 2020/")
    } else if(j==9) {
      setwd("../03 2020/")
    }
    
    #For each month read all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0]
    #Read each file in the list and append to cpe with the correct label
    if(length(file_list)>0) { #If directory is empty
      for(i in 1:length(file_list)) {
        df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
        colnames(df) <- c("timestamp", "variable", "value")
        df$value <- as.character(df$value)
        df$variable <- as.character(df$variable)
        
        #Add ID to each file
        df$id <- rep(str, length(df$timestamp))
        
        #Add to cpe 
        cpe <- rbind(cpe, df)
      }
    }
  }
}
setwd("~/OneDrive - Coventry University/Data portal/Data uploads/MG/")
write.csv(cpe, file="CPE_all_raw.csv", row.names=FALSE)

#############################################################################
#Reading all socket files
sockets <- data.frame()
for(k in 1:10) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/")
  if(k==1) {
    str = "Nur 1A S1"
    setwd("./Nur 1A S1/")
  } else if(k==2) {
    str = "Nur 1A S2"
    setwd("./Nur 1A S2/")
  } else if(k==3) {
    str = "Nur 1B S1"
    setwd("./Nur 1B S1/")
  } else if(k==4) {
    str = "Nur 1C S1"
    setwd("./Nur 1C S1/")
  } else if(k==5) {
    str = "Nur 2A S1"
    setwd("./Nur 2A S1/")
  } else if(k==6) {
    str = "Nur 2A S2"
    setwd("./Nur 2A S2/")
  } else if(k==7) {
    str = "Nur 2B S1"
    setwd("./Nur 2B S1/")
  } else if(k==8) {
    str = "Nur 2C S1"
    setwd("./Nur 2C S1/")
  } else if(k==9) {
    str = "Playground S1"
    setwd("./Playground S1/")
  } else if(k==10) {
    str = "Playground S2"
    setwd("./Playground S2/")
  } 
  
  #For each socket - read files for each month - July, Aug, Sep, Oct, Nov, Dec, Jan, Feb, Mar
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
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
    } else if(j==7) {
      setwd("../01 2020/")
    } else if(j==8) {
      setwd("../02 2020/")
    } else if(j==9) {
      setwd("../03 2020/")
    }
    
    #For each month list all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0]
    #Read each daily file in the list and append to sockets with the correct label
    if(length(file_list)>0) {
      for(i in 1:length(file_list)) {
        df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
        colnames(df) <- c("timestamp", "variable", "value")
        df$value <- as.character(df$value)
        df$variable <- as.character(df$variable)
        
        #Add ID to each file
        df$id <- rep(str, length(df$timestamp))
        
        #Add to cpe 
        sockets <- rbind(sockets, df)
      } 
    }
  }
}
setwd("~/OneDrive - Coventry University/Data portal/Data uploads/MG/")
write.csv(sockets, file="Sockets_all_raw.csv", row.names=FALSE)

#################################################################################################

#################################################################################################
########################### Nepal streetlights ##############################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Full/")
file_list <- list.files()
sl <- data.frame()
for(k in 1:length(file_list)) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  
  if(k!=6) {
    df$timestamp <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01")
  } else {
    df$timestamp <- as.POSIXct(df$timestamp,format="%d/%m/%Y %H:%M", tz="GMT")
  }
  
  df <- gather(df, "variable", "value", 2:length(df))
  
  #Add the Streetlight ID to the data
  df$streetlight <- rep(gsub("_.*","",file_list[k]), length(df$timestamp))
  
  #Bind to sl data frame
  sl <- rbind(sl, df)
}

setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl2 <- data.frame()
for(k in 1:6) {
  #Read files for each SL
  setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
  if(k==1) {
    setwd("./SL1/")
  } else if(k==2) {
    setwd("./SL2/")
  } else if(k==3) {
    setwd("./SL3/")
  } else if(k==4) {
    setwd("./SL4/")
  } else if(k==5) {
    setwd("./SL6/")
  } else if(k==6) {
    setwd("./SL7/")
  }
  
  #Input the file for each SL
  file_list <- list.files()
  #SD card files have SD in it and are xlsx - 2 line header
  #Victron files have log in it and are csv - 3 line header
  for(i in 1:length(file_list)) {
    name <- file_list[i]
    
    #Check for the phrase SD in the name 
    if(grepl("SD", name, fixed=TRUE)) {
      #Concatenate the headers spread across first 2 rows
      headers <- read_excel(file_list[i], col_names = FALSE, na="..", n_max = 2)
    } else {
      #Concatenate the headers spread across first 3 rows
      headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
    }
    #Replace NA in header with "" for missing row 3 values
    headers[is.na(headers)] <- ""
    column_labels <- headers %>% summarize_all(str_c, collapse = " ")
    headers = unname(unlist(column_labels[1,]))
    
    #Check for the phrase SD in the name 
    if(grepl("SD", name, fixed=TRUE)) {
      #Read data without the first two rows
      df <- read_excel(file_list[i], col_names = headers, na="..", skip = 2)
    } else {
      #Read data without the first three rows
      df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
    }
    #Replace NA in data frame with "" for missing values as in raw file
    df[is.na(df)] <- ""
    
    #Check for the timezone of the timestamp - convert to Kathmandu = GMT+5:45
    if(grepl("GMT", colnames(df)[1], fixed=TRUE)) {
      df[,1] <- df[,1] %m+% hours(5) %m+% minutes(45)
    } else if(grepl("India", colnames(df)[1], fixed=TRUE)) {
      df[,1] <- df[,1] %m+% hours(1)
    }
    
    df <- gather(df, "variable", "value", 2:length(df))
    colnames(df) <- c("timestamp", "variable", "value")
    
    #Add the Streetlight ID to the data
    df$streetlight <- rep(substr(name,6,8), length(df$timestamp))
    
    #Bind to sl data frame
    sl2 <- rbind(sl2, df)
  }
}

setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Oct2019_Mar2020/")
file_list <- list.files()
sl3 <- data.frame()
for(k in 1:length(file_list)) {
  headers <- read_csv(file_list[k], col_names = FALSE, na="..", n_max = 3)
  #Replace NA in header with "" for missing row 3 values
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  df <- read_csv(file_list[k], col_names = headers, na="..", skip = 3)
  df[is.na(df)] <- ""
  
  #Select dates for the system_hourly data
  if(k==1) {
    df <- df[date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-15" &
               date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19",]
  } else if(k==2 | k==3 | k==4 | k==5) {
    df <- df[date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-05" &
               date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19",]
  } else if(k==6) {
    df <- df[(date(df$`timestamp Asia/Katmandu (+05:45) `)>="2019-10-16" &
                date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-01-28") | 
               (date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-05" & 
                  date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19"),]
  }
  
  df <- gather(df, "variable", "value", 2:length(df))
  colnames(df) <- c("timestamp", "variable", "value")
  #Add the Streetlight ID to the data
  df$streetlight <- rep(substr(file_list[k],6,8), length(df$timestamp))
  
  #Bind to sl data frame
  sl3 <- rbind(sl3, df)
}

sl_all <- data.frame()
sl_all <- rbind(sl_all, sl, sl2, sl3)
sl_all <- distinct(sl_all)
sl_all <- sl_all[order(sl_all$timestamp),]
sl_all$date <- date(sl_all$timestamp)
#Select dates greater than 1st July
sl_all <- sl_all[sl_all$date>="2019-07-01",]
sl_all <- sl_all[,-5] #Remove date

##Create files for each streetlight
sl1 <- sl_all[sl_all$streetlight=="SL1",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL1.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL2",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL2.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL3",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL3.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL4",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL4.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL5",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL5.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL6",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL6.csv",
          row.names=FALSE)

sl1 <- sl_all[sl_all$streetlight=="SL7",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Nepal/Nepal_SL7.csv",
          row.names=FALSE)

#################################################################################################

#################################################################################################
########################### Rwanda streetlights ##############################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/Full/")
file_list <- list.files()
sl_rwanda <- data.frame()
for(k in 1:length(file_list)) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  df$timestamp <- as.POSIXct(df$timestamp, tz="GMT",origin="1970-01-01")
  df <- gather(df, "variable", "value", 2:length(df))
  
  #Add the Streetlight ID to the data
  df$streetlight <- rep(gsub("_.*","",file_list[k]), length(df$timestamp))
  
  #Bind to sl data frame
  sl_rwanda <- rbind(sl_rwanda, df)
}

setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
sl_rwanda2 <- data.frame()
for(k in 1:4) {
  #Read files for each SL
  setwd("~/OneDrive - Coventry University/HEED_analysis/Rwanda Streetlights/Data/")
  if(k==1) {
    setwd("./SL1/")
  } else if(k==2) {
    setwd("./SL2/")
  } else if(k==3) {
    setwd("./SL3/")
  } else if(k==4) {
    setwd("./SL4/")
  }
  
  #Input the file for each SL
  file_list <- list.files()
  #SD card files have SD in it and are xlsx - 2 line header
  #Victron files have log in it and are csv - 3 line header
  for(i in 1:length(file_list)) {
    
    headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
    headers[is.na(headers)] <- ""
    column_labels <- headers %>% summarize_all(str_c, collapse = " ")
    headers = unname(unlist(column_labels[1,]))
    headers[1] <- "timestamp"
    
    df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
    df[is.na(df)] <- ""
    
    df <- gather(df, "variable", "value", 2:length(df))
    
    #Add the Streetlight ID to the data
    df$streetlight <- rep(substr(file_list[i],7,9), length(df$timestamp))
    
    #Bind to sl data frame
    sl_rwanda2 <- rbind(sl_rwanda2, df)
  }
}

sl_all_rwanda <- data.frame()
sl_all_rwanda <- rbind(sl_all_rwanda, sl_rwanda, sl_rwanda2)
sl_all_rwanda <- sl_all_rwanda[order(sl_all_rwanda$timestamp),]
write.csv(sl_all_rwanda, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Rwanda/Rwanda_SL_raw.csv",
          row.names=FALSE)
sl_all_rwanda$date <- date(sl_all_rwanda$timestamp)

#Select dates greater than 1st July
sl_all <- sl_all[sl_all$date>="2019-07-01",]
sl_all <- sl_all[,-5] #Remove date

##Create files for each streetlight
sl1 <- sl_all_rwanda[sl_all_rwanda$streetlight=="SL1",]
write.csv(sl1, "~/OneDrive - Coventry University/Data portal/Data uploads/SL_Rwanda/Rwanda_SL1.csv",
          row.names=FALSE)


#################################################################################################