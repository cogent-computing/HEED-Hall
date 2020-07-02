s <- sockets_v_i[sockets_v_i$id=="S1" & sockets_v_i$date=="2019-08-01",]
s$value[s$variable=="vRELAY1_I"] = s$value[s$variable=="vRELAY1_I"] / 1000.0
s$value[s$variable=="vRELAY1_V"] = s$value[s$variable=="vRELAY1_V"] / 1000.0
s$value[s$variable=="vRELAY1_VA"] = s$value[s$variable=="vRELAY1_VA"] / 1000000.0
s$value[s$variable=="vRELAY1_PF"] = s$value[s$variable=="vRELAY1_PF"] / 1000.0
s$value[s$variable=="AC_Day_Energy_session"] = 
  s$value[s$variable=="AC_Day_Energy_session"] * 0.017
s <- spread(s, variable, value, fill=NA)
s$VI <- s$vRELAY1_I * s$vRELAY1_V 
s$RP <- s$vRELAY1_VA * s$vRELAY1_PF 
s$VIPF <- s$vRELAY1_I * s$vRELAY1_V * s$vRELAY1_PF

s2 <- s

plot(s2$timeUse[1:24], s2$vRELAY1_VA[1:24], col="blue", type="o", pch=1, lty=3,
     ylim=c(0,15), xlab="Time",ylab="Power(W)",main="Real and Apparent power")
par(new=TRUE)
plot(s2$timeUse[1:24], s2$vRELAY1_LVL[1:24], col="black", type="o",
     ylim=c(0,15), xlab = NA,main="",ylab=NA,lty=6,pch=2)
par(new=TRUE)
plot(s2$timeUse[1:24], s2$VI[1:24], col="darkgreen", type="o",
     xlab = NA,main="",ylab=NA,ylim=c(0,15),lty=2,pch=3)
par(new=TRUE)
plot(s2$timeUse[1:24], s2$RP[1:24], col="darkorange",type="o", 
     xlab = NA,main="",ylab=NA,lty=4,ylim=c(0,15),pch=4)
par(new=TRUE)
plot(s2$timeUse[1:24], s2$VIPF[1:24], col="purple",type="o", 
     xlab = NA,main="",ylab=NA,lty=5,ylim=c(0,15),pch=5)
legend("topright", c("vRELAY1_VA","V*I","vRELAY1_LVL","VA*PF", "V*I*PF"),
       col=c("blue","darkgreen","black","darkorange","purple"),
       lty=c(3,2,6,4,5), pch=c(1,3,2,4,5))

#Find cumulative values for mean VA and mean LVL
testLVL <- s2$vRELAY1_LVL[1:24]
testVA <- s2$vRELAY1_VA[1:24]
testRP <- s2$RP[1:24]
testVIPF <- s2$VIPF[1:24]
va <- testVA
lvl <- testLVL
rp <- testRP
vipf <- testVIPF
for(i in 2:length(testVA)) {
  va[i] <- va[i] + va[i-1]
  lvl[i] <- lvl[i] + lvl[i-1]
  rp[i] <- rp[i] + rp[i-1]
  vipf[i] <- vipf[i] + vipf[i-1]
}
plot(s2$timeUse[1:24], s2$AC_Day_Energy_session[1:24], col="blue", type="o", pch=1,
     ylim=c(0,80), xlab="Time",ylab="Power(W)",main="Cumulative energy consumption for a day")
par(new=TRUE)
plot(s2$timeUse[1:24],lvl, col="black",type="o", 
     xlab = NA,main="",ylab=NA,ylim=c(0,80),lty=2,pch=2)
par(new=TRUE)
plot(s2$timeUse[1:24],va, col="purple",type="o", 
     xlab = NA,main="",ylab=NA,lty=4,ylim=c(0,80),pch=3)
par(new=TRUE)
plot(s2$timeUse[1:24],rp, col="orange",type="o", 
     xlab = NA,main="",ylab=NA,lty=5,ylim=c(0,80),pch=4)
par(new=TRUE)
plot(s2$timeUse[1:24],vipf, col="darkgreen",type="o", 
     xlab = NA,main="",ylab=NA,lty=6,ylim=c(0,80),pch=5)
legend("bottomright", c("AC_Day_Energy_Session","Cumulative LVL",
                        "Cumulative VA","Cumulative VA*PF","Cumulative V*I*PF"),
       col=c("blue","black","purple","orange","darkgreen"),lty=c(1,2,4,5,6),
       pch=c(1,2,3,4,5),cex=0.8, pt.cex = 1)

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

sockets_on_hours2_spread <- spread(sockets_on_hours2, variable, onHours)
##############################################################################
sockets_on_hours2[sockets_on_hours2$id=="S1",] %>%
  ggplot(aes(x=date, y=onHours, color=variable,shape=variable)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1")) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Hours of data collection per variable for socket S1" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Parameter",
       shape="Parameter")

sockets_on_hours2[sockets_on_hours2$id=="S2",] %>%
  ggplot(aes(x=date, y=onHours, color=variable,shape=variable)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1")) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Hours of data collection per variable for socket S2" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Parameter",
       shape="Parameter")

sockets_on_hours2[sockets_on_hours2$id=="S3",] %>%
  ggplot(aes(x=date, y=onHours, color=variable,shape=variable)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1")) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Hours of data collection per variable for socket S3" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Parameter",
       shape="Parameter")

sockets_on_hours2[sockets_on_hours2$id=="S4",] %>%
  ggplot(aes(x=date, y=onHours, color=variable,shape=variable)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1")) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title="Hours of data collection per variable for socket S4" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Parameter",
       shape="Parameter")
#############################################################################
hours2 <- sockets_on_hours2_spread


hours <- sockets_on_hours2[sockets_on_hours2$month=="Jul" 
                           & sockets_on_hours2$onHours<24,]


sockets_on_hours2[sockets_on_hours2$month=="Jul" 
                  & sockets_on_hours2$onHours<24,] %>%
  ggplot(aes(date, onHours, color=variable, shape=variable)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5,2,3)) +
  labs(title="Hours of data collection per day for each socket in Jul 2019" ,
       y="Number of hours of data collection",
       x = "Date" ,
       color="Parameter", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8)) +
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day")

hours <- sockets_on_hours2[sockets_on_hours2$onHours<24,]
hours[hours$variable=="AC_Day_Energy_session",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Energy Session" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours[hours$variable=="vRELAY1_I",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Current" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours[hours$variable=="vRELAY1_V",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Voltage" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours[hours$variable=="vRELAY1_PF",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Power Factor" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours[hours$variable=="vRELAY1_VA",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Apparent Power" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours[hours$variable=="vRELAY1_LVL",] %>%
  ggplot(aes(x=date, y=onHours, color=id)) +
  geom_point() + 
  scale_y_continuous(breaks=c(0,3,6,9,12,15,18,21,24), 
                     labels=c("0","3","6","9","12","15","18","21","24"),
                     limits=c(0,24)) +
  scale_shape_manual(values=c(0,4,1,3,5,2)) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange")) +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(title="Hours of data collection on incomplete days for Real Power" ,
       y="Number of hours of data collection",
       x = "Month" ,
       color="Sockets")

hours_summary <- hours %>%
  group_by(id, variable) %>%
  summarise(totalHours = sum(onHours))

hours_summary %>%
  ggplot(aes(id, totalHours, fill=variable)) +
  geom_bar(stat="identity", width=.5, position="dodge") +
  labs(title="Total missing hours per variable for all sockets" ,
       y="Total missing hours of data collection",
       x = "Socket ID" ,
       fill="Variable")

#Days on which lvl is missing, is energy day session or va and pf or v,i and pf non zero
indexLVL <- which(hours2$vRELAY1_LVL < 24)
indexV <- which(hours2$vRELAY1_V < 24)
indexI <- which(hours2$vRELAY1_I < 24)
indexVA <- which(hours2$vRELAY1_VA < 24)
indexPF <- which(hours2$vRELAY1_PF < 24)
indexSess <- which(hours2$AC_Day_Energy_session<24)

#total missing hours per variable
sum(24-hours2$vRELAY1_LVL[indexLVL])
sum(24-hours2$vRELAY1_V[indexV])
sum(24-hours2$vRELAY1_I[indexI])
sum(24-hours2$vRELAY1_VA[indexVA])
sum(24-hours2$vRELAY1_PF[indexPF])
sum(24-hours2$AC_Day_Energy_session[indexSess])

#Find overlap in the indexes
which(!(indexLVL %in% indexSess))
which(!(indexLVL %in% indexV))
which(!(indexLVL %in% indexI))
which(!(indexLVL %in% indexPF))
which(!(indexLVL %in% indexVA))

indexLVL[which(indexLVL %in% indexSess & indexLVL %in% indexV & indexLVL %in% indexI
      & indexLVL %in% indexPF & indexLVL %in% indexVA)]

#Plotting for index 1
test <- sockets_v_i[sockets_v_i$date==hours2$date[24],]
unique(test$timeUse[test$variable=="vRELAY1_LVL"])
unique(test$timeUse[test$variable=="vRELAY1_V"])
unique(test$timeUse[test$variable=="vRELAY1_I"])
unique(test$timeUse[test$variable=="vRELAY1_VA"])
unique(test$timeUse[test$variable=="vRELAY1_PF"])
unique(test$timeUse[test$variable=="AC_Day_Energy_session"])

##############################################################################
sockets_power <- sockets_sub[sockets_sub$variable=="AC_Day_Energy_session",]
sockets_power <- sockets_power[sockets_power$month=="Nov",]
s <- sockets_power[sockets_power$id=="S4",]
#Choose full days of data collection i.e. where length of data is 24.
s_sub <- data.frame()
for(i in 1:length(unique(s$date))) {
  df <- s[s$date==unique(s$date)[i], ]
  if(length(df$id)==24) {
    s_sub <- rbind(s_sub, df)
  }
}
#Get use each hour to see proportion of use per hour - get diff in value 
#from values that are reset at 0th hour of each day
s_sub$diff <- s_sub$value
s2 <- data.frame()
for(i in 1:length(unique(s_sub$date))) {
  df <- s_sub[s_sub$date==unique(s_sub$date)[i],]
  for(j in 2:length(df$id)) {
    df$diff[j] <- df$value[j] - df$value[j-1]
  }
  s2 <- rbind(s2, df)
}

#Once you have differences per hour - get them as proportion of change over the day
#Max change in a day is final value - value at 0th hour.
#Of this, how is the diff spread among different hours
s2$proportion <- s2$diff
s_sub <- data.frame()
for(i in 1:length(unique(s2$date))) {
  df <- s2[s2$date==unique(s2$date)[i],]
  maxDiff <- df$value[length(df$id)] 
  for(j in 1:length(df$id)) {
    df$proportion[j] <- df$diff[j] / maxDiff
  }
  s_sub <- rbind(s_sub, df)
}

#See the distribution of proportions across hours
s_sub$date2 <- as.factor(s_sub$date)
s_sub %>%
  ggplot(aes(x=timeUse, y=proportion, color=date2)) +
  geom_point()

library(wesanderson)

pal <- wes_palette("Zissou1", 100, type = "continuous")

heat_map <- ggplot(s_sub, aes(timeUse, date)) + geom_tile(aes(fill = proportion)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("Time of day") +
  ylab("Date of month") +
  ggtitle("Proportion of energy use per hour")
heat_map

#Get average proportions for the month
prop <- s_sub %>%
  group_by(month, timeUse) %>%
  summarise(proportion = mean(proportion, na.rm=TRUE))
plot(prop$timeUse, prop$proportion)

#Fill in the missing values by taking all data raw