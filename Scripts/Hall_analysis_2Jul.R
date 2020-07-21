#******************************************************************************************#
# This is the script for analysing corrected data for Hall                                 #
# Author: K Bhargava                                                                       #
# Last updated on: 21st July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
MONTHS <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
THEME <- theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
               legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
               panel.grid.major.y = element_line(colour="grey"), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=9, family="Times New Roman"),
               axis.title = element_text(size=10, family="Times New Roman")) 
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 5"
#******************************************************************************************#

#******************************************************************************************#
# Read in data files
imputed_data <- read.csv(here(filepath,"na_seadec_correctedData.csv"), 
                                  header=TRUE, stringsAsFactors = FALSE)
imputed_data <- imputed_data %>% mutate(date=as.Date(date),
                                        timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
                                        month = factor(month, levels = MONTHS, labels = MONTHS))

# Predicted load 
predicted_data <- data.frame(month=rep("Predicted",24),timeUse = c(0:23), 
                             User.Load.W=c(0,0,0,0,0,0,0.2,0.2,0.1,0.1,0.1,0.6,0.5,0.5,
                                           0.6,0.1,0.7,0.7,0.2,0.2,0.2,0,0,0)*1000,
                             stringsAsFactors = FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Calculate daily data - original, imputed and actual data
na_seadec_sub <- imputed_data[,-c(9:10,11:12,72:73,78:79,117,120)] # Remove SoC, voltage, system and solar battery power (+actual)
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:114))
system_daily <- na_seadec_sub %>% group_by(id, month, date) %>% summarise(value=sum(value, na.rm=TRUE))
system_daily <- as.data.frame(system_daily)

# Calculate daily value of SoC -  take mean for the day
na_seadec_sub <- imputed_data[,c(1:4,9:10)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, 5:6)
system_daily_soc <- na_seadec_sub %>% group_by(id, month, date) %>%summarise(value=mean(value, na.rm=TRUE))
system_daily_soc <- as.data.frame(system_daily_soc)

# Bind data sets
system_daily <- rbind(system_daily, system_daily_soc)

system_daily <- spread(system_daily, id, value)
system_daily <- system_daily %>% mutate(month=as.character(month))
write.csv(system_daily, file=here(filepath,"system_daily_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#*****************************************************************************************#
# Monthly daily avg - 20th July 2019 to 31st Mar 2020
system_daily <- read.csv(here(filepath,"system_daily_correctedData.csv"), header=TRUE, 
                         stringsAsFactors=FALSE)
system_daily <- system_daily %>% mutate(date=as.Date(date), 
         month=factor(month, levels = MONTHS, labels = MONTHS))
system_daily <- gather(system_daily, id, value, 3:114)
system_monthly <- system_daily %>% group_by(id, month) %>% summarise(value=mean(value, na.rm=TRUE))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to kW
system_monthly <- system_monthly %>% mutate(value=ifelse(id=="Battery.Monitor.State.of.charge.._ma" | 
                        id=="Battery.Monitor.State.of.charge.._original", value, value/1000.0))
# Consider absolute values for all variables
system_monthly <- spread(system_monthly, id, value)
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Plot 6 - Typical user load (socket and lights) at Hall between 20th July and 31st Mar
# Get actual CPE and vRELAY data and add values to get actual load
na_seadec_sub <- imputed_data[,c(1:3, which(grepl("Actual", colnames(imputed_data), fixed=TRUE)))]
na_seadec_sub <- na_seadec_sub[, c(1:3, which(grepl("CPE", colnames(na_seadec_sub), fixed=TRUE) |
                                               grepl("vRELAY", colnames(na_seadec_sub), fixed=TRUE)))]
# Calculate user load - actual and predicted
na_seadec_sub <- na_seadec_sub %>% mutate(Actual.User.Load.W=rowSums(na_seadec_sub[,c(4:28)]),
                               Predicted.User.Load.W=rep(predicted_data$User.Load.W,256))

typical_load <- na_seadec_sub %>% group_by(month, timeUse) %>% 
  summarise(User.Load.W = mean(Actual.User.Load.W))
typical_load <- as.data.frame(typical_load)
typical_load <- rbind(typical_load, predicted_data)

"Typical day load profile at the Hall between Jul'19 and Mar'20"
ggplot(typical_load, aes(timeUse, User.Load.W/1000.0, color=month)) + 
  geom_line(aes(linetype=month)) + scale_x_continuous(breaks=seq(0,24,2)) + THEME + 
  scale_y_continuous(breaks=seq(0,0.7,0.1)) + theme(legend.text=element_text(size=7, family="Times New Roman"))+
  labs(x="Time of day", y="User load (kW)",color="", linetype="")
ggsave(here(plot_dir,"typical_load_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 8a - User demand over and under predictions at the hall (a)
# Calculate over or under prediction for each instance
na_seadec_sub <- na_seadec_sub %>% 
  mutate(Diff=Predicted.User.Load.W-Actual.User.Load.W,
         Diff_range=ifelse(Diff<(-500),1,ifelse(Diff>=(-500) & Diff<(-400), 2, 
         ifelse(Diff>=(-400) & Diff<(-300), 3, ifelse(Diff>=(-300) & Diff<(-200), 4,
         ifelse(Diff>=(-200) & Diff<(-100), 5, ifelse(Diff>=(-100) & Diff<0, 6, 
         ifelse(Diff>=0 & Diff<100, 7, ifelse(Diff>=100 & Diff<200, 8,
         ifelse(Diff>=200 & Diff<300, 9, ifelse(Diff>=300 & Diff<400, 10,
         ifelse(Diff>=400 & Diff<500, 11, 12))))))))))))

# "Over and under predictions of user load at Hall between Jul'19 and Mar'20"
ggplot(na_seadec_sub, aes(timeUse, Diff/1000.0, color=month, shape=month)) +
  geom_point() + scale_shape_manual(values=c(1,4,1,4,1,4,1,4,1)) + 
  labs(x="Time of day", y="Prediction error (kW)", color="", shape="") +
  scale_x_continuous(breaks=seq(0,24,2)) + scale_y_continuous(breaks=seq(-1,0.75,0.25)) + 
  THEME + geom_hline(aes(yintercept=0), color="black", linetype="dashed") +
  theme(legend.text=element_text(size=7, family="Times New Roman"))
ggsave(here(plot_dir,"diffPred_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

pal <- wes_palette("Zissou1", 12, type = "continuous")
# title="Over and under predictions of user load at Hall between Jul'19 and Mar'20"
ggplot(na_seadec_sub, aes(date, timeUse)) + geom_tile(aes(fill = Diff_range)) + 
  scale_fill_gradientn(colours = pal, breaks=c(1,3,6,9,12), labels = c("<-0.5", ">-0.4 & <-0.3",">-0.1 & <0", ">0.2 & <0.3", ">0.5")) + 
  scale_y_continuous(breaks=seq(0,24,by=3)) + xlab("X axis") + ylab("Y axis") + 
  labs(y="Time of day", x = "DATE", fill="Error (kW)") + THEME + 
  guides(fill = guide_colorbar(barheight = 8, barwidth = 0.5)) +
  theme(legend.text=element_text(size=7, family="Times New Roman"), legend.position = "right",
        legend.title = element_text(size=7, family="Times New Roman"))
ggsave(here(plot_dir,"diffPred_tile_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 9a - Daily AC consumption values since commissioning the hall (a)
system_daily <- spread(system_daily, id, value)
system_daily <- system_daily %>% mutate(days=as.numeric(date - as.Date("2019-07-19")))
# title="Daily AC consumption at the Hall between Jul'19 and Mar'20" 
ggplot(system_daily, aes(days, Actual.AC.consumption_ma/1000.0, color="Actual")) +
  geom_point(shape=4) + labs(y="AC consumption (kW)", x = "Days since commissioning", colour="") + THEME +
  theme(legend.position="none") + scale_x_continuous(breaks = seq(1,260,28)) 
ggsave(here(plot_dir,"daily_acLoad_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 10a-c - Typical day showing key power generation and consumption parameters at Hall 
# (a) Jul-Sep, (b) Oct-Dec and (c) Jan-Mar.

# +ve/-ve actual battery power (B_cp, B_dp), +ve actual solar battery power (E_a);
# actual AC load (E_load); potential pv (E_p); SoC
na_seadec_sub <- imputed_data[,c(1:3,123:124,121,119,55,9)]
# Calculate capture loss: Potential PV - Actual PV; add phases for above months
na_seadec_sub <- na_seadec_sub %>% 
  mutate(Capture.loss.W=Potential.PV.power.W-Positive.Actual.Solar.Charger.Battery.Power_ma,
         phase=ifelse(month=="Jul" | month=="Aug" | month=="Sep", 1, 
                      ifelse(month=="Oct" | month=="Nov" | month=="Dec",2,3)))

na_seadec_sub <- gather(na_seadec_sub, id, value, 4:10)
typical_day <- na_seadec_sub %>% group_by(phase, timeUse, id) %>% summarise(value=mean(value))
typical_day <- as.data.frame(typical_day)
typical_day <- spread(typical_day, id, value)
colnames(typical_day) <- c("phase","timeUse","E_load","SoC","L_c","B_dp","E_a","B_cp","E_p")

# Plot typical values for each SL
plotTypical <- function(df, lim, br, p) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=B_cp/1000.0, color="B_cp", linetype="B_cp")) +
    geom_line(aes(y=abs(B_dp)/1000.0, color="B_dp",linetype="B_dp")) + 
    geom_line(aes(y=E_a/1000.0, color="E_a",linetype="E_a")) +
    geom_line(aes(y=E_load/1000.0, color="E_load",linetype="E_load")) + 
    geom_line(aes(y=E_p/1000.0, color="E_p",linetype="E_p")) +
    geom_line(aes(y=L_c/1000.0, color="L_c",linetype="L_c")) + 
    geom_line(aes(y = SoC/p, color = "SoC",linetype="SoC")) + 
    scale_y_continuous(breaks= seq(0,lim,br), sec.axis = sec_axis(~.*p, 
                                             name = "State of Charge (%)")) +
    labs(y="Energy (kW)", x = "Time of day", colour="", linetype="") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
}
# "Actual typical day profile for the Hall between Jul'19 and Sep'19"
plotTypical(typical_day[typical_day$phase==1,], 1.75, 0.25, 60) 
ggsave(here(plot_dir,"typical_day_jul19_sep19.pdf"), width = 8, height = 6, units = "cm")
# "Actual typical day profile for the Hall between Oct'19 and Dec'19"
plotTypical(typical_day[typical_day$phase==2,], 1.60, 0.20, 60) 
ggsave(here(plot_dir,"typical_day_oct19_dec19.pdf"), width = 8, height = 6, units = "cm")
# title="Actual typical day profile for the Hall between Jan'20 and Mar'20"
plotTypical(typical_day[typical_day$phase==3,], 1.60, 0.20, 60) 
ggsave(here(plot_dir,"typical_day_jan20_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 12a - Daily average yield, capture losses and system losses at hall 
# Get daily data for PV power, Potential PV and AC load 
# actual AC load (Yield), Ea (System loss=Ea-Yield), potential PV (Capture loss=Pot. PV- Ea)
na_seadec_sub <- system_daily[,c(1,2,3,88,94)] 
colnames(na_seadec_sub) <- c("month", "date", "E_load", "E_a", "E_p")
na_seadec_sub <- na_seadec_sub %>% mutate(L_c=E_p-E_a, L_s=E_a-E_load)
na_seadec_sub <- na_seadec_sub[,-c(4:5)]
# Daily average per month
hall_perf <- gather(na_seadec_sub, id, value, c(3:5))
hall_perf <- hall_perf %>% group_by(month, id) %>% summarise(value=mean(value))
hall_perf <- as.data.frame(hall_perf)  
# title="Daily average electrical energy values at the Hall between Jul'19 and Mar'20"
ggplot(hall_perf, aes(month, value/1000.0, fill=id)) + geom_bar(stat="identity", width=.5, position = "stack") + 
  scale_y_continuous(breaks=seq(0,14,2)) + 
  labs(y="Consumed & potential electric energy(kWh)", x = "Month", fill="") +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) + THEME + 
  theme(axis.title = element_text(size=7), legend.text = element_text(size=7), axis.text = element_text(size=7))
ggsave(here(plot_dir,"hall_perf_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#