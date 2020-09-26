#Kepler Manu-Long 12/09/19
#Create a categorised Scatterplot within R

library(dplyr)
library(implyr)
library(ggplot2)
library(jsonlite)
library(tidyr)
library(stats)
library(imputeTS)

#Change this to where you store the OpenTsdbLibV2.R file
source("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/OpenTsdbLibV2.R")  

#set metrics
machines <- c("ES41251")
machineName <- machines[1]

start_ts <- "2019/09/5-00:00:00"
end_ts   <- "2019/09/11-00:00:00"
#metrics <- c("ES_Cwd_Mtr_DE_Temp_C", "ES_Cwd_Mtr_NDE_Temp_C")
metrics <- c("ES_Hst_Mtr_Rr_DE_Temp_C", "ES_Hst_Mtr_Rr_NDE_Temp_C")

#create data table
data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
data <- na_ma(data)
data <- data[order(c(data$TS)),]
#data$ld_cycles_payload_tonnes <- round(data$ld_cycles_payload_tonnes)
#data$ld_cycles_total_duration <- floor(data$ld_cycles_total_duration)
#data <- subset(data, select = -c(TS))
names(data) <- c("TS", "DE", "NDE")
#head(data)

fault_rects <- data.frame(xstart = c(0,90), xend = c(100, 100), ystart = c(90, 0), yend = c(100, 100), col = letters[1])
warning_rects <- data.frame(xstart = c(0,80), xend = c(90, 90), ystart = c(80, 0), yend = c(90, 90), col = letters[2])

ggplot(data=data, aes(x=data$DE,y=data$NDE))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank()) +
  labs(x = "DE", y = "NDE") +
  geom_point(colour = '#000000',size = 0.5)+
  coord_fixed(ylim = c(0,100), xlim = c(0,100), ratio = 0.75)+
  geom_abline(linetype = "dotted", size = 0.75, colour = "red")+
  scale_x_continuous(breaks = seq(0,100, by=10), expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,100, by=10), expand = c(0,0))+
  #geom_rect(xmin = 0, xmax = 100, ymin = 90, ymax = 100, fill = '#FF7070', alpha = 0.01) +
  #geom_rect(xmin = 90, xmax = 100, ymin = 0, ymax = 90, fill = '#FF7070', alpha = 0.01) +
  #geom_rect(xmin = 0, xmax = 90, ymin = 80, ymax = 90, fill = '#FFDA69', alpha = 0.01) +
  #geom_rect(xmin = 80, xmax = 90, ymin = 0, ymax = 80, fill = '#FFDA69', alpha = 0.01) +
  geom_text(data = NULL, label = "F a u l t", x = 50, y = 95) +
  geom_text(data = NULL, label = "W a r n i n g", x = 50, y = 85)
  
