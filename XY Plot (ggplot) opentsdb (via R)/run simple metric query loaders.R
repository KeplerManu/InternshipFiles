closeAllConnections() 
rm(list=ls()) 
#gc()   

library(RODBC) 
library(reshape2)
library(plyr) 

library(zoo) ##added

library(stats)
library(stinepack) #
library(graphics)
library(grDevices)
library(forecast) #
library(magrittr)
library(Rcpp)
library(testthat) #
library(utils)
library(timeSeries) #
library(tis)
library(xts) #
library(tsdb)
library(imputeTS) #
library(ggplot2)
library(plotly)
library(rjson)
library(readxl)

   source("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/OpenTsdbLibV2.R")  

   workDir <- "C:/Users/kmanulon/Documents/" 
   
   machines <- c("LD-1850-2222")
   machineName <- machines[1]
   
   start_ts <- "2019/08/16-03:00:00"
   end_ts   <- "2019/08/16-04:00:00"
   #end_ts   <- "2019/06/11-03:06:13" ##end of adapt on << to figure out boudaries
   m <- machines[1]  
   machineName <- m 
   metrics <- c("LD_Bucket_Load", "LD_Bucket_Load_Tons", "LD_Bucket_Load_Tonnes")
   #metrics <- c("ES_Hst_Mtr_Ft_Spd_RPM","ES_Hst_Mtr_Ft_Mot_Lmt_Cur_A","ES_Hst_Mtr_Ft_Torque_Pct")
   
   data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
   data <- data[order(c(data$TS)),]
   #data$TS <-  as.POSIXct(data$TS, origin = "1970/1/1", tz="UTC")
   data$Type <- 'Data'
   data$Order <- seq.int(nrow(data))
   data <- subset(data, select = -c(TS))
   names(data) <- c("Speed", "Torque", "Type", "Order")
   head(data,30)
   
   data_limit <- read_excel("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/XY Plot data.xlsx", sheet = "Hoist Limits")
   data_limit <- subset(data_limit, select = -c(Raw))
   data_limit$Type = 'Limit'
   data_limit$Order <- seq.int(nrow(data_limit))
   names(data_limit) <- c("Torque", "Speed", "Type", "Order")
   
   # data_ma4 <- na.ma(data, k = 4, weighting = "exponential")
   # head(data_ma4,30)
   
   data_ma <- na_ma(data)
   #head(data_ma,30)
   
   #data_approx <-  na.approx(data) 
   #head(data_approx,30)
   # 
   #data_interpolation <- na.interpolation(data)
   #head(data_interpolation,30)
   # 
   # data_locf <- na.locf(data)
   # head(data_locf,30)
   
   #condition1 <- data_ma$ES_Hst_Mtr_Ft_Torque_Pct > 64 & data_ma$ES_Hst_Mtr_Ft_Torque_Pct < 68 & data_ma$ES_Hst_Mtr_Ft_Spd_RPM > 1150
   #condition2 <- data_ma$ES_Hst_Mtr_Ft_Torque_Pct > 80 & data_ma$ES_Hst_Mtr_Ft_Torque_Pct < 81 & data_ma$ES_Hst_Mtr_Ft_Spd_RPM > 850
   #condition3 <- data_ma$ES_Hst_Mtr_Ft_Torque_Pct > 115 & data_ma$ES_Hst_Mtr_Ft_Torque_Pct < 117 & data_ma$ES_Hst_Mtr_Ft_Spd_RPM > 713
   #data_adaptable_limit <- data.frame(
      #Torque = c(63, max(data_ma$ES_Hst_Mtr_Ft_Torque_Pct[which(condition1)]), max(data_ma$ES_Hst_Mtr_Ft_Torque_Pct[which(condition2)]), max(data_ma$ES_Hst_Mtr_Ft_Torque_Pct[which(condition3)]), 116.2),
      #Speed = c(1400, max(data_ma$ES_Hst_Mtr_Ft_Spd_RPM[which(condition1)]), max(data_ma$ES_Hst_Mtr_Ft_Spd_RPM[which(condition2)]), max(data_ma$ES_Hst_Mtr_Ft_Spd_RPM[which(condition3)]), 713)
      #)
   
   new_data <- rbind(data_ma, data_limit)
   #tail(new_data,30)
   
   ggplot(data=new_data, aes(x=Torque,y=Speed))+
     theme_minimal()+
    geom_point(aes(colour = Type),size = ifelse(new_data$Type == 'Data', 0.2, -1))+
    geom_path(aes(colour = Type),size= ifelse(dataset$Type == 'Data', 0.1, 0.5))
    #scale_color_gradient(low = "#0091ff", high = "#f0650e")
   #scale_color_gradient(low = "#ffffff", high = "#000000")
      #geom_path(data = new_data, aes(x = Torque, y = Speed), colour = "red", size=0.3)
      #geom_path(data = data_adaptable_limit, aes(x=Torque, y = Speed), colour = "orange", size = 1)
   
    #plot_ly(data = data, x = ~ES_Hst_Mtr_Ft_Torque_Pct, y = ~ES_Hst_Mtr_Ft_Spd_RPM, type="scatter", #text = paste("Clarity: ", data$clarity),
            #mode = "markers", color = ~TS, size = ~TS)
   