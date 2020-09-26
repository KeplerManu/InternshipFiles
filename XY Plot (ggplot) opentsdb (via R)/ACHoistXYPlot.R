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
library(xlsx)

   source("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/OpenTsdbLibV2.R")  

   workDir <- "C:/Users/kmanulon/Documents/" 
   
   #set metrics
   machines <- c("ES41271")
   machineName <- machines[1]
   
   start_ts <- "2019/08/30-05:00:00"
   end_ts   <- "2019/08/30-06:00:00"
   #end_ts   <- "2019/06/11-03:06:13" ##end of adapt on << to figure out boudaries
   #m <- machines[1]  
   #machineName <- m 
   metrics <- c("ES_Hst_Mtr_Ft_Spd_RPM","ES_Hst_Mtr_Ft_Torque_Pct") #"ES_Hst_Mtr_Ft_Mot_Lmt_Cur_A"
   
   #create data table
   data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
   data <- data[order(c(data$TS)),]
   #data$TS <-  as.POSIXct(data$TS, origin = "1970/1/1", tz="UTC")
   data$Type <- 'Data'
   data$Order <- seq.int(nrow(data))
   names(data) <- c("TS", "Speed", "Torque", "Type", "Order")
   #head(data,30)
   
   #create limit table
   data_limit <- data.frame("Torque" = c(0, 0, -63, -70.73668111, -74.26289404, -84.88401733, -99.04839404, -116.0676353, -116.0676353, -116.0676353, -116.0676353, -116.0676353, -116.0676353, -116.0676353, -99.04839404, -84.88401733, -74.26289404, -70.73668111, -63, -63,
                                         0, 0, 63, 63, 70.73668111, 74.26289404, 84.88401733, 99.04839404, 116.0676353, 116.0676353, 116.0676353, 116.0676353, 116.0676353, 116.0676353, 116.0676353, 99.04839404, 84.88401733, 74.26289404, 70.73668111, 63, 0, 0),
                            "Speed" = c(-1400, -1400, -1400, -1267.6, -1207, -1057, -907, -727, -487, -187, 0, 173, 473, 713, 893, 1042, 1192, 1253, 1400, 1400, 1400, -1400, -1400, -1400, -1267.6, -1207, -1057, -907, -727, -487, -187, 0,
                                        173, 473, 713, 893, 1042, 1192, 1253, 1400, 1400, 1400),
                            "Type" = "Limit",
                            "Order" = 1:42)
   
   data_adaptive_limit <- data.frame("Torque" = c(63, 68.4, 74.8, 91, 101, 108.6, 113, 125.7, 125.7, 116), 
                                     "Speed" = c(1400, 1380, 1305, 1135, 1040, 970, 930, 808, 0, 0),
                                     "Type" = "Adaptive Limit",
                                     "Order" = 1:10)
   
   #create na imputation table to be combined with data table
   data_ma <- na_ma(data)
   data_ma <- subset(data_ma, select = -c(TS))
   #head(data_ma,30)
   
   #Combine tables into 1 table so graph can be displayed in Power BI
   XY_plot_data <- rbind(data_ma, data_limit, data_adaptive_limit)
   #tail(XY_plot_data,30)
   
   ggplot(data=XY_plot_data, aes(x=Torque,y=Speed))+
   theme_minimal()+
   geom_point(aes(colour = Type), size = ifelse(XY_plot_data$Type == 'Data', 0.2, -1))+
   geom_path(data = subset(XY_plot_data, Type == 'Data'), aes(colour = Type), size = 0.1) +
   geom_path(data = subset(XY_plot_data, Type == 'Limit'), aes(colour = Type), size = 1) +
   geom_path(data = subset(XY_plot_data, Type == 'Adaptive Limit'), aes(x = Torque, colour = Type), size = 1) +
   scale_colour_manual(breaks = c('Data', 'Limit', 'Adaptive Limit'), values = c("#FFC000", "#4F81BD", "#BE4B48")) + #yellow, blue, red
   scale_x_continuous(breaks = seq(-125, 125, by=25), expand = c(0.05,0))+
   scale_y_continuous(expand = c(0,0))
      
      #geom_path(data = XY_plot_data, aes(x = Torque, y = Speed), colour = "red", size=0.3)
      #geom_path(data = data_adaptable_limit, aes(x=Torque, y = Speed), colour = "orange", size = 1)
   
    #plot_ly(data = data, x = ~ES_Hst_Mtr_Ft_Torque_Pct, y = ~ES_Hst_Mtr_Ft_Spd_RPM, type="scatter", #text = paste("Clarity: ", data$clarity),
            #mode = "markers", color = ~TS, size = ~TS)
   
   #write.xlsx(data_ma, file = "E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/Test.xlsx", 
    #sheetName = "Sheet2", col.names = TRUE, row.names = TRUE, append = FALSE)
   