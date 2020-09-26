closeAllConnections() 
rm(list=ls()) 
gc()   
 ###############################################################################
             
  # 1. INIT       : Initialise & Define
   #______________________________________________________________________________
   #__________________________________________________________
   # LIBRARY #----------------------------------------------------------
   #---Default library path
   #.libPaths("C:/Users/kmanulon/Documents/R/win-library/3.6")
   #---library() 
library(RODBC) 
library(reshape2)
library(plyr) 
#library(ifultools)   
#__________________________________________________________
   #source("G:/DEPARTMENT/Smart Services/Remote Health Monitoring/OpenTSDB/OpenTsdbLibV2.R")
   source("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/OpenTsdbLibV2.R")  
  #__________________________________________________________
   # DIRECTORY #----------------------------------------------------------
   workDir <- "C:/Users/kmanulon/Documents/" #__________________________________________________________
   #--FUNCTIONS--
   
   
   machines <- c("ES41143")
   machineName <- machines[1]
   
   start_ts <- "2019/07/27-20:30:00"
   end_ts   <- "2019/07/27-21:30:00"
   m <- machines[1]  
   machineName <- m 
   metrics <- c("ES_Sys_RHM_SAO_State","ES_Hst_Mtr_Ft_Cur_A")
   data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
    