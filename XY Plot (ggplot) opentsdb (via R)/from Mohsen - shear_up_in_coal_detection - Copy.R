
closeAllConnections()
rm(list=ls())
gc()

###############################################################################


###############################################################################
# 1. INIT       : Initialise & Define
#______________________________________________________________________________
#__________________________________________________________
# LIBRARY
#----------------------------------------------------------
#---Default library path
.libPaths("C:/Users/msajjadi/Documents/R/win-library/3.5")
#---library()
library(RODBC)
library(reshape2)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(scales)
library(knitr)
library(dplR) #coverts characters to LaTex form
library(ifultools)
library(sendmailR)
#__________________________________________________________

#source("G:/DEPARTMENT/Smart Services/Remote Health Monitoring/OpenTSDB/OpenTsdbLibV2.R")
source("./OpenTsdbLibV2.R")

#__________________________________________________________
# DIRECTORY
#----------------------------------------------------------
#workDir <- "C:/Users/ncady.JGI/Downloads/ModelCorrelation"
workDir <- "C:/Users/msajjadi/Documents/Work/RHM_Reports/shear_up_in_coal_detection/"
#__________________________________________________________
#--FUNCTIONS--
#database_query - connects to the HMM_db and returns the query data
database_query <- function(query){
  connection <- odbcConnect("AUS_RHM_DBO", uid = "rhm_aus_dbo", pwd = "Aussie#1")
  query_data <- sqlQuery(connection,query)
  close(connection) #close connection as no longer in use
  return(query_data)
}

#__________________________________________________________
# SavePlotAsPDF(outputDir,plotList)
#----------------------------------------------------------
# Description: Save plots (list) to size A3 pdf with scaling of 0.8
SavePlotAsPDF <- function(outputDir,plotList)
{
  scaling   <- 0.8
  cairo_pdf(file=outputDir,width=16.5*scaling,height=11.7*scaling,onefile=TRUE)
  #---Write timeline to pdf file
  print(plotList)
  #---Complete writing to pdf
  dev.off() #pdf timeline can now be opened
}#END: SavePlotAsPDF(outputDir,plotList)
#__________________________________________________________

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


ourTimeZone <- "Australia/Sydney"
databaseTimeZone <- "America/Winnipeg"

currentTime <- Sys.time()                #DEFAULT: CURRENT TIME
currentTime2 <- Sys.time()-1                #DEFAULT: CURRENT TIME
#currentTime <- as.POSIXct("2015-07-05")   #USE THIS TO RUN MANUALLY ON A SET DATE
attributes(currentTime)$tzone <- ourTimeZone
print(paste0("TIME STAMP: ", format(currentTime, "%H:%M:%S %d/%m/%Y")))

machines <- c("6450","6451","6789","7151","5008")
machines <- c("JM7058")
machineName <- machines[1]
days_ago <- 30


  # start_ts <- "2017/09/28 01:00:00"
  # end_ts   <- "2017/09/28 07:00:00"
  # 
  start_ts <- "2019/06/20-06:00:00"
  end_ts   <- "2019/06/24-06:00:00"
  report_date <- "10_to_18_Oct_2017"
  
  d <- days_ago
m <- machines[1]
#  for (m in machines)
#  {
    machineName <- m
    # for (d in 1:days_ago)
    # {
      ### set dates to previous day for reporting purposes
      # start_ts <- paste0(as.character(format(as.Date(Sys.Date()-d), "%Y/%m/%d"))," 00:00:00")
      # end_ts <- paste0(as.character(format(as.Date(Sys.Date()-d), "%Y/%m/%d"))," 23:59:59")
      # report_date <- paste0(as.character(format(as.Date(Sys.Date()-d), "%Y-%m-%d")))
      ###############################################################################
      # 2. INPUT      : Get Inputs
      #______________________________________________________________________________
      
      metrics <- c("CM_Cutt_Boom_Hgt_Cm","CM_Op_Cutt_Boom_Elev_Up_Stat","CM_Cutt_Mtr1_Lft_Ph1_Cur_A","CM_Cutt_Mtr1_Rt_Ph1_Cur_A","CM_Op_Trac_Mtr1_Lft_Fwd_Stat","CM_Op_Trac_Mtr1_Rt_Fwd_Stat")
      
      runs.df <- data.frame("shear_id"=numeric(),
                            "shear_type"=character(),
                            "shear_start"=character(),
                            "shear_end"=character(),
                            "shear_dur"=numeric(),
                            "boom_height_diff"=numeric(),
                            "shear_avg_speed"=numeric()
      )
      
      i <- 1
      
      data_list = list()
      
      #start_ts <- start_times[i]
      #end_ts <- end_times[i]
      print (start_ts)
      print (end_ts)
      
      data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
      

      data <- with(data,
                   data.frame(ts   = TS,
                              boom_height      = CM_Cutt_Boom_Hgt_Cm, 
                              shear_up    = CM_Op_Cutt_Boom_Elev_Up_Stat,
                              cutAmps_LH  = CM_Cutt_Mtr1_Lft_Ph1_Cur_A, 
                              cutAmps_RH  = CM_Cutt_Mtr1_Rt_Ph1_Cur_A, 
                              tram_fwd_LH    = CM_Op_Trac_Mtr1_Lft_Fwd_Stat,
                              tram_fwd_RH    = CM_Op_Trac_Mtr1_Rt_Fwd_Stat,
                              shear_up_in_coal = ifelse((CM_Cutt_Mtr1_Lft_Ph1_Cur_A>55 | CM_Cutt_Mtr1_Rt_Ph1_Cur_A>55) & CM_Op_Cutt_Boom_Elev_Up_Stat==1 & (CM_Op_Trac_Mtr1_Lft_Fwd_Stat==1 | CM_Op_Trac_Mtr1_Rt_Fwd_Stat==1),1,0)
                   )
      )
      

      
      if (exists("data") && is.data.frame(get("data")) && nrow(data)>0)
      {
        
        #machine_id <- data[1,]$machine_id
        ShearUpRLE <- rle(data$shear_up_in_coal)
        ShearUpRLE_1 = which(ShearUpRLE$values == 1 & ShearUpRLE$lengths > 1)
        ShearUpRLE.cumsum = cumsum(ShearUpRLE$lengths)
        ShearUpRLE.ends = ShearUpRLE.cumsum[ShearUpRLE_1]
        
        
        
        newindex = ifelse(ShearUpRLE_1 > 1, ShearUpRLE_1 - 1, 0)
        ShearUpRLE.starts = ShearUpRLE.cumsum[newindex] + 1
        if (0 %in% newindex) ShearUpRLE.starts = c(1,ShearUpRLE.starts)
        #print(data[shearDownRLE.starts[1]:shearDownRLE.ends[1],])
  
        if (exists("newindex") && is.vector("newindex") && !is.logical(newindex))
        {
          for (i in 1:length(ShearUpRLE.starts))
          {
            current_shear <- data[ShearUpRLE.starts[i]:ShearUpRLE.ends[i],]
            runs.df <- rbind(runs.df,
                             data.frame (
                               "shear_id"=i,
                               "shear_type"="SHEAR_UP_IN_COAL",
                               "shear_start"=current_shear[1,]$ts,
                               "shear_end"=current_shear[nrow(current_shear),]$ts,
                               "shear_dur"=nrow(current_shear),
                               "boom_height_diff"=diff(c(current_shear[1,"boom_height"],current_shear[nrow(current_shear),"boom_height"])),
                               "shear_avg_speed"=diff(c(current_shear[1,"boom_height"],current_shear[nrow(current_shear),"boom_height"]))*10/nrow(current_shear)
                               )
                            )
          }
          if (exists("current_shear")) rm(current_shear)
        }
        
        write.csv(file=paste0("shear_up_in_coal_list_",machineName,"_",report_date,".csv"), x=runs.df)
        
      }
          
    }
        
  
  



  

