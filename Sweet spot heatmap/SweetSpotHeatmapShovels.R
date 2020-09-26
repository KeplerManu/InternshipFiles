## following these instructions to retrieve Impala data
## https://blog.cloudera.com/blog/2017/07/implyr-r-interface-for-apache-impala/

# Install packages
#install.packages("dplyr")
#install.packages("implyr")
#install.packages("odbc")
#install.packages("httr")
#install.packages("jsonlite")

library(odbc)
library(httr)
library(rvest)
library(dplyr)
library(implyr)
library(ggplot2)
library(jsonlite)
library(stringr)
library(scales)
library(gridExtra)
library(ggthemes)
library(tidyr)

#Change this to where you store the OpenTsdbLibV2.R file
source("E:/Komatsu Power BI/Kathryn/XY Plot (ggplot) opentsdb (via R)/OpenTsdbLibV2.R")  

#set metrics
machines <- c("ES41178")
machineName <- machines[1]

start_ts <- "2019/09/05-00:00:00"
end_ts   <- "2019/09/10-00:00:00"
metrics <- c("es_cycles_payload_metric_raw", "es_cycles_cycle_duration")

#create data table
data <- RunSimpleMetricQuery(start_ts, end_ts, metrics = metrics, machine_serial_no = machineName, wide_format = TRUE )
data <- na.omit(data)
data <- data[order(c(data$TS)),]
data$es_cycles_payload_metric_raw <- round(data$es_cycles_payload_metric_raw)
data$es_cycles_cycle_duration <- floor(data$es_cycles_cycle_duration)
data <- subset(data, select = -c(TS))
names(data) <- c("payload_tonnes", "duration_seconds")
head(data)

#count how many of each pair of the same payload and duration there are in the dataset
count_tbl <- as.data.frame(count(data, data$payload_tonnes, data$duration_seconds))
names(count_tbl) <- c("payload", "duration", "freq")

#filter so cycle time within 60 seconds
count_tbl <- filter(count_tbl, duration <= 60)

#create function to reorder count_tbl - used after adding rows
reorder_count_tbl <- function(){
  count_tbl[order(c(count_tbl$duration)),]
}

#creates a base value so gaps can be filled
count_tbl[nrow(count_tbl)+1,] = c(1,1,0)
count_tbl[nrow(count_tbl)+1,] = c(40,60,0)
count_tbl <- reorder_count_tbl()

i <- 1
j <- 1
#adds one field if there's a gap in duration so that it can filled by the complete() fucntion
for(value in diff(count_tbl[,2])){
  if(value >= 2){
    while(value > 1){
    count_tbl[nrow(count_tbl) + 1,] = c(0, count_tbl[i,2] + (value - 1), 0)
    value = value - 1
    }
  }
  i = i + 1
}

#adds one field if there's a gap in payload so that it can filled by the complete() fucntion
count_tbl <- count_tbl[order(c(count_tbl$payload)),]
for(value in diff(count_tbl[,1])){
  if(value >= 2){
    while(value > 1){
      count_tbl[nrow(count_tbl) + 1,] = c(count_tbl[j,1] + (value - 1), 0, 0)
      value = value - 1
    }
  }
  j = j + 1
}

#completely fill the whole table
count_tbl <- reorder_count_tbl() %>%
  complete(payload, duration, fill=list(freq=0))

#alternative count method
#count_frame <- data %>%
  #unique() %>%
  #group_by(payload_tonnes, duration_seconds) %>%
  #summarise(freq = n())

#code based off https://www.r-bloggers.com/exploring-london-crime-with-r-heat-maps/
# plot above data in faceted heat map
ggplot(count_tbl,aes(x=(duration - 0.5),y=(payload - 0.5), fill=freq))+
  #add border white colour of line thickness 0.25
  geom_tile(color = 'white')+
  labs(x="Cycle Time (sec)",y="Load Weight (Tonnes)")+
  scale_y_continuous(breaks = seq(50,120, by=10),expand = c(0,0))+
  #set x axis pin frequency
  scale_x_continuous(breaks = seq(10,60, by=5), expand = c(0,0))+
  scale_fill_gradient(low = "#3399ff", high = "#da291c") +
  ggtitle("Sweet Spot") +
  coord_fixed(ylim = c(50,120), xlim = c(15,50), ratio = 0.4)+
  #set a base size for all fonts
  theme_grey(base_size=10)+
  #theme options
  theme(
    # vertical labels on x axis
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.6)
    #remove plot background
    #plot.background=element_blank(),
    #remove plot border
    #panel.border=element_blank(),
    #panel.spacing.y=unit(0.5, "cm"),
  )

#write.xlsx(count_tbl, file = "E:/Komatsu Power BI/Kathryn/Sweet spot heatmap/Test.xlsx", 
# sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)