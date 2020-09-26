## following these instructions to retrieve Impala data
## https://blog.cloudera.com/blog/2017/07/implyr-r-interface-for-apache-impala/

# Install packages
install.packages("dplyr")
install.packages("implyr")
install.packages("odbc")
install.packages("httr")
install.packages("jsonlite")

#Load packages
library(odbc)
library(implyr)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)

#Create an ODBC connection to Impala
drv <- odbc::odbc()

impala <- src_impala(
    drv = drv,
    driver = "Cloudera ODBC Driver for Impala",
    host = "alarmodbc.jganalytics.local",
    port = 21050,
    database = "alarmevents",
    uid = NULL,
    pwd = NULL
)

#show list of available tables/views
src_tbls(impala)

#retrieve the data - too big. crashing. refer to DWAPI read
#es_cycles_tbl <- tbl(impala,  "es_cycles_total_duration_vs_payload")
#es_cycles_tbl <- tbl(impala, in_schema("dwh", "es_cycles_total_duration_vs_payload"))
#head(es_cycles_tbl)



#density plot using inbuilt Iris dataset

attach(iris)
head(iris)
summarise(iris)

ggplot(iris, aes(x=Petal.Length, y=Sepal.Length) ) +
  geom_bin2d() +
  theme_bw()

# Number of bins in each direction?
ggplot(iris, aes(x=Petal.Length, y=Sepal.Length) ) +
  geom_bin2d(bins = 10) +
  theme_bw()



## retrieve data from impala via DWAPI, ie API, ie JSON
#instructions


#http://dwapi.jganalytics.local/dwapi/v1/buckets
#http://dwapi.jganalytics.local/dwapi/v1/bucket/last/ld_cycles/LD-2350-2223

username = NULL
password = NULL
base <- "http://dwapi.jganalytics.local/dwapi/v1/bucket/last/"
tablename <- "ld_cycles"
serialno <- "LD-2350-2223"

call1 <- paste(base,tablename,"/",serialno, sep="")

#get_data <- GET(call1, authenticate(username,password, type = "basic"))
get_data <- GET(call1)
get_data_text <- content(get_data, "text")
get_data_json <- fromJSON(get_data, flatten = TRUE)
get_data_df <- as.data.frame(get_data_json)
view(get_data_df)











##density plot

## Data in a data.frame
x1 <- rnorm(n=1E3, sd=2)
x2 <- x1*1.2 + rnorm(n=1E3, sd=2)
df <- data.frame(x1,x2)

# ## Use densCols() output to get density at each point
# x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
# df$dens <- col2rgb(x)[1,] + 1L
# 
# ## Map densities to colors
# cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
#                             "#FCFF00", "#FF9400", "#FF3100"))(256)
# df$col <- cols[df$dens]
# 
# ## Plot it, reordering rows so that densest points are plotted on top
# plot(x2~x1, data=df[order(df$dens),], pch=20, col=col, cex=2)

# Number of bins in each direction?
ggplot(df, aes(x=x1, y=x2) ) +
 geom_bin2d(bins = 10) +
 theme_bw()



