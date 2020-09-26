options(digits.secs=3)
library(rjson)
library(jsonlite)
library(httr)
library(zoo)
library(reshape2)

# #Define an enum of date formats 
# available_timestamp_formats <- list(OPENTSDBREADABLE = 1,
#                                NUMERIC = 2,
#                                POSIXCT = 3)

# default connection object. This script would act like an Http wrapper object to communicate with 
# Opentsdb server. Use ConnectToServer function to reset the connection to point to a different IP
# old AWS URL:  url = "http://52.11.118.177", port = 9998
#old public address : connection_obj        <- list("url" = "http://analytics.joyglobal.com", "port" = 9999)
#connection_obj        <- list("url" = "http://analytics.joyglobal.com", "port" = 9999)
connection_obj        <- list("url" = "http://timeseriesread.jganalytics.local", "port" = 9999)
#connection_obj        <- list("url" = "http://analytics.jganalytics.local", "port" = 9999)
#connection_obj        <- list("url" = "http://13.65.36.234:9999/", "port" = 9999)


# Sets up the connection object that will be used in future queries to construct the HTTP Request object
#
# Args:
#    url - URL to connect to. Please use InitHttpProxy to initialize the proxy settings
#    port - port number of web service
# Returns:
#  Just initializes the global connection object. It is used internally in this library.
# old AWS URL:  url = "http://52.11.118.177", port = 9998
#old public address ConnectToServer <- function(url = "http://analytics.joyglobal.com", port = 9999){
#ConnectToServer <- function(url = "http://13.65.36.234:9999/", port = 9999){
#ConnectToServer <- function(url = "http://analytics.joyglobal.com", port = 9999){
ConnectToServer <- function(url = "http://analytics.jganalytics.local", port = 9999){
  connection_obj$url  <- url
  connection_obj$port <- port
  
}

# Initialize the proxy settings on the machine. 
# TODO - Check if there is a workaround available to prevent having to 
# set this before each HttpRequest. Currently, Curl_fetch_memory errors are encountered
# at random points directly or indirectly linked to the proxy settings expiring. 
# This has been solved by setting config before each call to RunMetricQuery
InitHttpProxy <- function(proxy_url = "au-hvs-proxy.jgi.joyglobalinc.com",  #"au-wic-proxy.jgi.joyglobalinc.com"
                          port=9090){
  #set_config(use_proxy(proxy_url,port))
}

# Used internally to send out http requests over the network.
# Args:
#    query_url          - query url
#    query_obj    - query list
#    retry_count  - how many times to retry in case of error
# Returns:
#    httpResult json object or NULL in case of error
HttpRequest <- function(query_url, query_obj, retry_count = 4){
  # For some reason, RStudio loses the proxy settings throwing curl_exceptions. 
  # Setting it before each call, resolved taht problem
  InitHttpProxy()
  status <- TRUE
  for(try_counter in 1:retry_count){
    status <- tryCatch({ 
    #Construct the HTTPRequest packet and send it out
    req <- POST(url = query_url, 
                body = jsonlite::toJSON(query_obj,auto_unbox = TRUE, pretty = TRUE) ,
                add_headers("Content-Type"="application/json"), encode = 'json')
    
  
    
    #Check the status of the result
    stop_for_status(req)
    status <- TRUE 
    }, 
    error = function(err){
      print("There was an HTTP processing error")
      print(err)
      status <- FALSE
    })
    if( status) break
  }
  
#   if(req$status_code == 400){
#     print("--------------------------------")
#     print( toJSON(query_obj,auto_unbox = TRUE, pretty = TRUE)   )
#     print("-------------------------------")
#     
#   }
  
  if( !status){
    print(sprintf("There was an HTTP Error. Timed out after trying %d times. Returning", retry_count))
    return(NULL)
  }
  
  #Parse the JSON content in the HTTPResult object
  json_result <- content(req, "text")
  
  # Now we can parse
  result_obj <- rjson::fromJSON(json_result)
  
  #check if there was a valid result 
  if( is.null(result_obj) || length(result_obj) == 0){
    print("There were no errors in the http request, the results were blank!")
    return(NULL)
  }
  
  return(result_obj)
}

# Measure the rate of change of the engine rpm 
# Check that the value is > 500 at least 96% of the time to ensure engine is turned on and working 
# during the time interval between start_ts - end_ts
# Args:
#    start_ts - start timestamp
#    end_ts - end timestamp
#    output_timezone - timezone to convert the results' TS to 
# Returns:
#   True/False indicating whether machine is turned on or off during this period. It can 
#   be used to decide whether any further metrics need to be fetched for that interval or not
 CheckValidEngineRpm  <- function(start_ts, end_ts, #timestamp_format = available_timestamp_formats$POSIXCT,
                                 machine_serial_no ,
                                 output_timezone = "UTC"){
  
  #Construct the tags object which has "serialno" - <serial_no> mapping for the query
  tags      <- ConstructTagsObject(c("SerialNo"), c(machine_serial_no))
  
  # send http request querying raw data for the metric between start_ts and end_ts
  #msResolution is set to TRUE here as the engine rpm has a millisecond update frequency
  results   <- RunMetricQuery(start_ts, end_ts, 
                              output_timezone,
                              #timestamp_format, 
                              msResolution = TRUE, 
                              metrics = ConstructMetricsSubQuery(metrics = c("LD_Engine_"),
                                                                      downsample = NULL, rate = FALSE, 
                                                                      aggregator = "avg", tags = tags))
                                                                                                      
  if( is.null(results) || !nrow(results)){
    return(FALSE)
  }
 
  #Clean up the results and format it to readable format
  results      <- dcast(results, TS ~ metric)
  total_cnt    <- nrow(results)
  if( "character" %in% class(start_ts)){
        start_ts <- as.POSIXct(start_ts, format = "%Y/%m/%d-%H:%M:%S")
        
  }else if("numeric" %in% class(start_ts)){
    if( stringr::str_length(as.character(start_ts)) > 10 ) {
      start_ts <- as.POSIXct(start_ts/1000, origin="1970-01-01")
    }else{
      start_ts <- as.POSIXct(start_ts, origin="1970-01-01")
    }
    
  }
  if("character" %in% class(end_ts)){
    end_ts   <- as.POSIXct(end_ts,   format = "%Y/%m/%d-%H:%M:%S")
  }else if("numeric" %in% class(end_Ts)){
    if( stringr::str_length(as.character(end_ts)) > 10 ) {
      end_ts <- as.POSIXct(end_ts/1000, origin="1970-01-01")
    }else{
      end_ts <- as.POSIXct(end_ts, origin="1970-01-01")
    }
  }
  
  #TODO Add timezone
#   if( timestamp_format == available_timestamp_formats$OPENTSDBREADABLE){
#     start_ts <- as.POSIXct(start_ts, format = "%Y/%m/%d-%H:%M:%S")
#     end_ts   <- as.POSIXct(end_ts,   format = "%Y/%m/%d-%H:%M:%S")
#   }
#   else if(timestamp_format == available_timestamp_formats$NUMERIC){
#     if( stringr::str_length(as.character(start_ts)) > 10 ) {
#       start_ts <- as.POSIXct(start_ts, format = "%Y/%m/%d-%H:%M:%S")
#       end_ts   <- as.POSIXct(end_ts,   format = "%Y/%m/%d-%H:%M:%S")
#     }
#   }
  # Based on tag frequency, engine rpm is updated 5 times every second, 
  # which means we should have about 300 updates per minute
  # find number of minutes in the interval and multiply by 300.
  # if the result has at least 96% of the data, consider it valid
  # provided at least 96% of the time the value is > 500 indicating
  # machine is turned on
  no_records_expected <- difftime(end_ts, start_ts, units = "mins")
  no_records_expected <- no_records_expected * 300
  return( total_cnt >= 0.96 * no_records_expected && 
            sum(as.numeric(results$`Engine-`) > 500) > 0.96 * total_cnt)
}

#Obtain all available metrics on the machine
# Args:
#   machine_serial_no - serial number of machine
#   
#   showTSUID - whether TSUID should be returned for each metric
#   limit - used to set how many result rows to fetch from database
# Returns:
#  All metrics available for the machine selected, 
#  as a dataframe with one column if showTSUID is FALSE, if true returns an additional 
#  column indicating the TSUID for the metric - to be used with TSUID queries
LookupMetricsForMachine <- function(machine_serial_no,  showTSUID = FALSE, limit = 1000){
  
  # Construct list of tags. The way tags are passed to this end point is slightly different
  # from the metric query.
  #key_value_map <- lapply(1:length(tags), function(i){return(list("key"   = names(tags)[i], 
   #                                                               "value" = unname(tags)[i]))})
  key_value_map <- list(list("key" = "SerialNo", "value" = as.character(machine_serial_no)))
  
  #End point of the API for lookup type queries
  url_endpoint  <- "/api/search/lookup"
  
  payload       <- list("metric" = "*", "limit" = limit, 
                        "tags"   = key_value_map)
  if( showTSUID ){
    payload     <- append(payload, list("showTSUIDs" = "true"))
  }
  
  results       <- HttpRequest(query_url = paste0(connection_obj$url, ":", connection_obj$port, url_endpoint),
                          query_obj = payload, retry_count = 4 )
  if( is.null(results) || is.null(results$results) || !length(results$results)){
    print("Query returned with errors or no data. Null dataset returned.")
    return(NULL)
  }
  
  metrics_for_machine <- lapply(results$results, function(i) {return(data.frame("metric" = i$metric, "TSUID" = i$tsuid))})
  
  #Parse the metrics object 
  result_df    <- do.call("rbind", metrics_for_machine)
  result_df$metric <- as.character(result_df$metric)
  result_df$TSUID <- as.character(result_df$TSUID)
  if(showTSUID){
    return(result_df)
    
  }
  
  #return the metrics object
  return(result_df$metric)
}


# Obtain the last timestamp for a given metric
# Args:
#   metrics - list of metrics for which the last recorded timestamp should be returned. 
#   machine_serial_no - serial number of machine
#   output_timezone - this argument lets the function cast the timestamp to the machine time zone
#   backScan - specify how many hours to go back in time to look for an update on this metric
# Returns:
#  Dataframe of metric, TS and value at that timestamp
FetchLastRecordForMetrics <- function(metrics, machine_serial_no, output_timezone = "UTC", backScan = 24){
  
  url_endpoint <- "/api/query/last"
  tags    <- ConstructTagsObject(c('SerialNo') , c(as.character(machine_serial_no)))
  metric_o <- lapply(1:length(metrics), function(i){ 
    return(list("metric" = metrics[i],
                "tags" = tags))})
  query_o <- list("queries" = metric_o, "resolveNames" = "true", "backScan" = backScan)
  results <- HttpRequest(query_url =  paste0(connection_obj$url, ":", 
                                             connection_obj$port, url_endpoint), 
                         query_obj =  query_o)
  if( is.null(results ) ){
    print("Either an error in service or results were blank")
    return(NULL)
  }
  
  # Preliminary checking and cleaning
  datapoint_results <- lapply(results, function(l){
    if(is.null(l) || !length(l) ){
      print("Empty results set. Perhaps there is no data in 
                                                the database for this machine-metric")
      return(NULL)
    }
    return(data.frame("metric" = l$metric, 
                      "TS" = l$timestamp, 
                      "value" = as.numeric(l$value)))
  })
  #Convert to a dataframe
  results_df <- do.call("rbind", datapoint_results)
  
  #Convert timestamps to POSIXct
  if( !is.null(results_df) && nrow(results_df)){
    results_df$TS <- as.POSIXct(as.numeric(as.character(results_df$TS))/1000, 
                                origin= as.POSIXct("1970-01-01",
                                                   tz=output_timezone), 
                                tz=output_timezone)
  }
  return(results_df)
}

# This is the most frequently used endpoint of opentsdb - to fetch data
# The following function is a simpler version where all metrics have to use the same parameters
# for query including downsampling rate, rate and aggregators. 
# For more complicated combinations, use ConstructMetricsSubQuery to construct the subqueries and 
# pass it on to RunMetricQuery 
# Args:
#   start_ts          -  start time of the query
#   end_ts            -  end time of the query
#   output_timezone - timezone to convert the results' TS to 
#   metrics           -  vector of metric names
#   machine_serial_no -  serial number of machine
#   timestamp_format  -  what is the format of the start_ts, end_ts values. 
#                        For time values directly queried from system via R interface, 
#                        this would be default POSIXct
#   msResolution      -  Millisecond resolution. 
#   showTSUIDs        -  whether TSUID should be returned for each metric
#   aggregator        -  What aggregation to use when multiple timestamps have same value
#                       and then set metrics in this function to c(metric_query_1, metric_query_2)
#  timeout_max        -  the HTTPRequest does not always succeed in establishing connection with remote server. This option lets
#                       the user program how many times the request should be retried before expiring.
#  downsample         - Should downsampling be used
#  rate               - Should values be returned or rate of change for the metric 
# Returns:
#  All datapoints for the selected metrics 
RunSimpleMetricQuery <- function(start_ts, end_ts, metrics, 
                                 machine_serial_no, 
                                 output_timezone= "UTC",
                                 #timestamp_format = available_timestamp_formats$POSIXCT,
                                 msResolution = TRUE, showTSUIDS = FALSE, aggregator = "avg", 
                                 timeout_max = 3, downsample = NULL, rate = FALSE, wide_format = FALSE){
  
  metrics_sub_query <- ConstructMetricsSubQuery(metrics = metrics, downsample = downsample, 
                                                rate = rate, aggregator = aggregator, 
                                                tags = c('SerialNo'= as.character(machine_serial_no)))

  return(RunMetricQuery(start_ts = start_ts, end_ts = end_ts, output_timezone = output_timezone, #timestamp_format = timestamp_format,
                 msResolution = msResolution, showTSUIDs = showTSUIDS, 
                 metrics = metrics_sub_query, timeout_max = timeout_max, wide_format = wide_format))
}

# This is the most frequently used endpoint of opentsdb - to fetch data
# The following function relies on the end user to construct the sub queries and 
# pass it on to the RunMetricQuery function. 
# Args:
#   start_ts         -  start time of the query
#   end_ts           -  end time of the query
#   output_timezone - timezone to convert the results' TS to 
#   timestamp_format -  similar to lookup_query. Can take values - OPENTSDBREADABLE(e.g.2016/04/20-00:00:00)
#                       or R internal POSIXct or NUMERIC : unix timestamp( e.g. 1461128400000)              
#   msResolution     -  Millisecond resolution. 
#   showTSUIDs       -  whether TSUID should be returned for each metric
#   metrics          -  this can either be a character vector containing the names of all metrics to query 
#                       or can be a list of sub queries i.e. result of call to ConstructMetricsSubQuery. For all simple queries,
#                       metrics would be a vector of metric names. for mor ecomplicated queries,
#                       The query is constructed one piece at a time
#                       as it is possible to specify multiple options for each sub query for downsampling, aggregates and so on. 
#                       For this reason, sub query construction is split into a different function ConstructMetricsSubQuery. 
#                       The user can construct as many sub queries as required(metric_query_1, metric_query_2)
#                       and then set metrics in this function to c(metric_query_1, metric_query_2)
#  timeout_max       -  the HTTPRequest does not always succeed in establishing connection with remote server. This option lets
#                       the user program how many times the request should be retried before expiring.
# Returns:
#  All datapoints for the selected metrics 
RunMetricQuery  <- function(start_ts, end_ts, output_timezone = "UTC", #timestamp_format = available_timestamp_formats$POSIXCT, 
                            msResolution = TRUE, showTSUIDs = FALSE, metrics, timeout_max = 3, 
                            wide_format = FALSE){

  url_endpoint <- "/api/query?arrays=TRUE"
  
  if( "POSIXct" %in% class(start_ts)){
    #attributes(start_ts)$tzone <- "UTC"
    start_ts <- format(start_ts, format = "%Y/%m/%d-%H:%M:%S")
  }else if("character" %in% class(start_ts)){
    start_ts <- as.POSIXct(strptime(start_ts, format="%Y/%m/%d-%H:%M:%S"))
    #attributes(start_ts)$tzone <- "UTC"
    start_ts <- format(start_ts, format = "%Y/%m/%d-%H:%M:%S")
  }
  
  if( "POSIXct" %in% class(end_ts)){
    #attributes(end_ts)$tzone <- "UTC"
    end_ts   <- format(end_ts, format = "%Y/%m/%d-%H:%M:%S")
  } else if("character" %in% class(end_ts)){
    end_ts  <- as.POSIXct(strptime(end_ts, format="%Y/%m/%d-%H:%M:%S"))
    #attributes(end_ts)$tzone <- "UTC"
    end_ts  <- format(end_ts, format = "%Y/%m/%d-%H:%M:%S")
  }
  #If not POSIXct it is in character format readable in opentsdb - 2016/04/05-01:00:00
  
  #THis is the main portion of the query body
  query_o <- list("start" = start_ts, "end" = end_ts, 
                    "msResolution" = stringr::str_to_lower(as.character(msResolution)), 
                    showTSUIDs = stringr::str_to_lower(as.character(showTSUIDs)), 
                    "queries" = metrics)
  
  # Send query to server and collect results
  #results <- HttpRequest(query_url =  paste0(connection_obj$url, ":", connection_obj$port, url_endpoint),
  results <- HttpRequest(query_url =  paste0(connection_obj$url, url_endpoint), 
                         query_obj =  query_o, 
                         retry_count = timeout_max )
  
  if( is.null(results ) ){
    print("Either an error in service or results were blank")
    return(NULL)
  }
  
 
  # Preliminary checking and cleaning
    datapoint_results <- lapply(results, function(l){
      
      if(is.null(l$dps) || !length(l$dps) ){
        print("Empty results set")
        return(NULL)
      }
      
      #Valid data found
      # Since the arrays param has been set to true in the query end point,resuls
      # are returned as an array instead of a list of values where names of the list
      # is equal to the timestamp values. Now it would be an array of 2 values, [ts,value]
      ts_value <- do.call("rbind", l$dps)
      return(data.frame("metric" = l$metric, 
                        "TS" = ts_value[,1], 
                        "value" = as.numeric(ts_value[,2])))
    })
    
    #Convert to a dataframe
    results_df <- do.call("rbind", datapoint_results)
    
    #Convert timestamps to POSIXct
    if( !is.null(results_df) && nrow(results_df)){
      
      #offset computation : we want to acquire the "UTC +/- hh:mm" for our timezone
      #get a recent time, convert it to the target timezone, and extract the offset with format()
      timeForOffset <- Sys.time() #get a recent time...
      attr(timeForOffset,"tzone") <- output_timezone #convert to output timezone
      offset <- format(timeForOffset, "%z") #extract the UTC offset
      UTChoursOffset  <- as.numeric(substr(offset,1,3))#parse UTC offset Hours
      UTCminuteOffset <- as.numeric(substr(offset,4,5))#parse UTC offset Minutes
      UTCOffsetInSec <- UTChoursOffset * 3600 + UTCminuteOffset * 60
      
      #millisecond resolution is achieved by returned an EPOCH timestamp with ms precision and multiplied by 1000 (avoinds having to send a decimal/float number)
      #here we divide by 1000 to put the decimal pouint at the right sport and parse the EPOCH timestamp
      if(msResolution){
        results_df$TS <- as.numeric(as.character(results_df$TS))/1000
      }

      #The timestamp is a GMT to be interpreted as local time. So we adjust the origin for the timezone, read the time and re-adjust for the timezone
      #!BUG this seems to add an hour (maybe issues with daylight savng)
      #results_df$TS <- as.POSIXct(as.numeric(as.character(results_df$TS)), 
      #                           origin= as.POSIXct("1970-01-01", tz=output_timezone), tz=output_timezone)
      #Instead, we acquire the UTC offset of the target zone, subtract it from the time, and re-apply it by parsing at the target timezone (phew!)
      results_df$TS <- as.POSIXct(results_df$TS - UTCOffsetInSec, origin= "1970-01-01", tz=output_timezone)
      
      
      
      
      if( wide_format){
        results_df <- dcast(results_df, TS ~ metric, fun.aggregate = mean)
      }
    }
    #TODO Either return results_df or do an na.locf on the results df or return as a list
    return(results_df)
#     results_df <- dcast(results_df, ts ~ metric)
#     results_df <- na.locf(results_df, na.rm = FALSE)
#     results_df <- na.locf(results_df, na.rm = TRUE, fromLast = TRUE)
#     return(results_df)
}

# Construct a list for the JSON serializer to construct the query 
# This is the <queries list> part of the main query
# which would be serialized into "queries" : [{metric1},{metric2}]
# Args:
#   metrics    - list of metrics to query e.g. c('Engine-', 'Ambient')
#   downsample - if null, no downsampling is done. 
#       If true, downsampling is done based on the rate and aggregator selected. E.g. 1m-avg finds the average 
#       in cases where no of readings returned are > 1 per minute. As a result, the numer of rows in the result
#       would be restricted to the number of minutes in the time span
#   rate       - Lets you return the diff between two consecutive readings. It is similar to diff(vector) in R
#   aggregator - The aggregator to use when identical TS values are encountered for same tag
#   tags       - named vector where name = key and value = value. e.g. c('serialno' = 'L-2350-2228')
# Returns:
#  Metrics object/List of queries which the JSON serializer can serialize to form the query.
ConstructMetricsSubQuery <- function(metrics, downsample = NULL , 
                                   rate = FALSE, aggregator = "avg", 
                                   tags){
  
  # Construct list of tags
  tag_structure <- ConstructTagsObject(names(tags), unname(tags))
  
  # Generate sub query for each metric
  x <- lapply(1:length(metrics), function(i){ 
                        l = list("metric" = metrics[i], "aggregator" = aggregator , 
                                 "rate" = stringr::str_to_lower(as.character(rate)),
                                 "tags" = tag_structure); 
                        if( !is.null(downsample) ){ 
                          l = append(l, list("downsample"= downsample))}; 
                        return(l)})
  
  #List of all sub queries
  return(x)
}


# Construct list of key - value mappings to be used to filter datapoints in opentsdb
# Most commonly used tag in the analytics scenario is <serialno> - <value> 
# e.g. key = c('SerialNo') value=c('L-2350-2228')
# 
# Args:
#   keys       - names of the tags
#   values     - values to map the names to 
# Returns:
# List of <key, value> pairs
ConstructTagsObject <- function(names, values){
  
  # The order and length of the two vectors is important as each key is assigned to 
  # a value in the corresponding location in the values vector
  if( length(names) != length(values)){
    print("Lengths of names and values array not equal cannot  construct tags object")
    return(NULL)
  }
  
  #return a list
  return(setNames(as.list(values), names))
  
}

# Below is an experimental version of the runsimplemetricquery in parallel
# Feel free to use it but verify and validate results
# should you notice any discrepancies please notify with error message/differences in returned data
# This is the most frequently used endpoint of opentsdb - to fetch data
# The following function is a simpler version where all metrics have to use the same parameters
# for query including downsampling rate, rate and aggregators. 
# For more complicated combinations, use ConstructMetricsSubQuery to construct the subqueries and 
# pass it on to RunMetricQuery 
# Args:
#   start_ts          -  start time of the query
#   end_ts            -  end time of the query
#   output_timezone - timezone to convert the results' TS to 
#   metrics           -  vector of metric names
#   machine_serial_no -  serial number of machine
#   timestamp_format  -  what is the format of the start_ts, end_ts values. 
#                        For time values directly queried from system via R interface, 
#                        this would be default POSIXct
#   msResolution      -  Millisecond resolution. 
#   showTSUIDs        -  whether TSUID should be returned for each metric
#   aggregator        -  What aggregation to use when multiple timestamps have same value
#                       and then set metrics in this function to c(metric_query_1, metric_query_2)
#  timeout_max        -  the HTTPRequest does not always succeed in establishing connection with remote server. This option lets
#                       the user program how many times the request should be retried before expiring.
#  downsample         - Should downsampling be used
#  rate               - Should values be returned or rate of change for the metric 
# Returns:
#  All datapoints for the selected metrics 
# NOTE _ Cannot be used with downsample option or if using, don't expecft identical results as the
# non parallel version. You can set up a custom parallel version to do the same depending on custom needs
RunSimpleMetricQueryParallel <- function(start_ts, end_ts, metrics, 
                                 machine_serial_no, 
                                 output_timezone= "UTC",
                                 #timestamp_format = available_timestamp_formats$POSIXCT,
                                 msResolution = TRUE, showTSUIDS = FALSE, aggregator = "avg", 
                                 timeout_max = 3, downsample = NULL, rate = FALSE, wide_format = FALSE){
  
  metrics_sub_query <- ConstructMetricsSubQuery(metrics = metrics, downsample = downsample, 
                                                rate = rate, aggregator = aggregator, 
                                                tags = c('SerialNo'= as.character(machine_serial_no)))
  # Setting up parallel environment isnt always beneficial especially 
  # when set up cost is higher than running the code
  # Put a lower bound on what requests can be ru nin parallel
  # For the timebeing set it equal to 4 days. Anything less than this would all be fetched at once
  # TODO - change it further to add calculation on amount of data being fetched based on number of tags
  if( difftime(end_ts, start_ts, "days" ) <= 4 || !is.null(downsample)){
    RunMetricQuery(start_ts = start_ts, end_ts = end_ts, output_timezone = output_timezone, #timestamp_format = timestamp_format,
                   msResolution = msResolution, showTSUIDs = showTSUIDS, 
                   metrics = metrics_sub_query, timeout_max = timeout_max, wide_format = wide_format)
  }
  
  #set up 
  library(doParallel)
  ncores  <- detectCores() - 1
  registerDoParallel(cores = ncores)
  
  # Divide the time span into equal time frames
  dts_seq  <- seq(start_ts, end_ts, length.out = ncores)
  
  #Run parallel data fetch
  data_ind <- foreach( i = 1:length(dts_seq) - 1 ) %dopar% {
                      source("G:\\Remote Health Monitoring\\OpenTSDB\\OpenTsdbLibV2.R")
                      results  <- RunMetricQuery(start_ts = dts_seq[i], end_ts = dts_seq[i + 1], 
                                                 output_timezone = output_timezone, 
                                                 msResolution = msResolution, showTSUIDs = showTSUIDS, 
                                                 metrics = metrics_sub_query, timeout_max = timeout_max, 
                                                 wide_format = FALSE)
                      return(results)
  }
  # stop implicit cluster 
  # Please note, if you see any errors, there is currently one known way to debug, 
  # change the parallel to non parallel version and check where it fails
  stopImplicitCluster()
  
  #combine results into one data frame
  data_combined <- do.call("rbind.fill", data_ind)
  
  if(wide_format){
    data_combined <- dcast(data_combined, TS ~ metric, fun.aggregate = mean)
  }
  #optional step to sort by TS 
  #data_combined <- data_combined[order(data_combined$TS),]
  
  # The results returned belwo should be identical to the one returned by 
  # RunSimpleMetricQuery when the parameters are identical
  return(data_combined)
}


# Function inserts data into openTSDB.
# Pass a DataFrame, which must have a 'TS' columns with timestamps
# Values should be numeric, anything else will not get inserted and trigger a warning message
# Args:
#   insertSeries    - Data frame of values to insert. WIll use column names as Metric names when inserting.
#                     All values must be numeric. Tolerates NA values. Tolerates repeated values (removes them)
#   machineSerialNo - 
#   dataSource      - Where does the data come from, i.e. name of the algorithm/rule that generated it
#   dataQuality     - 
#   dataType        - 'Derived' for processed data, otherwise 'raw' if it comes from sensors
# Returns:
#                   - HTTP status code message, "Success: (204) No Content" for a successful insert
#                     and (400) Bad Request for a unsuccessful insert

insertDataToOpenTsdb <- function(insertSeries, machineSerialNo, dataSource, dataQuality = 0.0, 
                                 dataType="Derived"){
  
  
  #subfunction that creates a payload from a single time series. We'll lapply over this function for each series.
  generatePayloadforSingleMetric <- function(series, machineSerialNo, dataSource, dataQuality = 0.0, 
                                             dataType="Derived"){
    #remove any NA value from this time-series
    series <- series[!is.na(series[ ,2]), ]
    
    #converting from POSIXct to numeric: openTSDB wants 'epoch' times
    series$TS <- as.numeric(series$TS)
    
    #checking we are inserting Numeric: OpenTSDB only acceopts those formats
    if(is.numeric(series$TS)!=TRUE | is.numeric(series[ ,2])!=TRUE){
      warning(paste("Error:Trying to insert non numeric data into OpenTSDB: ", names(series)[2]))
      return(NULL)
    } 
    
    #do rle on dataframe to remove redundant values 
    resultTobeSentDF <- data.frame(TS    = series$TS[cumsum(rle(series[ ,2])$length)+1],
                                   value = rle(series[ ,2])$values)
    resultTobeSentDF$TS <- c(series$TS[1],resultTobeSentDF$TS[-nrow(resultTobeSentDF)])
    
    #construct header or meta data or static part of payload
    metaData <- list('SerialNo' = machineSerialNo, 'DataSource' = dataSource,
                     'DataType' = dataType, 'DataQuality' = dataQuality)
    
    #extract the metric names from the column names of the series
    metricNames <- names(series)[2]
    
    #construct payload: merge meta data and dynamic part
    singleMetricPayload <- lapply(1:nrow(resultTobeSentDF),
                                  function(x) list(timestamp = resultTobeSentDF$TS[x],
                                                   value     = resultTobeSentDF$value[x],
                                                   metric    = metricNames,
                                                   tags      = metaData))
    
    return(singleMetricPayload)
  }
  
  
  
  #get all column names (don't keep TS)
  metricNames <- names(insertSeries)[names(insertSeries) != "TS"]
  #remove columns that are full of NAs (i.e. no values in this series)
  #drop=FALSE means that, if there is only one colum selected, the result swill still be a DF (instead of being converted to Vector)
  #without drop=FALSE, instead of iterating over the columns of the DF, it will iterate over the elements of the vector...
  metricNames <- metricNames[!sapply(insertSeries[ ,metricNames, drop=FALSE], function(x)all(is.na(x)))]
  #generate the JSON for every column
  payloads <- lapply(metricNames, function(X) generatePayloadforSingleMetric(insertSeries[c("TS",X)], 
                                                                             machineSerialNo, 
                                                                             dataSource, 
                                                                             dataQuality, 
                                                                             dataType))
  #bind all the payloads
  payload <-do.call(c,payloads)
  #convert the payloads to JSON
  jsonPayload <- toJSON(payload,auto_unbox=TRUE,pretty=TRUE)
  #print(jsonPayload)
  #setup proxy
  InitHttpProxy() 
  
  # construct queryUrl using Server Information
  url_endpoint = "/api/put?"
  queryUrl = paste0(connection_obj$url, ":", connection_obj$port, url_endpoint)
  
  
  #post data to OpenTsDB
  request <- POST(url = queryUrl, body = jsonPayload)
  stop_for_status(request)
  
  #return status of the request
  return(http_status(request)$message)
}



# Setting parallel = TRUE Please note the below uses parallel data fetch. Any analysis that is already using several cores
# may hang waiting for OS resources, in a deadlock condition. Use only with analyses that 
# are sure to be using only one core and other cores available. 
# Parallel  = tRUE takes longer if number of queries are smaller, use only when justified
SimulateUnionQuery <- function(triggerTimes, metric, 
                               window.BeforeEvent,window.AfterEvent, 
                               machine, parallel = FALSE){
  if( is.null(triggerTimes) || !length(triggerTimes)){
    print("Nothing to query")
    return(NULL)
  }
  data_ind <- list()
  #apply the the window to the trigerTimes
  startTimes <- triggerTimes - window.BeforeEvent
  endTimes   <- triggerTimes + window.AfterEvent
  #Compute union of all intervals to  avoid double-querying
  #The interval library only works over Matrices of numeric...
  intervalMatrix <- as.matrix(cbind(as.numeric(startTimes),
                                    as.numeric(endTimes))) 
  uInterv <- Intervals(intervalMatrix) #convert to interval object
  uInterv <- interval_union(uInterv)   #compute the Union of all intervals to avoid query overlapping times
  
  #restore into original variable format (POSIXct) 
  queryTimeIntervals <- data.frame(start = as.POSIXct(uInterv[,1], 
                                           origin = "1970-01-01", 
                                           tz = machine$mineTimeZone), 
                    end   = as.POSIXct(uInterv[,2], 
                                            origin = "1970-01-01", 
                                            tz = machine$mineTimeZone)) #BEWARE OF TZ !!!
  
  if( parallel){
    library(doParallel)
    no_cores_available <- detectCores() - 1
    
    cl2 <- makeCluster(no_cores_available)
    
    clusterExport(cl2, varlist = c( 'machine', 'queryTimeIntervals', 'metric'),
                  envir=environment())
    data_ind <- parLapply(cl2, 1:nrow(queryTimeIntervals), function(i) {
      #source("G:\\Remote Health Monitoring\\OpenTSDB\\OpenTsdbLibV2.R")
      source("G:\\DEPARTMENT\\Smart Services\\Remote Health Monitoring\\OpenTSDB\\OpenTsdbLibV2.R")
      
      data <- RunSimpleMetricQuery(queryTimeIntervals[i,"start"], queryTimeIntervals[i, "end"], metric, 
                                   machine$machineSN, machine$mineTimeZone, 
                                   downsample = NULL,
                                   wide_format = FALSE)
      
      return(data)})
    
    stopCluster(cl2)
    
  }else{
#     dts$brks  <- cut(dts$start, breaks = "hours")
#     dts_mod   <- ddply(dts, ~brks, summarize, start = head(start, 1), end = tail(end, 1))
    print(paste0("Starting query fetch at ", Sys.time()))
    data_ind <- lapply(1:nrow(queryTimeIntervals) ,function(i){
      print(paste0("Starting sub query ", i, " at --> " , Sys.time()))
      data <- RunSimpleMetricQuery(queryTimeIntervals[i,"start"], 
                                   queryTimeIntervals[i, "end"], metric, 
                                   machine$machineSN, machine$mineTimeZone, 
                                   downsample = NULL,
                                   wide_format = FALSE)
      print(paste0("Ending sub query ", i, " at  --> ", Sys.time()))
      
      return(data)
    })
  }
  print(paste0("Ending query fetch at ", Sys.time()))
  raw_data <- do.call("rbind.fill", data_ind)
  return(raw_data)
}

