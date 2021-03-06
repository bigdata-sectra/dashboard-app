rproj_dir <- getwd()

source(file.path(rproj_dir,"utils","paths.R"))

library("data.table")
library("fasttime")
library("lubridate")
library("dplyr")
library("httr")
library("readr")

travel_times_processing <- function() {
  travel_times_dt <- fread(file.path(data_20190318,"travel_times_2019.04.29.csv"))
  routes_dt <- fread(file.path(data_20190318,"routes_2019.04.29.csv"))
  
  travel_times_dt <- unique(travel_times_dt, by=c("name", "updatetime"))
  
  travel_times_dt$updatetime <- fastPOSIXct(travel_times_dt$updatetime, tz = "GMT")
  # travel_times_dt <- travel_times_dt[which(travel_times_dt$updatetime < fastPOSIXct("2018-12-01") &
  #                                            travel_times_dt$updatetime >= fastPOSIXct("2018-11-01")),]

  travel_times_dt <- dplyr::inner_join(travel_times_dt, routes_dt[, c("name", "length")], by = "name")

  travel_times_dt$delay <- 1000 * travel_times_dt$time / travel_times_dt$length
  
  travel_times_dt$historicdelay <- 1000 * travel_times_dt$historictime / travel_times_dt$length
  
  travel_times_dt$date <- as.Date(travel_times_dt$updatetime)
  
  travel_times_dt$weekday <- wday(travel_times_dt$date, week_start = 1)
  
  travel_times_dt$day_type <- ifelse(travel_times_dt$weekday <= 5, "L",
                                     ifelse(travel_times_dt$weekday == 6, "S",
                                            "D"))
  travel_times_dt$velocity <- 3.6 * travel_times_dt$length / travel_times_dt$time
  
  return(setDT(travel_times_dt))
}

routes_processing <- function() {
  routes_dt <- fread(file.path(data_20190318,"routes_2019.04.29.csv"))
  routes_dt$start_date <- fastPOSIXct(routes_dt$start_date)
  
  
  return(routes_dt)
}

dict_loading <- function(){
  x <- GET("https://raw.githubusercontent.com/bigdata-sectra/documents-hub/518906459ac586448cc303d64a2412908c9af453/waze/dicc-tramos-waze.csv?token=AKAZEUMG227U77PIG7XMAJ243QRQ4",
           authenticate(Sys.getenv("GITHUB_USER"),Sys.getenv("GITHUB_R_TOKEN")))
  dict_dt <- content(x, type="text/csv", encoding = 'latin1')
  return(dict_dt)
  
}

# "https://raw.githubusercontent.com/bigdata-sectra/documents-hub/master/waze/dicc-tramos-waze.csv"
