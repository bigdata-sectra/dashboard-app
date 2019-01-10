# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

#source(file.path(rproj_dir,"utils","library.R"))
source(file.path(rproj_dir,"utils","paths.R"))

library("data.table")
library("fasttime")
library("lubridate")
library("dplyr")

travel_times_processing <- function() {
  travel_times_dt <- fread(file.path(data_20181217,"travel_times_17.12.2018.csv"))
  routes_dt <- fread(file.path(data_20181217,"routes_17.12.2018.csv"))
  
  travel_times_dt <- unique(travel_times_dt, by=c("name", "updatetime"))
  
  travel_times_dt$updatetime <- fastPOSIXct(travel_times_dt$updatetime, tz = "GMT")
  travel_times_dt <- travel_times_dt[which(travel_times_dt$updatetime < fastPOSIXct("2018-12-01") &
                                             travel_times_dt$updatetime >= fastPOSIXct("2018-11-01")),]

  travel_times_dt <- dplyr::inner_join(travel_times_dt, routes_dt[, c("name", "length")], by = "name")

  travel_times_dt$delay <- 1000 * travel_times_dt$time / travel_times_dt$length
  
  travel_times_dt$date <- as.Date(travel_times_dt$updatetime)
  
  travel_times_dt$weekday <- wday(travel_times_dt$date, week_start = 1)
  
  travel_times_dt$day_type <- ifelse(travel_times_dt$weekday <= 5, "L",
                                     ifelse(travel_times_dt$weekday == 6, "S",
                                            "D"))
  travel_times_dt$velocity <- 3.6 * travel_times_dt$length / travel_times_dt$time
  
  return(setDT(travel_times_dt))
}

routes_processing <- function() {
  routes_dt <- fread(file.path(data_20181217,"routes_17.12.2018.csv"))
  routes_dt$start_date <- fastPOSIXct(routes_dt$start_date)
  
  
  return(routes_dt)
}

