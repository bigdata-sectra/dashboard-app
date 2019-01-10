# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

#source(file.path(rproj_dir,"utils","library.R"))

library("data.table")
library("lubridate")
library("dplyr")

get_outliers <- function(dt, input_time_grouper, day_type_grouper = 'weekday'){

  dt$floor_date <- floor_date(dt$updatetime, paste(as.character(input_time_grouper), " mins"))
  dt$floor_hour <- ifelse(hour(dt$floor_date) < 10, paste('0', hour(dt$floor_date), sep=''), hour(dt$floor_date))
  dt$floor_minute <- ifelse(minute(dt$floor_date) < 10, paste('0', minute(dt$floor_date), sep=''), minute(dt$floor_date))
  dt$floor_time <- paste(dt$floor_hour, dt$floor_minute, sep=":")
  
  tt_dt_w_iqr <- dt[,list(q3=quantile(delay, 3/4, type = 1),q1=quantile(delay, 1/4, type = 1), count = .N),by=c('name',day_type_grouper,'floor_time')]
  tt_dt_w_iqr$iqr <- tt_dt_w_iqr$q3 - tt_dt_w_iqr$q1

  dt <- dplyr::inner_join(dt, tt_dt_w_iqr, by = c("name",day_type_grouper,"floor_time"))
  
  dt$outlier <- ifelse(dt$delay > dt$q3 + 1.5*dt$iqr |
                       dt$delay < dt$q1 - 1.5*dt$iqr, 
                       1, 
                       0)
  
  return(dt)
}