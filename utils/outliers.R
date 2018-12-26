rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))

getLibrary("data.table")
getLibrary("lubridate")
getLibrary("dplyr")

get_outliers <- function(dt, input_time_grouper){

  dt$floor_date <- floor_date(dt$updatetime, paste(as.character(input_time_grouper), " mins"))
  dt$floor_hour <- ifelse(hour(dt$floor_date) < 10, paste('0', hour(dt$floor_date), sep=''), hour(dt$floor_date))
  dt$floor_minute <- ifelse(minute(dt$floor_date) < 10, paste('0', minute(dt$floor_date), sep=''), minute(dt$floor_date))
  dt$floor_time <- paste(dt$floor_hour, dt$floor_minute, sep=":")
  
  tt_dt_w_iqr <- dt[,list(q3=quantile(delay, 3/4),q1=quantile(delay, 1/4), count = .N),by=list(name,weekday,floor_time)]
  tt_dt_w_iqr$iqr <- tt_dt_w_iqr$q3 - tt_dt_w_iqr$q1

  dt <- dplyr::inner_join(dt, tt_dt_w_iqr, by = c("name","weekday","floor_time"))
  
  dt$outlier <- ifelse(dt$delay > dt$q3 + 1.5*dt$iqr |
                       dt$delay < dt$q1 - 1.5*dt$iqr, 
                       1, 
                       0)
  
  return(dt)
}