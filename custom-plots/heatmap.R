# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

#source(file.path(rproj_dir,"utils","library.R"))
source(file.path(rproj_dir,"utils","plot_parameters.R"))

library("plotly")
library("lubridate")
library("dplyr")

heatmap_w_outliers <- function(dt, input_route, input_time_grouper, input_date){
  
  dt$floor_date <- floor_date(dt$updatetime, paste(as.character(input_time_grouper), " mins"))
  dt$floor_hour <- ifelse(hour(dt$floor_date) < 10, paste('0', hour(dt$floor_date), sep=''), hour(dt$floor_date))
  dt$floor_minute <- ifelse(minute(dt$floor_date) < 10, paste('0', minute(dt$floor_date), sep=''), minute(dt$floor_date))
  dt$floor_time <- paste(dt$floor_hour, dt$floor_minute, sep=":")
  
  p <- plot_ly(x = dt$floor_time[which(dt$name == input_route)],
               y = dt$date[which(dt$name == input_route)],
               z = dt$delay[which(dt$name == input_route)], 
               type = "heatmap",
               name = 'heatmap',
               colors = colorRamp(c(c_scale_low, c_scale_high)))
    
    p <- layout(p, title = paste('Mapa de calor cada', input_time_grouper,'mins (con outliers)'))
    p <- layout(p, margin = plot_margins)
    p <- layout(p, hovermode = 'compare')
  
  return(p)
}