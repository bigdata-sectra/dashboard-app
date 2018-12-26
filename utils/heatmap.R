rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))

getLibrary("data.table")
getLibrary("lubridate")
getLibrary("dplyr")

heatmap_generator <- function(dt, input_route, input_time_grouper, bg_color, container_color){
  
  dt$floor_date <- floor_date(dt$updatetime, paste(as.character(input_time_grouper), " mins"))
  dt$floor_hour <- ifelse(hour(dt$floor_date) < 10, paste('0', hour(dt$floor_date), sep=''), hour(dt$floor_date))
  dt$floor_minute <- ifelse(minute(dt$floor_date) < 10, paste('0', minute(dt$floor_date), sep=''), minute(dt$floor_date))
  dt$floor_time <- paste(dt$floor_hour, dt$floor_minute, sep=":")
  
  p <- plot_ly(x = dt$floor_time[which(dt$name == input_route)],
               y = dt$date[which(dt$name == input_route)],
               z = dt$delay[which(dt$name == input_route)], 
               type = "heatmap", 
               colorscale = "Set2") %>%
    layout(title = paste('Mapa de calor cada', input_time_grouper,'mins. para la ruta', input_route, '(sin outliers)')) %>%
    layout(plot_bgcolor = bg_color) %>% 
    layout(paper_bgcolor = container_color)
  
  return(p)
}