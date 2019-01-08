rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","plot_parameters.R"))

getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")

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
               colors = colorRamp(c("#4ECDC4", "#FF6B6B"))) %>%
    add_trace(x = dt$floor_time[which(dt$name == input_route & dt$date == input_date)],
              y = dt$date[which(dt$name == input_route & dt$date == input_date)],
              name = as.character(input_date),
              type = "scatter",
              mode = 'lines+markers',
              hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date)]
                                ),
              inherit = F,
              line = list(width = 2, color = line_color),
              marker = list(size = 4, color = line_color)
              )
    
    if (sum(dt$out_sum[which(dt$name == input_route & dt$date == input_date)]) > 0) {
      p <- add_trace(p, 
                     x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                     y = dt$date[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                     hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                                       "<br> N :", dt$out_sum[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)]),
                     name = 'calculated with outliers',
                     mode = 'markers',
                     type = 'scatter',
                     inherit = F,
                     marker = list(size = 4, color = marker_color)
      )
    }
    
    p <- layout(p, title = paste('Mapa de calor cada', input_time_grouper,'mins (sin outliers)'))
    p <- layout(p, margin = plot_margins)
    p <- layout(p, hovermode = 'compare')
    p <- layout(p, showlegend = FALSE)
  
  return(p)
}