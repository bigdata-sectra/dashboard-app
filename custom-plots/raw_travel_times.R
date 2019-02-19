# get the path of the project file in order to normalize sources
rproj_dir <- getwd()

source(file.path(rproj_dir,"utils","plot_parameters.R"))

library("plotly")


raw_travel_times <- function(dt, input_route, input_date){
  
  p <- plot_ly() 
  p <- add_trace(p, x = dt$updatetime[which(dt$name == input_route & dt$date == input_date)],
                 y = dt$delay[which(dt$name == input_route & dt$date == input_date)], 
                 name = 'travel time', 
                 mode = 'lines+markers',
                 type = 'scatter',
                 line = list(width = 2, color = c_primary_trace),
                 marker = list(size = 4, color = c_primary_trace))
  if (sum(dt$outlier[which(dt$name == input_route & dt$date == input_date)]) > 0) {
    p <- add_trace(p, x = dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                   y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                   hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                     "<br> Outlier respecto de :", dt$count[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)]),
                   name = 'outlier',
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(color = c_dots)
    )
  }
  
  # adding trace of historic delay
  p <- add_trace(p, x = dt$updatetime[which(dt$name == input_route & dt$date == input_date)],
                 y = dt$historicdelay[which(dt$name == input_route & dt$date == input_date)], 
                 name = 'historic travel time', 
                 mode = 'lines+markers',
                 type = 'scatter',
                 line = list(width = 2, color = c_background_trace),
                 marker = list(size = 4, color = c_background_trace),
                 opacity = 0.8,
                 visible = "legendonly")
  
  #TODO: dplyr "%>%" operator is not working properly 
  p <- layout(p, title = paste('Datos crudos dia', input_date))
  p <- layout(p, yaxis = list(title="s/km"))
  p <- layout(p, legend = list(x = 0.05, y = 0.95))
  p <- layout(p, margin = plot_margins)
  
  return(p)
  
}