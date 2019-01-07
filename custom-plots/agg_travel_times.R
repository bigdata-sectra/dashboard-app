# get the path of the project file in order to normalize sources
rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","plot_parameters.R"))
source(file.path(rproj_dir,"utils","getLibrary.R"))

getLibrary("plotly")

agg_travel_times <- function(dt, input_route, input_date, input_time_grouper){
  p <- plot_ly() %>%
    add_trace(x = dt$updatetime[which(dt$name == input_route & dt$date == input_date)], 
              y = dt$delay[which(dt$name == input_route & dt$date == input_date)],
              name = 'travel time agreggated',
              type = 'scatter', 
              mode = 'lines+markers', 
              line = list(width = 2),
              marker = list(size = 4)) %>%
    layout(title = paste('Datos agregados cada', input_time_grouper,'minutos para el dia', input_date, '(con outliers)')) %>%
    layout(margin = plot_margins)
  
  return(p)
}