# get the path of the project file in order to normalize sources
# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

source(file.path(rproj_dir,"utils","plot_parameters.R"))
#source(file.path(rproj_dir,"utils","library.R"))

library("plotly")

agg_travel_times_w_o_outliers <- function(dt, input_route, input_date, input_time_grouper){
  p <- plot_ly() %>%
    add_trace(x = dt$updatetime[which(dt$name == input_route & dt$date == input_date)], 
              y = dt$delay[which(dt$name == input_route & dt$date == input_date)],
              hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date)]),
              name = 'travel time agreggated',
              type = 'scatter', 
              mode = 'lines+markers', 
              line = list(width = 2, color = c_primary_trace),
              marker = list(size = 4, color = c_primary_trace)
              # error_y = list(array = dt$stdv[which(dt$name == input_route & dt$date == input_date)],
              #                opacity = 0.5)
    ) %>%
    layout(title = paste('Perfil agregados cada', input_time_grouper,'minutos para el dia', input_date, '(sin outliers)'),
           yaxis = list(title="s/km")) %>%
    layout(margin = plot_margins)
  
  return(p)
}