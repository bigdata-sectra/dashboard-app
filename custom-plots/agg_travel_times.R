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
              marker = list(size = 4))
    
    if (sum(dt$out_sum[which(dt$name == input_route & dt$date == input_date)]) > 0) {
      p <- add_trace(p, 
                     x = dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                     y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                     hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)],
                                       "<br> N :", dt$out_sum[which(dt$name == input_route & dt$date == input_date & dt$out_sum > 0)]),
                     name = 'calculated with outliers',
                     mode = 'markers',
                     type = 'scatter'
                     )
    }
    p <- layout(p, title = paste('Perfil agregados cada', input_time_grouper,'minutos para el dia', input_date, '(con outliers)'), 
           yaxis = list(title="s/km"))
    p <- layout(p, legend = list(x = 0.05, y = 0.95))
    p <- layout(p, margin = plot_margins)
  
  return(p)
}