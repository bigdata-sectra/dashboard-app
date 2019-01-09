# get the path of the project file in order to normalize sources
rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","plot_parameters.R"))
source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","get_day_type.R"))

getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")

boxplot <- function(dt, input_day_type, input_route, input_date){
  if (input_day_type == 'day_type') {
    dia_texto <- get_day_type(lubridate::wday(input_date, week_start = 1))
    p <- plot_ly(y = dt$delay[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))], 
                 x = dt$floor_time[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                 name = 'boxplot tiempos',
                 type = "box", 
                 text = paste("Value :", dt$delay[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                              "<br> total tramo horario :", dt$count[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                              "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))]),
                 boxpoints = "all", 
                 jitter = 0,
                 pointpos = 0,
                 marker = list(color = c_primary_trace),
                 line = list(color = c_primary_trace)
                 )
      
    if (sum(dt$outlier[which(dt$name == input_route & dt$date == input_date)]) > 0){
      p <- add_trace(p,
                     y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)], 
                     x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                     name = paste('outliers dia ', as.character(input_date)),
                     hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                       "<br> outliers agrupados :", dt$count[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                       "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)]),
                     mode = 'markers',
                     type = 'scatter',
                     inherit = F,
                     marker = list(color = c_dots))
    }
       
    p <- layout(p,
                title = paste('Boxplot de tiempos de viaje para los dias', dia_texto), 
                yaxis=list(title="s/km")) 
    p <- layout(p,                   
                legend = list(orientation = 'h', 
                              xanchor= "center",
                              y= 1.1,
                              x= 0.5)) 
    p <- layout(p, 
                margin = plot_margins) 
    
  }
  else {
    dia_texto <- lubridate::wday(input_date, label = TRUE, abbr = FALSE)
    p <- plot_ly(y = dt$delay[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))], 
                 x = dt$floor_time[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))], 
                 type = "box", 
                 text = paste("Value :", dt$delay[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))],
                              "<br> N :", dt$count[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))],
                              "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))]),
                 boxpoints = "all", 
                 jitter = 0.3,
                 pointpos = 0
    )
      
    if (sum(dt$outlier[which(dt$name == input_route & dt$date == input_date)]) > 0){
      p <- add_trace(p,
                     y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)], 
                     x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                     name = paste('outliers dia ', as.character(input_date)),
                     hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                       "<br> N :", dt$count[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                       "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)]),
                     mode = 'markers',
                     type = 'scatter',
                     inherit = F)
    }
    
    p <- layout(p,
                title = paste('Boxplot de tiempos de viaje para los dias', dia_texto), 
                yaxis=list(title="s/km"))
    p <- layout(p, 
                legend = list(orientation = 'h', 
                           xanchor= "center",
                           y= 1.1,
                           x= 0.5))
    p <- layout(p, 
                    margin = plot_margins)  
  }
  

  return(p)
}

