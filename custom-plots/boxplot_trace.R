# get the path of the project file in order to normalize sources
# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

source(file.path(rproj_dir,"utils","plot_parameters.R"))
#source(file.path(rproj_dir,"utils","library.R"))
source(file.path(rproj_dir,"utils","get_day_type.R"))

library("plotly")
library("lubridate")
library("dplyr")

boxplot_trace <- function(dt, dt_grouped, input_day_type, input_route, input_date){
  if (input_day_type == 'day_type') {
    dia_texto <- get_day_type(lubridate::wday(input_date, week_start = 1))
    p <- plot_ly(y = dt$delay[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))], 
                 x = dt$floor_time[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                 name = 'boxplot tiempos',
                 type = "box", 
                 text = paste("Value :", dt$delay[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                              "<br> N :", dt$count[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                              "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))]),
                 marker = list(color = c_background_trace),
                 line = list(color = c_background_trace)
    ) %>% 
      add_trace(x = dt_grouped$floor_time[which(dt_grouped$name == input_route & dt_grouped$date == input_date)], 
                y = dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date)],
                name = paste('Perfil dia ', as.character(input_date)),
                type = 'scatter', 
                mode = 'lines+markers', 
                line = list(width = 2, color = c_primary_trace),
                marker = list(size = 4, color = c_primary_trace),
                inherit = F)
    
    if (sum(dt_grouped$out_sum[which(dt_grouped$name == input_route & dt_grouped$date == input_date)]) > 0) {
      p <- add_trace(p, 
                     x = dt_grouped$floor_time[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                     y = dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                     hovertext = paste("Value :", dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                                       "<br> outliers agrupados :", dt_grouped$out_sum[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)]),
                     name = paste('calculado con outliers - dia ', as.character(input_date)),
                     mode = 'markers',
                     type = 'scatter',
                     marker = list(color = c_dots),
                     inherit = F
      )
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
    ) %>% 
      add_trace(x = dt_grouped$floor_time[which(dt_grouped$name == input_route & dt_grouped$date == input_date)], 
                y = dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date)],
                name = paste('Perfil dia ', as.character(input_date)),
                type = 'scatter', 
                mode = 'lines+markers', 
                line = list(width = 2, color = c_primary_trace),
                marker = list(size = 4, color = c_primary_trace),
                inherit = F)
    
    if (sum(dt_grouped$out_sum[which(dt_grouped$name == input_route & dt_grouped$date == input_date)]) > 0) {
      p <- add_trace(p, 
                     x = dt_grouped$floor_time[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                     y = dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                     hovertext = paste("Value :", dt_grouped$delay[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)],
                                       "<br> outliers agrupados :", dt_grouped$out_sum[which(dt_grouped$name == input_route & dt_grouped$date == input_date & dt_grouped$out_sum > 0)]),
                     name = paste('calculado con outliers - dia ', as.character(input_date)),
                     mode = 'markers',
                     type = 'scatter',
                     marker = list(color = c_dots),
                     inherit = F
      )
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

