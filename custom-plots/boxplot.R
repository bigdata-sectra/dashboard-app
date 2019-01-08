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
                              "<br> N :", dt$count[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))],
                              "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$day_type == substr(dia_texto, 1, 1))]),
                 boxpoints = "all", 
                 jitter = 0,
                 pointpos = 0
    ) %>% 
      add_trace(y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)], 
                x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                name = paste('outliers dia ', as.character(input_date)),
                hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                  "<br> N :", dt$count[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                  "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)]),
                mode = 'markers',
                type = 'scatter',
                inherit = F) %>%
      layout(title = paste('Boxplot de tiempos de viaje para los dias', dia_texto), 
             yaxis=list(title="s/km")) %>%  
      layout(legend = list(orientation = 'h', 
                           xanchor= "center",
                           y= 1.1,
                           x= 0.5)) %>% 
      layout(margin = plot_margins) 
    
    # %>% 
    #   layout(annotations = list(
    #     x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
    #     y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)], 
    #     text = "",
    #     showarrow = TRUE,
    #     arrowhead = 6,
    #     ax = 0,
    #     ay = 0,
    #     xanchor = "center"
    #   )
      # ) 
    
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
      add_trace(y = dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)], 
                x = dt$floor_time[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                name = paste('outliers dia ', as.character(input_date)),
                hovertext = paste("Value :", dt$delay[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                  "<br> N :", dt$count[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)],
                                  "<br> Updatetime :", dt$updatetime[which(dt$name == input_route & dt$date == input_date & dt$outlier == 1)]),
                mode = 'markers',
                type = 'scatter',
                inherit = F) %>%
      layout(title = paste('Boxplot de tiempos de viaje para los dias', dia_texto), 
             yaxis=list(title="s/km")) %>%  
      layout(legend = list(orientation = 'h', 
                           xanchor= "center",
                           y= 1.1,
                           x= 0.5)) %>% 
      layout(margin = plot_margins)  
  }
  

  return(p)
}

