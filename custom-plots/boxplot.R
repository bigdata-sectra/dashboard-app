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
  }
  else {
    dia_texto <- lubridate::wday(input_date, label = TRUE, abbr = FALSE)
  }
  
  p <- plot_ly(y = dt$delay[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))], 
          color = dt$floor_time[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))], 
          type = "box", 
          text = paste("Value :", dt$delay[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))],
                       "<br> N :", dt$count[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))],
                       "<br> day :", dt$floor_date[which(dt$name == input_route & dt$weekday == wday(input_date, week_start = 1))]),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = 0
  ) %>% 
    layout(title = paste('Boxplot de tiempos de viaje para los dias', dia_texto)) %>% 
    layout(plot_bgcolor = bg_color) %>% 
    layout(paper_bgcolor = container_color) %>% 
    layout(margin = plot_margins)
  
  return(p)
}

