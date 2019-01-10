# get the path of the project file in order to normalize sources
# rproj_dir <- rprojroot::find_rstudio_root_file()
rproj_dir <- getwd()

#source(file.path(rproj_dir,"utils","library.R"))
source(file.path(rproj_dir,"utils","get_polyline.R"))
source(file.path(rproj_dir,"utils","get_arrowhead.R"))

library("leaflet")
library("dplyr")

route_map <- function(r_dt, input_route){
  polyline_matrix <- get_polyline_matrix(r_dt$line[which(r_dt$name == input_route)])
  
  #----- calculos para la flecha -----#
  from_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix)-1, 1], 
                           y = polyline_matrix[nrow(polyline_matrix)-1, 2])
  to_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix), 1], 
                         y = polyline_matrix[nrow(polyline_matrix), 2])
  arrow_head <- get_arrowhead(from_point_arrow, to_point_arrow)
  #----- -----#
  
  l <- leaflet() %>% 
    addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
             attribution = paste(
               "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
               "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
             )
    ) %>%
    setView(lng = polyline_matrix[floor(nrow(polyline_matrix)/2),1], 
            lat = polyline_matrix[floor(nrow(polyline_matrix)/2),2], 
            zoom = 15) %>%
    addPolylines(lng = polyline_matrix[,1], lat = polyline_matrix[,2], color = c_primary_trace, opacity = 1) %>%
    addPolylines(lng = arrow_head[,"x"], lat = arrow_head[,"y"], color = c_dots, opacity = 1) # arrow head
  
  return(l)
  
}



