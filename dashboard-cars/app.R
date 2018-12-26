rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","paths.R"))
source(file.path(rproj_dir,"utils","data_processing.R"))
source(file.path(rproj_dir,"utils","outliers.R"))
source(file.path(rproj_dir,"utils","get_polyline.R"))
source(file.path(rproj_dir,"utils","get_arrowhead.R"))

#----- packages -----#
getLibrary("shiny")
getLibrary("shinythemes")
getLibrary("data.table")
getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")
getLibrary("leaflet")

#----- colors -----#
line_color <- 'rgb(64,224,208)'
bg_color <- '#272B30'
container_color <- '#1c1e22'

#----- data -----#
tt_dt <- travel_times_processing()
r_dt <- routes_processing()
routes_options <- r_dt$name

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Car Travel Times"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(style = "position:fixed;width:inherit;",
      selectInput('route', label = 'Pick a route: ', choices = routes_options),
      dateInput('date1', label = 'Pick a date: ', value = as.Date("2018-11-01")),
      sliderInput('time_grouper', label = 'Pick a time interval: ', min = 5, max = 60, value = 15, step = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("routes_map"),
      plotlyOutput("travel_time_plot"),
      plotlyOutput("travel_time_agg_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$routes_map <- renderLeaflet({
    polyline_matrix <- get_polyline_matrix(r_dt$line[which(r_dt$name == input$route)])
    from_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix)-1, 1], 
                             y = polyline_matrix[nrow(polyline_matrix)-1, 2])
    to_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix), 1], 
                           y = polyline_matrix[nrow(polyline_matrix), 2])
    arrow_head <- get_arrowhead (from_point_arrow, to_point_arrow)
    leaflet() %>% 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
                           attribution = paste(
                             "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                             "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
                           )
    ) %>% 
      setView(lng = polyline_matrix[floor(nrow(polyline_matrix)/2),1], 
              lat = polyline_matrix[floor(nrow(polyline_matrix)/2),2], 
              zoom = 16) %>%
      addPolylines(lng = polyline_matrix[,1], lat = polyline_matrix[,2]) %>%
      addPolylines(lng = arrow_head[,"x"], lat = arrow_head[,"y"], color = "#ff0033") # arrow head
  })
  
  tt_dt_w_outliers <- reactive(get_outliers(tt_dt, input$time_grouper))
  
  # plot of raw data
  output$travel_time_plot <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    plot_ly() %>%
      add_trace(x = tt_dt_out$updatetime[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1)],
                y = tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1)], 
                name = 'travel time', 
                mode = 'lines',
                type = 'scatter',
                line = list(color = line_color, width = 2)) %>%
      add_trace(x = tt_dt_out$updatetime[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)], 
                y = tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)],
                hovertext = paste("Value :", tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)],
                                  "<br> N :", tt_dt_out$count[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)]),
                name = 'outlier', 
                mode = 'markers',
                type = 'scatter') %>% 
      layout(plot_bgcolor = bg_color) %>% 
      layout(paper_bgcolor = container_color)
    })
  
  # plot of agreggated data
  tt_dt_grouped <- reactive({tt_dt %>%
                    group_by(name, date, updatetime = floor_date(tt_dt$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
                    summarise(delay = mean(delay))})
  
  output$travel_time_agg_plot <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    plot_ly() %>%
      add_trace(x = tt_dt_grouped$updatetime[which(tt_dt_grouped$name == input$route & tt_dt_grouped$date == input$date1)], 
                y = tt_dt_grouped$delay[which(tt_dt_grouped$name == input$route & tt_dt_grouped$date == input$date1)],
                name = 'travel time agreggated',
                type = 'scatter', 
                mode = 'lines+markers', 
                line = list(color = line_color, width = 2),
                marker = list(color = line_color)) %>% 
      layout(plot_bgcolor = bg_color) %>% 
      layout(paper_bgcolor = container_color)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# runApp('dashboard-cars', host = "0.0.0.0", port = 8080)