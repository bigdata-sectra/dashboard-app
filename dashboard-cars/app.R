# get the path of the project file in order to normalize sources
rproj_dir <- rprojroot::find_rstudio_root_file()

# source parameters and functions
source(file.path(rproj_dir,"utils","paths.R"))
source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","data_processing.R"))
source(file.path(rproj_dir,"utils","outliers.R"))
source(file.path(rproj_dir,"utils","get_polyline.R"))
source(file.path(rproj_dir,"utils","get_arrowhead.R"))
source(file.path(rproj_dir,"utils","heatmap.R"))
source(file.path(rproj_dir,"utils","get_day_type.R"))

#----- packages -----#
getLibrary("shiny")
getLibrary("shinythemes")
getLibrary("data.table")
getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")
getLibrary("leaflet")

#----- colors -----#
line_color <- '#40E0D0'
marker_color <- '#E0D040'
bg_color <- '#272B30'
container_color <- '#1c1e22'

m <- list(
  t = 50 
)

#----- data -----#
tt_dt <- travel_times_processing()
r_dt <- routes_processing()
routes_options <- sort(r_dt$name)

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
      sliderInput('time_grouper', label = 'Pick a time interval: ', min = 5, max = 60, value = 15, step = 5),
      # ,
      # selectInput('display_unit', label = "Select a display unit: ", choices = c('s/km', 'km/h'))
      selectInput('day_type_grouper', label = "Select a day type grouper: ", choices = c('weekday', 'day_type'))
      #,
      #actionButton("do", "Submit")
      #submitButton()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("routes_map"),
      HTML("<br>"),
      plotlyOutput("travel_time_plot"),
      HTML("<br>"),
      plotlyOutput("outliers_boxplots"),
      HTML("<br>"),
      plotlyOutput("travel_time_agg_plot"),
      HTML("<br>"),
      plotlyOutput("travel_time_agg_w_o_outliers_plot"),
      HTML("<br>"),
      plotlyOutput("travel_time_heatmap")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$routes_map <- renderLeaflet({
    polyline_matrix <- get_polyline_matrix(r_dt$line[which(r_dt$name == input$route)])
    # calculos para la flecha
    from_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix)-1, 1], 
                             y = polyline_matrix[nrow(polyline_matrix)-1, 2])
    to_point_arrow <- list(x = polyline_matrix[nrow(polyline_matrix), 1], 
                           y = polyline_matrix[nrow(polyline_matrix), 2])
    arrow_head <- get_arrowhead(from_point_arrow, to_point_arrow)
    
    leaflet() %>% 
      addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                           attribution = paste(
                             "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                             "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
                           )
    ) %>%
      setView(lng = polyline_matrix[floor(nrow(polyline_matrix)/2),1], 
              lat = polyline_matrix[floor(nrow(polyline_matrix)/2),2], 
              zoom = 16) %>%
      addPolylines(lng = polyline_matrix[,1], lat = polyline_matrix[,2], color = line_color, opacity = 1) %>%
      addPolylines(lng = arrow_head[,"x"], lat = arrow_head[,"y"], color = marker_color, opacity = 1) # arrow head
  })
  
  # calculo de outliers
  tt_dt_w_outliers <- reactive(get_outliers(tt_dt, 
                                            input$time_grouper, 
                                            day_type_grouper = input$day_type_grouper))
  
  # plot of raw data with marked outliers
  output$travel_time_plot <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    p <- plot_ly() 
    p <- add_trace(p, x = tt_dt_out$updatetime[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1)],
                y = tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1)], 
                name = 'travel time', 
                mode = 'lines+markers',
                type = 'scatter',
                line = list(color = line_color, width = 2),
                marker = list(color = line_color, size = 4))
    if (sum(tt_dt_out$outlier[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1)]) > 0) {
      p <- add_trace(p, x = tt_dt_out$updatetime[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)],
                y = tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)],
                hovertext = paste("Value :", tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)],
                                  "<br> N :", tt_dt_out$count[which(tt_dt_out$name == input$route & tt_dt_out$date == input$date1 & tt_dt_out$outlier == 1)]),
                name = 'outlier',
                mode = 'markers',
                type = 'scatter',
                marker = list(color = marker_color)
      )
    }
    p %>%
      layout(title = paste('Datos crudos día', input$date1)) %>%
      layout(legend = list(x = 0.05, y = 0.95))  %>% 
      layout(plot_bgcolor = bg_color) %>% 
      layout(paper_bgcolor = container_color) %>% 
      layout(margin = m)
    })
  
  # plot of boxplots for every time interval
  output$outliers_boxplots <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    
    if (input$day_type_grouper == 'day_type') {
      dia_texto <- get_day_type(lubridate::wday(input$date1, week_start = 1))
    }
    else {
      dia_texto <- lubridate::wday(input$date1, label = TRUE, abbr = FALSE)
    }
    
    plot_ly(y = tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$weekday == wday(input$date1, week_start = 1))], 
          color = tt_dt_out$floor_time[which(tt_dt_out$name == input$route & tt_dt_out$weekday == wday(input$date1, week_start = 1))], 
          type = "box", 
          text = paste("Value :", tt_dt_out$delay[which(tt_dt_out$name == input$route & tt_dt_out$weekday == wday(input$date1, week_start = 1))],
                       "<br> N :", tt_dt_out$count[which(tt_dt_out$name == input$route & tt_dt_out$weekday == wday(input$date1, week_start = 1))],
                       "<br> day :", tt_dt_out$floor_date[which(tt_dt_out$name == input$route & tt_dt_out$weekday == wday(input$date1, week_start = 1))]),
          boxpoints = "all", 
          jitter = 0.3,
          pointpos = 0
          ) %>% 
    layout(title = paste('Boxplot de tiempos de viaje para los días', dia_texto)) %>% 
    layout(plot_bgcolor = bg_color) %>% 
    layout(paper_bgcolor = container_color) %>% 
    layout(margin = m)
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
                marker = list(color = line_color, size = 4)) %>%
      layout(title = paste('Datos agregados cada', input$time_grouper,'minutos para el día', input$date1, '(con outliers)')) %>%
      layout(plot_bgcolor = bg_color) %>% 
      layout(paper_bgcolor = container_color) %>% 
      layout(margin = m)
  })
  
  # plot of agreggated data w/o outliers
  tt_dt_grouped_w_o_outliers <- reactive({
    tt_dt_out <- tt_dt_w_outliers()
    tt_dt_w_o_out <- tt_dt_out[which(tt_dt_out$outlier == 0),]
    tt_dt_w_o_out %>%
      group_by(name, date, updatetime = floor_date(tt_dt_w_o_out$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
      summarise(stdv = sd(delay), delay = mean(delay))
    })
  
  output$travel_time_agg_w_o_outliers_plot <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    plot_ly() %>%
      add_trace(x = tt_dt_grouped_w_o_outliers$updatetime[which(tt_dt_grouped_w_o_outliers$name == input$route & tt_dt_grouped_w_o_outliers$date == input$date1)], 
                y = tt_dt_grouped_w_o_outliers$delay[which(tt_dt_grouped_w_o_outliers$name == input$route & tt_dt_grouped_w_o_outliers$date == input$date1)],
                name = 'travel time agreggated',
                type = 'scatter', 
                mode = 'lines+markers', 
                line = list(color = line_color, width = 2),
                marker = list(color = line_color, size = 4),
                error_y = list(array = tt_dt_grouped_w_o_outliers$stdv[which(tt_dt_grouped_w_o_outliers$name == input$route & tt_dt_grouped_w_o_outliers$date == input$date1)], 
                               color = '#B1F2EC',
                               opacity = 0.5)
                ) %>% 
      layout(title = paste('Datos agregados cada', input$time_grouper,'minutos para el día', input$date1, '(sin outliers)')) %>%
      layout(plot_bgcolor = bg_color) %>% 
      layout(paper_bgcolor = container_color) %>% 
      layout(margin = m)
  })
  
  output$travel_time_heatmap <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    heatmap_generator(tt_dt_grouped_w_o_outliers,input$route,input$time_grouper,bg_color,container_color, m) 
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# runApp('dashboard-cars', host = "0.0.0.0", port = 8080)