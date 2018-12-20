rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","paths.R"))
source(file.path(rproj_dir,"utils","data_processing.R"))
source(file.path(rproj_dir,"utils","outliers.R"))

#----- packages -----#
getLibrary("shiny")
getLibrary("shinythemes")
getLibrary("data.table")
getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")

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
    sidebarPanel(
      selectInput('route', label = 'Pick a route: ', choices = routes_options),
      dateInput('date1', label = 'Pick a date: ', value = as.Date("2018-11-01")),
      sliderInput('time_grouper', label = 'Pick a time interval: ', min = 5, max = 60, value = 15, step = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("travel_time_plot"),
      plotlyOutput("travel_time_agg_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    plot_ly(x = tt_dt_grouped$updatetime[which(tt_dt_grouped$name == input$route & tt_dt_grouped$date == input$date1)], 
            y = tt_dt_grouped$delay[which(tt_dt_grouped$name == input$route & tt_dt_grouped$date == input$date1)],
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

