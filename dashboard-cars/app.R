# get the path of the project file in order to normalize sources
rproj_dir <- rprojroot::find_rstudio_root_file()

# source parameters and functions
source(file.path(rproj_dir,"utils","paths.R"))
source(file.path(rproj_dir,"utils","getLibrary.R"))

source(file.path(rproj_dir,"custom-functions","data_processing.R"))
source(file.path(rproj_dir,"custom-functions","outliers.R"))

source(file.path(rproj_dir,"custom-plots","route_map.R"))
source(file.path(rproj_dir,"custom-plots","raw_travel_times.R"))
source(file.path(rproj_dir,"custom-plots","boxplot.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times_w_o_outliers.R"))
source(file.path(rproj_dir,"custom-plots","heatmap.R"))

#----- packages -----#
getLibrary("shiny")
getLibrary("data.table")
getLibrary("plotly")
getLibrary("lubridate")
getLibrary("dplyr")
getLibrary("leaflet")
getLibrary("shinycssloaders")
getLibrary("shinydashboard")

#----- data -----#
tt_dt <- travel_times_processing()
r_dt <- routes_processing()

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Car Travel Times"),
  dashboardSidebar(
    #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  ),
  dashboardBody(
    
    fluidRow(
      
      box(
        title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        selectInput('route', label = 'Pick a route: ', choices = sort(r_dt$name)),   
        dateInput('date1', label = 'Pick a date: ', value = as.Date("2018-11-01")),
        sliderInput('time_grouper', label = 'Pick a time interval: ', min = 5, max = 60, value = 15, step = 5),  
        selectInput('day_type_grouper', label = "Select a day type grouper: ", choices = c('weekday', 'day_type'))
        # ,
        # selectInput('display_unit', label = "Select a display unit: ", choices = c('s/km', 'km/h')
      ),
      box(
        title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        withSpinner(leafletOutput("routes_map"))
      )
      
    ),
    
    fluidRow(
      tabBox(
        title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        tabPanel("Tab1", withSpinner(plotlyOutput("travel_time_plot"))),
        tabPanel("Tab2", withSpinner(plotlyOutput("travel_time_agg_plot"))),
        tabPanel("Tab3", withSpinner(plotlyOutput("travel_time_agg_w_o_outliers_plot")))
      ),
      tabBox(
        selected = "Tab1",
        tabPanel("Tab1", withSpinner(plotlyOutput("outliers_boxplots"))),
        tabPanel("Tab2", withSpinner(plotlyOutput("travel_time_heatmap")))
      )
    ),
    fluidRow(textOutput("checking_datatables"))   
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # plot the map of a route
  output$routes_map <- renderLeaflet({
    route_map(r_dt,input$route)
  })
  
  tt_dt_f <- reactive({
    tt_dt[which(tt_dt$name == input$route & tt_dt$date == input$date1),]
  })
  
  # calculo de outliers
  tt_dt_w_outliers <- reactive({
    get_outliers(tt_dt_f(), 
                 input$time_grouper, 
                 day_type_grouper = input$day_type_grouper)
    })
  
  # plot of raw data with marked outliers
  output$travel_time_plot <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    
    validate(
      need(nrow(tt_dt_out)>0, "NO DATA AVAILABLE FOR THIS DAY!")
    )
    
    raw_travel_times(tt_dt_out, input$route, input$date1)
    
    })
  
  # plot of boxplots for every time interval
  output$outliers_boxplots <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    boxplot(tt_dt_out, input$day_type_grouper, input$route, input$date1)
    })
  
  # calculate data frame grouped
  tt_dt_grouped <- reactive({
    tt_dt_f <- tt_dt_f()
    tt_dt_f %>%
      group_by(name, date, updatetime = floor_date(tt_dt_f$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
      summarise(delay = mean(delay))
    })
  
  # plot of agreggated data
  output$travel_time_agg_plot <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    
    validate(
      need(nrow(tt_dt_grouped)>0, "NO DATA AVAILABLE FOR THIS DAY!")
    )
    
    agg_travel_times(tt_dt_grouped, input$route, input$date1, input$time_grouper)
  })
  
  # calculate data frame grouped w/o outliers
  tt_dt_grouped_w_o_outliers <- reactive({
    tt_dt_out <- tt_dt_w_outliers()
    tt_dt_w_o_out <- tt_dt_out[which(tt_dt_out$outlier == 0),]
    tt_dt_w_o_out %>%
      group_by(name, date, updatetime = floor_date(tt_dt_w_o_out$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
      summarise(stdv = sd(delay), delay = mean(delay))
    })
  
  # plot of agreggated data w/o outliers
  output$travel_time_agg_w_o_outliers_plot <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    
    validate(
      need(nrow(tt_dt_grouped_w_o_outliers)>0, "NO DATA AVAILABLE FOR THIS DAY!")
    )
    
    agg_travel_times_w_o_outliers(tt_dt_grouped_w_o_outliers, input$route, input$date1, input$time_grouper)
  })
  
  # plot heatmap
  output$travel_time_heatmap <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    heatmap_generator(tt_dt_grouped_w_o_outliers,input$route,input$time_grouper) 
  })
  
  output$checking_datatables <- renderText({
    print(nrow(tt_dt_w_outliers()))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# runApp('dashboard-cars', host = "0.0.0.0", port = 8080)
