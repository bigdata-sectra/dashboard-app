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
source(file.path(rproj_dir,"custom-plots","boxplot_trace.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times_w_o_outliers.R"))
source(file.path(rproj_dir,"custom-plots","heatmap.R"))
source(file.path(rproj_dir,"custom-plots","heatmap_w_o_outliers.R"))

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
routes_names <- sort(unique(tt_dt$name))

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Tiempos de viaje"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sobre nosotros", icon = icon("th"), tabName = "about-us")
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  selectInput('route', label = 'Ruta: ', choices = routes_names, selected = routes_names[length(routes_names)]),   
                  dateInput('date1', label = 'Fecha: ', value = as.Date("2018-11-05")),
                  sliderInput('time_grouper', label = 'Selecciona un intervalo para agrupar los valores: ', min = 5, max = 60, value = 30, step = 5),  
                  selectizeInput('day_type_grouper', label = "Selecciona la manera en que se agrupan los valores: ", choices = c('Tipo de día [L/S/D]' = 'day_type', 'Día de la semana' = 'weekday'))
                ),
                box(
                  title = "Mapa", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  withSpinner(leafletOutput("routes_map"))
                )
              ),
              fluidRow(
                tabBox(
                  title = "Datos por día", width = 12,
                  tabPanel("Agrupados (c/o)", withSpinner(plotlyOutput("travel_time_agg_plot"))),
                  tabPanel("Agrupados (s/o)", withSpinner(plotlyOutput("travel_time_agg_w_o_outliers_plot"))),
                  tabPanel("Crudos", withSpinner(plotlyOutput("travel_time_plot")))
                )
              ),
              fluidRow(
                tabBox(
                  title = "",
                  tabPanel("Analisis día", withSpinner(plotlyOutput("boxplots_trace"))),
                  tabPanel("Análisis outliers", withSpinner(plotlyOutput("outliers_boxplots"))),
                  tabPanel("info", "")
                ),
                tabBox(
                  title = "",
                  tabPanel("Heatmap (c/o)", withSpinner(plotlyOutput("travel_time_heatmap"))),
                  tabPanel("Heatmap (s/o)", withSpinner(plotlyOutput("travel_time_heatmap_w_o_outliers"))),
                  tabPanel("info", "")
                )
              ) 
      ),
      tabItem(tabName = "about-us",
              h2("Desarrollado por el equipo de Big Data en Transporte de Sectra, MTT.")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # plot the map of a route
  output$routes_map <- renderLeaflet({
    route_map(r_dt,input$route)
  })
  
  # filtering by route name to speed things up
  tt_dt_f <- reactive({
    tt_dt[which(tt_dt$name == input$route),]
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
    tt_dt_out <- tt_dt_out[which(tt_dt_out$date == input$date1),]
    
    validate(
      need(nrow(tt_dt_out)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    raw_travel_times(tt_dt_out, input$route, input$date1)
    })
  
  # plot of boxplots for every time interval
  output$outliers_boxplots <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    
    validate(
      need(nrow(tt_dt_out)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    boxplot(tt_dt_out, input$day_type_grouper, input$route, input$date1)
    })
  
  # calculate data frame grouped
  tt_dt_grouped <- reactive({
    tt_dt_w_outliers <- tt_dt_w_outliers()
    tt_dt_w_outliers %>%
      group_by(name, date, updatetime = floor_date(tt_dt_w_outliers$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
      summarise(delay = mean(delay), out_sum = sum(outlier))
    })
  
  # boxplots with trace of current date
  output$boxplots_trace <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    
    validate(
      need(nrow(tt_dt_out)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    tt_dt_grouped <- tt_dt_grouped()
    tt_dt_grouped <- tt_dt_grouped[which(tt_dt_grouped$date == input$date1),]
    
    tt_dt_grouped$floor_hour <- ifelse(hour(tt_dt_grouped$updatetime) < 10, paste('0', hour(tt_dt_grouped$updatetime), sep=''), hour(tt_dt_grouped$updatetime))
    tt_dt_grouped$floor_minute <- ifelse(minute(tt_dt_grouped$updatetime) < 10, paste('0', minute(tt_dt_grouped$updatetime), sep=''), minute(tt_dt_grouped$updatetime))
    tt_dt_grouped$floor_time <- paste(tt_dt_grouped$floor_hour, tt_dt_grouped$floor_minute, sep=":")
    
    validate(
      need(nrow(tt_dt_grouped)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    boxplot_trace(tt_dt_out, tt_dt_grouped, input$day_type_grouper, input$route, input$date1)
  })
  
  
  # plot of agreggated data
  output$travel_time_agg_plot <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    tt_dt_grouped <- tt_dt_grouped[which(tt_dt_grouped$date == input$date1),]
    
    validate(
      need(nrow(tt_dt_grouped)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
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
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers[which(tt_dt_grouped_w_o_outliers$date == input$date1),]
    
    validate(
      need(nrow(tt_dt_grouped_w_o_outliers)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    agg_travel_times_w_o_outliers(tt_dt_grouped_w_o_outliers, input$route, input$date1, input$time_grouper)
  })
  
  # plot heatmap w/ outliers
  output$travel_time_heatmap <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    heatmap_w_outliers(tt_dt_grouped,input$route,input$time_grouper, input$date1) 
  })
  
  # plot heatmap w/o outliers
  output$travel_time_heatmap_w_o_outliers <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    heatmap_w_o_outliers(tt_dt_grouped_w_o_outliers,input$route,input$time_grouper, input$date1) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# runApp('dashboard-cars', host = "0.0.0.0", port = 8080)
