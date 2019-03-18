# get the path of the project file in order to normalize sources
rproj_dir <- getwd()

# source parameters and functions
source(file.path(rproj_dir,"utils","paths.R"))

source(file.path(rproj_dir,"custom-functions","data_processing.R"))
source(file.path(rproj_dir,"custom-functions","outliers.R"))
source(file.path(rproj_dir,"custom-functions","info.R"))

source(file.path(rproj_dir,"custom-plots","route_map.R"))
source(file.path(rproj_dir,"custom-plots","raw_travel_times.R"))
source(file.path(rproj_dir,"custom-plots","boxplot.R"))
source(file.path(rproj_dir,"custom-plots","boxplot_trace.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times.R"))
source(file.path(rproj_dir,"custom-plots","agg_travel_times_w_o_outliers.R"))
source(file.path(rproj_dir,"custom-plots","heatmap.R"))
source(file.path(rproj_dir,"custom-plots","heatmap_w_o_outliers.R"))

#----- packages -----#
library("shiny")
library("data.table")
library("plotly")
library("lubridate")
library("dplyr")
library("leaflet")
library("shinycssloaders")
library("shinydashboard")

#----- data -----#
tt_dt <- travel_times_processing()
r_dt <- routes_processing()
routes_names <- sort(unique(tt_dt$name))

dict_dt <- dict_loading()


#----- initial parameters -----#
project_options <- c('mapa congestion jgibson','mapa congestion ITI', 'mapa congestion jgibson v2')
initial_project <- project_options[length(project_options)]

main_streets <- sort(unique(dict_dt$main_street[which(dict_dt$project == initial_project)]))
initial_main_street <- main_streets[length(main_streets)]
initial_sense <- unique(dict_dt$sense[which(dict_dt$project == initial_project & dict_dt$main_street == initial_main_street)])[1]

initial_from <- dict_dt$from_intersection[which(dict_dt$project == initial_project 
                                                & dict_dt$main_street == initial_main_street 
                                                & dict_dt$sense == initial_sense)][1]
initial_to <- dict_dt$to_intersection[which(dict_dt$project == initial_project 
                                            & dict_dt$main_street == initial_main_street 
                                            & dict_dt$sense == initial_sense)][1]
initial_route <- dict_dt$name[which(dict_dt$project == initial_project 
                                    & dict_dt$main_street == initial_main_street 
                                    & dict_dt$sense == initial_sense 
                                    & dict_dt$from_intersection == initial_from 
                                    & dict_dt$to_intersection == initial_to)]


min_date <- min(tt_dt$date[which(tt_dt$name == initial_route)])
max_date <- max(tt_dt$date[which(tt_dt$name == initial_route)])
start_date <- min_date
end_date <- if(start_date + 30 < max_date){start_date + 30}else{max_date}
default_date <- if(min_date + 1 < max_date ){min_date + 1}else{max_date}

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
                  title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  selectInput('project', label = 'Tramificación: ',
                              choices = project_options,
                              selected = initial_project),
                  uiOutput('main_street_sections'),
                  uiOutput('direction'),
                  uiOutput('from_to'),
                  dateRangeInput('date_range', label = 'Seleccione un rango de fechas a analizar:',
                                 min = min_date,
                                 max = max_date,
                                 start = start_date,
                                 end = end_date),
                  sliderInput('time_grouper', 
                              label = 'Selecciona un intervalo para agrupar los valores: ', 
                              min = 5, max = 60, value = 30, step = 5),  
                  selectizeInput('day_type_grouper', label = "Selecciona la manera en que se agrupan los valores: ", 
                                 choices = c('Tipo de día [L/S/D]' = 'day_type', 'Día de la semana' = 'weekday'))
                )
              ),
              fluidRow(
                box(
                  title = "Mapa", status = "primary",
                  withSpinner(leafletOutput("routes_map"))
                ),
                tabBox(
                  title = "",
                  tabPanel("Heatmap (c/o)", withSpinner(plotlyOutput("travel_time_heatmap"))),
                  tabPanel("Heatmap (s/o)", withSpinner(plotlyOutput("travel_time_heatmap_w_o_outliers"))),
                  tabPanel("info", heatmap_info)
                )
              ),
              fluidRow(
                box(
                  title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  dateInput('date1', label = 'Fecha: ',
                            min = min_date,
                            max = max_date,
                            value = default_date)
                )
              ),
              fluidRow(
                tabBox(
                  title = "Datos por día",
                  tabPanel("Agrupados (c/o)", withSpinner(plotlyOutput("travel_time_agg_plot"))),
                  tabPanel("Agrupados (s/o)", withSpinner(plotlyOutput("travel_time_agg_w_o_outliers_plot"))),
                  tabPanel("Crudos", withSpinner(plotlyOutput("travel_time_plot")))
                ),
                tabBox(
                  title = "",
                  tabPanel("Analisis día", withSpinner(plotlyOutput("boxplots_trace"))),
                  tabPanel("Análisis outliers", withSpinner(plotlyOutput("outliers_boxplots"))),
                  tabPanel("info", boxplot_info)
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
server <- function(input, output, session) {
  
  observe({
    sections <- unique(dict_dt$main_street[which(dict_dt$project==input$project)])
    selected_main_street <- sections[length(sections)]
    output$main_street_sections <- renderUI({
      selectInput('main_street', 'Eje: ', choices = sections, selected = selected_main_street)
    })
  })
  
  observe({  
    senses <- unique(dict_dt$sense[which(dict_dt$project==input$project & dict_dt$main_street==input$main_street)])
    selected_sense <- senses[length(senses)]
    output$direction<-renderUI({
      selectInput('sense', 'Sentido: ', choices = senses, selected = selected_sense)
    })
  })  

  observe({
    from_int <- dict_dt$from_intersection[which(dict_dt$project==input$project & dict_dt$main_street==input$main_street & dict_dt$sense==input$sense)]
    to_int <- dict_dt$to_intersection[which(dict_dt$project==input$project & dict_dt$main_street==input$main_street & dict_dt$sense==input$sense)]
    pairs_of_int <- paste(from_int, '-', to_int, sep='')
    
    selected_from <- from_int[length(from_int)]
    selected_to <- to_int[length(to_int)]
    
    selected_pair <- paste(selected_from, '-', selected_to, sep='')
    output$from_to<-renderUI({
      selectInput('pairs', 'Tramo: ', choices = pairs_of_int, selected = selected_pair)
    })
  })

  input_route <- reactive({
    from_int <- unlist(strsplit(as.character(input$pairs), "-"))[1]
    to_int <- unlist(strsplit(as.character(input$pairs), "-"))[2]
    dict_dt$name[which(dict_dt$project==input$project & dict_dt$main_street==input$main_street & dict_dt$sense==input$sense & 
                         dict_dt$from_intersection==from_int & dict_dt$to_intersection==to_int)]
  })

  # create the list of dates to update inputs given the route name
  dates_list <- reactive({
    if(!identical(character(0), input_route())){    
      updated_min_date <- min(tt_dt$date[which(tt_dt$name == input_route())])
      updated_max_date <- max(tt_dt$date[which(tt_dt$name == input_route())])
      updated_start_date <- updated_min_date
      updated_end_date <- if(updated_start_date + 30 < updated_max_date){updated_start_date + 30}else{updated_max_date}
      c(updated_min_date,updated_max_date,updated_start_date,updated_end_date)
      }
  })
  
  # update range input
  observe({
    updateDateRangeInput(session, "date_range",
                         label = 'Seleccione un rango de fechas a analizar:',
                         min = dates_list()[1],
                         max = dates_list()[2],
                         start = dates_list()[3],
                         end = dates_list()[4]
    )
  })
  
  # create updated defaul date
  updated_default_date <- reactive({
    if(min(input$date_range) + 1 < max(input$date_range)){min(input$date_range) + 1}else{max(input$date_range)}
  })
  
  # update date input
  observe({
    updateDateInput(session, "date1",
                    label = 'Fecha: ',
                    min = min(input$date_range),
                    max = max(input$date_range),
                    value = updated_default_date()
    )
  })
  
  #----- reactives dataframes -----#
  
  # filtering by route name to speed things up
  tt_dt_f <- reactive({
    tt_dt[which(tt_dt$name == input_route() & tt_dt$date >= min(input$date_range) & tt_dt$date <= max(input$date_range)),]
  })
    
  # calculo de outliers
  tt_dt_w_outliers <- reactive({
    get_outliers(tt_dt_f(), 
                 input$time_grouper, 
                 day_type_grouper = input$day_type_grouper)
    })
  
  # calculate data frame grouped
  tt_dt_grouped <- reactive({
    tt_dt_w_outliers <- tt_dt_w_outliers()
    tt_dt_w_outliers %>%
      group_by(name, date, updatetime = floor_date(tt_dt_w_outliers$updatetime, paste(as.character(input$time_grouper), " mins"))) %>%
      summarise(delay = mean(delay), out_sum = sum(outlier))
  })
  
  #----- plots -----#
  
  # plot the map of a route
  output$routes_map <- renderLeaflet({
    if(!identical(character(0), input_route())){
      route_map(r_dt,as.character(input_route()))
    }

  })
  
  # plot of raw data with marked outliers
  output$travel_time_plot <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    tt_dt_out <- tt_dt_out[which(tt_dt_out$date == input$date1),]
    
    validate(
      need(nrow(tt_dt_out)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    raw_travel_times(tt_dt_out, input_route(), input$date1)
    })
  
  # plot of boxplots for every time interval
  output$outliers_boxplots <- renderPlotly({
    tt_dt_out <- tt_dt_w_outliers()
    
    validate(
      need(nrow(tt_dt_out)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    boxplot(tt_dt_out, input$day_type_grouper, input_route(), input$date1)
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
    
    boxplot_trace(tt_dt_out, tt_dt_grouped, input$day_type_grouper, input_route(), input$date1)
  })
  
  
  # plot of agreggated data
  output$travel_time_agg_plot <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    tt_dt_grouped <- tt_dt_grouped[which(tt_dt_grouped$date == input$date1),]
    
    validate(
      need(nrow(tt_dt_grouped)>0, "NO EXISTEN DATOS DISPONIBLES PARA LA SELECCIÓN!")
    )
    
    agg_travel_times(tt_dt_grouped, input_route(), input$date1, input$time_grouper)
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
    
    agg_travel_times_w_o_outliers(tt_dt_grouped_w_o_outliers, input_route(), input$date1, input$time_grouper)
  })
  
  # plot heatmap w/ outliers
  output$travel_time_heatmap <- renderPlotly({
    tt_dt_grouped <- tt_dt_grouped()
    heatmap_w_outliers(tt_dt_grouped,input_route(),input$time_grouper, input$date1) 
  })
  
  # plot heatmap w/o outliers
  output$travel_time_heatmap_w_o_outliers <- renderPlotly({
    tt_dt_grouped_w_o_outliers <- tt_dt_grouped_w_o_outliers()
    heatmap_w_o_outliers(tt_dt_grouped_w_o_outliers,input_route(),input$time_grouper, input$date1) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# runApp('dashboard-cars', host = "0.0.0.0", port = 8080)
