rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","paths.R"))

source(file.path(rproj_dir,"data_processing.R"))

getLibrary("shiny")
getLibrary("shinythemes")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

