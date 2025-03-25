#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(reticulate)
use_virtualenv("r-nyc_transit")

# Load helper functions
source("utils.R")

# Load historical (static) data
alert_durations <- get_historical_alert_data()

# Load current realtime data
rt_alerts <- get_gtfs_rt_alerts()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MTA Subway Alert Status"),
    

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
          uiOutput("overviewUI"),
          tableOutput("overviewTbl"),
          plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$overviewUI <- renderUI({
    
    all_route_statuses <- 
      alert_durations %>%
      distinct(route_id = affected) %>%
      enrich_routes(route_id) %>%
      left_join(
        rt_alerts, 
        by = "route_id") %>%
      group_by(route_id, route_grouping) %>%
      summarise(
        active_alerts = sum(!is.na(id)),
        headers = list(header__en)) %>%
      select(-headers)
    
    subway_style <- "
      display: inline-flex;
      justify-content: center;
      align-items: center;
      width: 40px;
      height: 40px;
      border-radius: 50%;
      background-color: #FF0000;
      color: white;
      font-size: 24px;
      font-weight: bold;
    "
    
    subway_ui <- function(route_id, route_grouping, ...) {
      subway_style <- paste0(
        "
        display: inline-flex;
        justify-content: center;
        align-items: center;
        width: 40px;
        height: 40px;
        border-radius: 50%;
        background-color:", route_grouping, ";
        color: white;
        font-size: 24px;
        font-weight: bold;
        ")
        
      tags$div(
        style = subway_style,
        route_id
      )
    }
    
    all_route_statuses %>%
      pmap(subway_ui)
    
  })
  
  output$overviewTbl <- renderTable({
    
    all_routes <- 
      alert_durations %>%
      distinct(route_id = affected) %>%
      enrich_routes(route_id)
    
    all_routes %>%
      left_join(
        rt_alerts, 
        by = "route_id") %>%
      group_by(route_id, route_grouping) %>%
      summarise(
        active_alerts = sum(!is.na(id)),
        headers = list(header__en)) %>%
      select(-headers)
  })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
