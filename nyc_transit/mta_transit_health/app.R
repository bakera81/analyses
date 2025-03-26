#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(bslib)
library(plotly)

# Load helper functions
source("utils.R")

# Load historical (static) data
alert_durations <- get_historical_alert_data()

# Load current realtime data
rt_alerts <- get_gtfs_rt_alerts()

# Identify the routes to display
all_routes <- get_all_routes(alert_durations)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("MTA Subway Alert Status"),
  
  # Dynamically added nav items for each subway route
  navbarPage(
    id = "nav_tabs",  
    title = "Routes",
    card(
      uiOutput("selected_route_content")
    )
  )
)

server <- function(input, output) {
  
  # On initialization, add tabs for each route
  observe({
    for (i in 1:nrow(all_routes)) {
      
      route_data <- all_routes[i, ]
      
      # Create tab label with subway_ui
      tab_label <- subway_ui(
        route_id = route_data$route_id,
        route_grouping = route_data$route_grouping
      )
      
      # Create the tab content
      tab_content <- tabPanel(
        title = tab_label,
        value = route_data$route_id,
        h4(paste("Route:", route_data$route_id)),
        # TODO: show number of alerts, including zero
        # p(paste("Current status:", route_data$status)),
        plotOutput(paste0("plot_", route_data$route_id))
      )
      
      # Insert tab 
      nav_insert(
        id = "nav_tabs",        # ID of the navbarPage
        nav = tab_content,      # The tab panel to insert
        target = NULL,          # No specific target tab (add to end)
        position = "after",     # Add after the target (or at end if target is NULL)
        select = FALSE          # Don't automatically select this tab
      )
      
      # Create the plot output for this route
      local({
        route_id <- route_data$route_id
        output_id <- paste0("plot_", route_id)
        
        output[[output_id]] <- renderPlot({
          plot(1:10, main = paste("Data for Route", route_id))
        })
      })
    }
  })
  
  selected_route_data <- reactive({
    alert_durations %>%
      # Based on the current time, determine the service (weekday / weeknight)
      mutate_service(now(), "current_service") %>%
      filter(
        affected == input$nav_tabs,
        service == current_service,
        start_time >= today() - 365) 
  })
  
  # Render content based on selected tab
  output$selected_route_content <- renderUI({
    # Get the selected tab
    selected_route <- input$nav_tabs
    
    # If it's the overview tab, show nothing or overview content
    if(is.null(selected_route) || selected_route == "overview_tab") {
      return(NULL)
    }
    
    selected_rt_alerts <- rt_alerts %>%
      filter(route_id == selected_route) 
      
    
    # Format all alerts for current route
    all_alert_taglist <- selected_rt_alerts %>%
      pmap(function(header__en, description__en, ...) {
        div(
          class = "alert-item",
          style = "margin-bottom: 15px;",
          h4(header__en, style = "margin-bottom: 5px; color: #d9534f;"),
          p(description__en)
        )
      }) %>%
      tagList()
      
    
    # Return detailed content for this route
    tagList(
      h3(paste("All current alerts for", selected_route, "trains")),
      p(paste(
        "Last updated ", 
        selected_rt_alerts %>%
          summarise(updated_at = max(date, na.rm = T)) %>%
          pull(updated_at))),
      plotlyOutput(outputId = "past_alert_freq"),
      plotlyOutput("past_alert_vol"),
      plotlyOutput("past_alert_duration"),
      p(paste(
        "There are", 
        all_routes %>%
          filter(route_id == selected_route) %>%
          pull(active_alerts),
        "current alerts:")),
      all_alert_taglist
    )
  })
  
  
  output$past_alert_duration <- renderPlotly({
    p <- 
      # selected_route_data() %>%
      alert_durations %>%
      mutate(
        duration_mins = as.numeric(duration_est) / 60,
        duration_hrs = as.numeric(duration_est) / (60 * 60)) %>%
      ggplot(aes(duration_hrs)) + 
      stat_ecdf() +
      scale_x_log10(
        breaks = c(.25, .5, 1, 6, 24, 48, 72, 7 * 24, 4 * 7 * 24),
        labels = c("15 mins", "30 mins", "1 hour", "6 hours", "1 day", "2 days", "3 days", "1 week", "1 month")) + 
      coord_cartesian(xlim = c(0.01, 7 * 24)) + 
      theme_light() + 
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  output$past_alert_vol <- renderPlotly({
    
    p <- 
      selected_route_data() %>% # Using the reactive dataset from above
      count(date = date(start_time)) %>%
      ggplot(aes(date, n)) +
      geom_col() +
      theme_light()
    
    ggplotly(p)
    
  })
  
  output$past_alert_freq <- renderPlotly({
    # TODO: update this to be number of concurrent alerts
    # TODO: Update this to include vline and hline for current alerts
    
    p <- selected_route_data() %>%
      mutate(now = now()) %>%
      mutate_service(now(), "current_service") %>%
      count(date = date(start_time)) %>%
      ggplot(aes(n)) + 
      stat_ecdf() +
      theme_light()
    
    ggplotly(p, dynamicTicks = TRUE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
