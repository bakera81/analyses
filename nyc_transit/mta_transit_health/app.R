
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

# Populate and style radio buttons
radio_choice_names <- get_radio_choice_names(all_routes)
radio_choice_values <- all_routes$route_id

# Define UI for application that draws a histogram
ui <- page_sidebar(

  # Application title
  # titlePanel("MTA Subway Alert Status"),
  title = "MTA Subway Alert Status",
  
  # Route selector via sidebar
  sidebar = sidebar(
    radioButtons(
      "radio", 
      "Select a route:",
      choiceNames = radio_choice_names,
      choiceValues = radio_choice_values)
  ),
  card(
    plotlyOutput("past_alert_freq")
  ),
  card(
    plotlyOutput("past_alert_vol")
  ),
  card(
    plotlyOutput("past_alert_duration")
  ),
  card(
    uiOutput("selected_route_alerts")
  )
)

server <- function(input, output) {
  
  selected_route_data <- reactive({
    alert_durations %>%
      # Based on the current time, determine the service (weekday / weeknight)
      mutate_service(now(), "current_service") %>%
      filter(
        affected == input$radio,
        service == current_service,
        start_time >= today() - 365) 
  })
  
  selected_rt_alerts <- reactive({
    rt_alerts %>%
      mutate_service(now(), "current_service") %>%
      filter(
        route_id == input$radio,
        service == current_service)
  })
  
  output$selected_route_alerts <- renderUI({
    # selected_rt_alerts() %>%
      accordion_list <- rt_alerts %>%
      mutate_service(now(), "current_service") %>%
      filter(
        route_id == input$radio,
        service == current_service) %>%
      rename(
        header = header__en,
        description = description__en) %>%
    pmap(alert_accordion_ui)
      
      accordion(
        accordion_list,
        open = F,
        multiple = T
      )
  })
  
  # Render content based on selected tab
  output$selected_route_content <- renderUI({
    # Get the selected tab
    selected_route <- input$nav_tabs_2
    
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
      selected_route_data() %>%
      # alert_durations %>%
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
