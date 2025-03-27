
library(shiny)
library(bslib)
library(plotly)

# Load helper functions
source("utils.R")

# Load historical (static) data
alert_durations <- get_historical_alert_data()

# Load current realtime data
rt_alerts <- get_gtfs_rt_alerts()
updated_at <- rt_alerts %>%
  summarise(max_date = max(date, na.rm = T)) %>%
  pull(max_date)

# Identify the routes to display
all_routes <- get_all_routes(alert_durations)

# Populate and style radio buttons
radio_choice_names <- get_radio_choice_names(all_routes)
radio_choice_values <- all_routes$route_id

# Define UI for application that draws a histogram
ui <- page_sidebar(

  # Application title
  # titlePanel("MTA Subway Alert Status"),
  title = "Realtime MTA Subway Alert Explorer",
  
  # Sidebar
  sidebar = sidebar(
    tags$div(
      style = "margin-bottom: 15px;",
      tags$label("Select a route:", `for` = "radio")
    ),
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 8px;",
      radioButtons(
        "radio", 
        label = NULL,
        choiceNames = radio_choice_names,
        choiceValues = radio_choice_values,
        inline = TRUE
      )
    ),
    radio_ui_style(),
  ), 
  
  # Main content
  uiOutput("route_title"),
  tags$p(
    tags$i("Last updated", updated_at)
  ),
  card(
    plotlyOutput("past_alert_freq", height = 400),
    height = 450,
    min_height = 450
  ),
  card(
    plotlyOutput("past_alert_vol", height = 400),
    height = 450,
    min_height = 450
  ),
  card(
    plotlyOutput("past_alert_duration", height = 400),
    height = 450,
    min_height = 450
  ),
  card(
    uiOutput("selected_route_alerts"),
    min_height = 450
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
  
  output$route_title <- renderUI({
    route_icon <- all_routes %>%
      filter(route_id == input$radio) %>%
      get_radio_choice_names()
    
    titlePanel(
      tagList(
        route_icon, 
        paste("How unusual is current", input$radio, "train service?")
    ))
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
  
  output$past_alert_duration <- renderPlotly({
    p <- 
      selected_route_data() %>%
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
    
    ggplotly(p, height = 400) 
  })
  
  
  output$past_alert_vol <- renderPlotly({
    
    p <- 
      selected_route_data() %>% # Using the reactive dataset from above
      count(date = date(start_time)) %>%
      ggplot(aes(date, n)) +
      geom_col() +
      theme_light()
    
    ggplotly(p, height = 400)
    
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
    
    ggplotly(p, height = 400, dynamicTicks = TRUE) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
