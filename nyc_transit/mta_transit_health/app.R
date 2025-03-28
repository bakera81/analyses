
library(shiny)
library(bslib)
library(plotly)
library(scales)
library(ggtext)

# Load helper functions
source("utils.R")

# Set default ggplot theme
theme_set(theme_light())

# Load historical (static) data
alert_durations <- get_historical_alert_data()

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
    tags$hr(),
    selectInput(
      inputId = "period",
      label = "Compare to:",
      choices = c(
         "Past 30 days" = 30, "Past year" = 365),
      selected = 30,
      multiple = F
    )
  ), 
  
  # Main content
  uiOutput("route_title"),
  card(
    plotlyOutput("past_alert_vol", height = 400),
    height = 450,
    min_height = 450
  ),
  card(
    plotlyOutput("past_alert_freq", height = 400),
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
  
  # Load all current realtime data
  rt_alerts <- reactive({
    rt_alerts_data <- get_gtfs_rt_alerts() %>%
      left_join(all_routes , by = "route_id")
      
    updated_at <- rt_alerts_data %>%
      summarise(
        max_date = format(
          max(date, na.rm = T),
          "%Y-%m-%d %I:%M %p")) %>%
      pull(max_date)
    
    return(list(
      data = rt_alerts, 
      updated_at = updated_at))
  })
  
  # Load only realtime alerts for selected route
  selected_rt_alerts <- reactive({
    
    data <- get_gtfs_rt_alerts() %>%
      left_join(all_routes , by = "route_id") %>%
      mutate_service(now(), "current_service") %>%
      filter(
        route_id == input$radio,
        service == current_service)
    
    updated_at <- data %>%
      summarise(
        max_date = format(
          max(date, na.rm = T),
          "%Y-%m-%d %I:%M %p")) %>%
      pull(max_date)
    
    list(
      data = data,
      updated_at = updated_at)
  })
    
  # Get historical data for selected route and period
  selected_route_day_vol_data <- reactive({
    
    # Set window size
    window_size <- 7
    
    # TODO: is my GTFS RT data based on alerts or events?
    alert_durations %>%
      # Based on the current time, determine the service (weekday / weeknight)
      mutate_service(now(), "current_service") %>%
      filter(
        affected == input$radio,
        service == current_service,
        start_time >= "2022-01-01",
        yday(start_time) >= yday(today()) - window_size,
        yday(start_time) <= yday(today()) + window_size) %>%
      get_historical_active_alert_days() %>% 
      mutate(
        year = year(date),
        day = format(date, "%b %d"),
        x_breaks_col = ymd(format(date, "9999-%m-%d"))) 
  })
  
  
  output$route_title <- renderUI({
    data <- selected_rt_alerts()
    
    n_alerts <- 
      data$data %>%
      count() %>%
      pull(n)
    
    if (n_alerts == 0) {
      n_alerts <- "no"
    }
      
    current_service <-
      tibble(now = now()) %>%
      mutate_service(now) %>%
      pull(service)
    
    route_icon <- all_routes %>%
      filter(route_id == input$radio) %>%
      get_radio_choice_names()
    tagList(
      titlePanel(
        tagList(
          route_icon, 
          paste("How unusual is", input$radio, "train service?")
      )),
      card(
        tags$p(list(
          "There are currently ", tags$b(paste(n_alerts, "active alerts")), 
           "for", current_service, input$radio, "trains.")),
        tags$p(
          tags$i("Last updated", data$updated_at)
        ),  
        min_height = 100
      )
    )
  })
  
  
  output$past_alert_vol <- renderPlotly({
    
    # Load the reactive data
    data <- selected_route_day_vol_data() 
    
    # Get current alert volume
    n_alerts <- 
      selected_rt_alerts()$data %>%
      count() %>%
      pull(n)
    
    # Create labels and breaks so we can bold today's date
    x_labels <- unique(data$day)
    # TODO: ggtext is not rendering bolds via plotly
    # x_labels[window_size - 1] <- paste0("**", format(today(), "%b %d"), "**")
    x_labels[window_size - 1] <- paste0("Today - ", format(today(), "%b %d"))
    x_breaks <- data %>%
      distinct(x_breaks_col = ymd(format(date, "9999-%m-%d"))) %>%
      pull(x_breaks_col)
    
    p <- data %>%
      # Use a funky date col to keep the X axis ordered correctly
      ggplot(aes(x_breaks_col, n_events, fill = factor(year))) +
      geom_col(
        position = position_dodge(preserve = "single")) +
      geom_hline(
        aes(yintercept = n_alerts, linetype = "Current status"),
        color = "red"
      ) + 
      scale_linetype_manual(
        name = "",
        values = c("Current status" = "dashed")
      ) +
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels) +
      scale_y_continuous(breaks = pretty_breaks()) + 
      scale_fill_brewer() + 
      theme(
        axis.text.x = element_markdown(angle = 45, hjust = 1)) +
      labs(
        title = "Past daily active alerts",
        subtitle = paste(
          "Active events per day for", 
          get_current_service(), input$radio, "trains"),
        x = "", 
        y = "Active events",
        fill = "Year"
      )
    
    plt <- ggplotly(p)
    
    # Fix the legend text
    for (i in seq_along(plt$x$data)) {
      if (!is.null(plt$x$data[[i]]$name)) {
        # Remove parentheses and ,1 from legend items
        plt$x$data[[i]]$name <- gsub("\\(|\\)|,1", "", plt$x$data[[i]]$name)
      }
    }
    
    plt
    
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


  output$selected_route_alerts <- renderUI({
    
    accordion_list <- selected_rt_alerts()$data %>%
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

}

# Run the application 
shinyApp(ui = ui, server = server)
