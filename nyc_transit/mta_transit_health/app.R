
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
alert_counts <- get_historical_alert_data()

# Identify the routes to display
all_routes <- get_all_routes(alert_counts)

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
    plotlyOutput("alert_vol_ecdf", height = 400),
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
    window_size <- 14
    
    selected_alert_counts <-
      alert_counts %>%
      # Based on the current time, determine the service (weekday / weeknight)
      mutate_service(now(), "current_service") %>%
      filter(
        affected == input$radio,
        service == get_current_service(),
        date >= "2022-01-01") %>%
      mutate_date_parts(date)
    
    # Expand to include non-service days
    # This ensures that we have all days, even if there were no alerts
    all_dates <- tibble(
      date = seq.Date(
        from = today() - window_size,
        to = today() + window_size,
        by = 1)) %>%
      mutate_date_parts(date) %>%
      select(-date, -year) 
    
    selected_alert_counts %>%
      inner_join(
        all_dates,
        by = c("month", "week_of_month", "day_of_week")) %>%
      select(
        service, affected, month, week_of_month, day_of_week, 
        year, date, everything()) %>%
      arrange(month, week_of_month, day_of_week)

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
    n_current_alerts <- 
      selected_rt_alerts()$data %>%
      count() %>%
      pull(n)
    
    # TODO: Make window size an input
    window_size = 14
    
    p <-
      data %>%
      # Use a funky date col to keep the X axis ordered correctly
      mutate(
        yoy_comp_date = paste(
          month(month, label = T), 
          "week", week_of_month, 
          wday(day_of_week, label = T, week_start = 7)),
        # Highlight today's date on the x axis
        yoy_comp_date = if_else(
          month == month(today()) &
            week_of_month == ceiling(day(today()) / 7) &
            day_of_week == wday(today(), week_start = 7),
          paste("(Today)", yoy_comp_date),
          yoy_comp_date)) %>%
      arrange(month, week_of_month, day_of_week) %>%
      mutate(yoy_comp_date = factor(
        yoy_comp_date, 
        levels = unique(yoy_comp_date), 
        ordered = T)) %>%
      ggplot(aes(yoy_comp_date, n_alerts, fill = factor(year))) +
      geom_col(
        position = position_dodge(preserve = "single")) +
      geom_hline(
        aes(yintercept = n_current_alerts, linetype = "Current"),
        color = "red"
      ) + 
      scale_linetype_manual(
        name = "",
        values = c("Current" = "dashed")
      ) +
      scale_y_continuous(breaks = pretty_breaks()) + 
      scale_fill_brewer() + 
      theme(
        axis.text.x = element_markdown(angle = 45, hjust = 1)) +
      labs(
        title = paste(
          "Alerts per day for", 
          get_current_service(), input$radio, "trains"),
        x = "", 
        y = "Alerts",
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
  
  
  output$alert_vol_ecdf <- renderPlotly({
    
    # Load the reactive data
    # data <- selected_route_day_vol_data() 
    
    # Get current alert volume
    n_current_alerts <- 
      selected_rt_alerts()$data %>%
      count() %>%
      pull(n)
    
    p_ecdf <- 
      selected_route_day_vol_data() %>%
      ggplot(aes(n_alerts)) + 
      stat_ecdf()
      
    
    # Get coordinates to highlight today's value 
    today_y <- 
      layer_data(p_ecdf, 1) %>%
      as_tibble() %>%
      mutate(diff = abs(x - n_current_alerts)) %>%
      arrange(diff) %>%
      head(1) %>%
      pull(y)
    
    p <- 
      p_ecdf + 
      annotate(
        "segment",
        x = n_current_alerts,
        y = 0,
        xend = n_current_alerts,
        yend = today_y,
        linetype = "dashed",
        color = "red"
      ) + 
      annotate(
        "segment",
        x = 0,
        y = today_y,
        xend = n_current_alerts,
        yend = today_y,
        linetype = "dashed",
        color = "red"
      ) + 
      # Plotly doesn't respect geom_label
      # annotate(
      #   "label",
      #   x = 1,
      #   y = today_y,
      #   label = round(today_y, 2),
      #   color = "red",
      #   vjust = -3
      # ) +
      labs(
        title = paste(
          "ECDF: Alerts per day for", 
          get_current_service(), input$radio, "trains"),
        x = "Number of alerts per day",
        y = "Fraction of all days, 2022 - 2024"
      )
    
    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = 1,
            y = today_y,
            text = round(today_y, 2),
            showarrow = FALSE,
            xref = "x",
            yref = "y",
            font = list(color = "red"),
            bgcolor = "#FFFFFF",  # Background color with opacity
            bordercolor = "red",
            borderwidth = 2,
            borderpad = 4)))
    
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
