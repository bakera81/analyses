library(tidyverse)
library(reticulate)
library(jsonlite)

virtualenv_create("r-nyc_transit")
virtualenv_install(
  "r-nyc_transit", 
  packages = c(
    "gtfs-realtime-bindings",
    "requests"))
use_virtualenv("r-nyc_transit")

### UI HELPER FUNCTIONS

radio_ui_style <- function() {
  tags$style(HTML("
    /* Hide the actual radio buttons */
    #radio .shiny-options-group input[type='radio'] {
      display: none;
    }
    
    /* Style for unselected items */
    #radio .shiny-options-group .radio-inline {
      padding-left: 0;
      margin-right: 0;
      transition: opacity 0.2s ease, transform 0.2s ease;
    }
    
    /* Style for selected item */
    #radio .shiny-options-group .radio-inline.active {
      transform: scale(1.05);
    }
    
    /* Create the border box around selected items */
    #radio .shiny-options-group .radio-inline.active::after {
      content: '';
      position: absolute;
      top: -3px;
      left: -3px;
      right: -3px;
      bottom: -3px;
      border: 3px solid #ffffff;
      border-radius: 50%;
      box-shadow: 0 0 0 1px #000000; /* Add a thin black outline for better visibility */
      pointer-events: none; /* Ensures the border doesn't interfere with clicks */
    }
    
    /* Remove margin from shiny's default radio group */
    #radio .shiny-options-group {
      margin-left: 0;
    }
  "))
}

subway_ui <- function(route_id, route_grouping, ...) {
  subway_style <- paste0("
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
      cursor: pointer;
      margin: 5px;")
  
  tags$div(
    style = subway_style,
    route_id
  )
}


get_radio_choice_names <- function(all_routes) {
  all_routes %>%
    pmap(subway_ui)
}

alert_accordion_ui <- function(header, description, ...) {
  accordion_panel(
    title = header,
    tags$p(description)
  )
}


### DATA HELPER FUNCTIONS

mutate_service <- function(.tbl, date_col, service_col = "service") {
  service_col_name <- rlang::sym(service_col)
  
  .tbl %>%
    mutate(
      !!service_col_name := case_when(
        wday({{ date_col }}, week_start = 7) %in% c(7, 1) ~ "weekend",
        hour({{ date_col }}) >= 5 ~ "weekday",
        TRUE ~ "weeknight"))   
}

get_current_service <- function() {
  case_when(
    wday(today(), week_start = 7) %in% c(7, 1) ~ "weekend",
    hour(now()) >= 5 ~ "weekday",
    TRUE ~ "weeknight")   
}


factorize_status_label <- function(.tbl, status_col) {
  
  # Create the join specification
  status_col_expr <- enquo(status_col)
  status_col_name <- quo_name(status_col_expr)
  # Always join raw_status with the given column
  join_by <- setNames("raw_status", status_col_name)
  
  status_levels <- 
    c("update", "some delays", "delays", "partial suspension", "no service")
  
  simplified_statuses <- 
    tribble(
      ~raw_status,          ~simplified_status_label,  
      "delays",               "delays",
      "weekday-service",      "update",
      "local-to-express",     "partial suspension",
      "part-suspended",       "partial suspension",
      "essential-service",    "partial suspension",
      "some-delays",          "some delays",
      "express-to-local",     "some delays",
      "no-scheduled-service", "no service",
      "trains-rerouted",      "partial suspension",
      "weekend-service",      "some delays",
      "stops-skipped",        "some delays",
      "some-reroutes",        "partial suspension",
      "slow-speeds",          "some delays",
      "reroute",              "partial suspension",
      "stations-skipped",     "partial suspension",
      "suspended",            "no service",
      "multiple-changes",     "update",
      "boarding-change",      "update",
      "multiple-impacts",     "update",
      "sunday-schedule",      "some delays",
      "service-change",       "update",
      "cancellations",        "no service",
      "severe-delays",        "delays",
      "information-outage",   "update",
      "shuttle-buses-detoured",     "partial suspension", 
      "planned-work",               "update",
      "arrival-information-outage", "some delays",
      "saturday-schedule",          "some delays",
      "special-notice",             "update",
      "station-notice",             "update",
      "crowding",                   "some delays",
      "on-or-close",                "update",
      "special-event",              "update"
    ) %>%
    mutate(
      simplified_status = factor(
        simplified_status_label,
        levels = status_levels)) %>%
    select(-simplified_status_label)
      
  
  .tbl %>%
    left_join(
      simplified_statuses,
      by = join_by) 
}

enrich_routes <- function(.tbl, route_col) {
  sort_levels <- c(
    "#EE352E",
    "#00933C",
    "#B933AD",
    "#0039A6",
    "#FF6319",
    "#FCCC0A",
    "#996633",
    "#A7A9AC",
    "#6CBE45",
    "#808183"
  )
  
  .tbl %>%
    mutate(
      route_grouping = case_when(
        {{ route_col }} %in% c("1", "2", "3") ~ "#EE352E",
        {{ route_col }} %in% c("4", "5", "6", "6X") ~ "#00933C",
        {{ route_col }} %in% c("7", "7X") ~ "#B933AD",
        {{ route_col }} %in% c("A", "C", "E") ~ "#0039A6",
        {{ route_col }} %in% c("B", "D", "F", "M") ~ "#FF6319",
        {{ route_col }} %in% c("N", "Q", "R", "W") ~ "#FCCC0A",
        {{ route_col }} %in% c("J", "Z") ~ "#996633",
        {{ route_col }} == "L" ~ "#A7A9AC",
        {{ route_col }} == "G" ~ "#6CBE45",
        {{ route_col }} == "S" ~ "#808183",
        TRUE ~ "other")) %>%
    mutate(
      route_grouping_fct = factor(
        route_grouping,
        levels = sort_levels
      )) %>%
    arrange(route_grouping_fct)
}


# Saves an RDS file
clean_historical_alert_data <- function() {
  # Fetch data
  # https://catalog.data.gov/dataset/mta-service-alerts-beginning-april-2020
  service_alerts <- 
    read_csv("MTA_Service_Alerts__Beginning_April_2020.csv") %>%
    janitor::clean_names() %>%
    filter(agency == "NYCT Subway") 
    # TODO: REMOVE THIS
    # filter(str_detect(affected, "1") | str_detect(affected, "Q") | str_detect(affected, "D"))
  # sample_n(1000)
  
  # Assumption: the alerts are only in affect if there is an update posted on the day
  # Clean duration data
  # TODO: save the cleaned data to decrease latency
  cleaned_service_alerts <- 
    service_alerts %>%
    mutate(
      status_label = str_split(status_label, fixed(" | ")),
      affected = str_split(affected, fixed(" | ")),
      date = mdy_hms(date)) %>%
    mutate_service(date) %>%
    unnest(status_label) %>%
    unnest(affected) %>%
    factorize_status_label(status_label) %>%
    group_by(service, affected, date = date(date)) %>%
    summarise(
      n_alerts = n_distinct(alert_id),
      # update_number is the highest update number for the corresponding event 
      #   that had an alert within the day.
      update_number = max(update_number, na.rm = T),
      most_minor_alert = 
        levels(simplified_status)[min(as.integer(simplified_status))],
      most_major_alert = 
        levels(simplified_status)[max(as.integer(simplified_status))],
      all_alert_statuses = paste(simplified_status, collapse = ","))
  
  saveRDS(cleaned_service_alerts, "./cleaned_service_alerts.rds")
}

# Reads and RDS file
get_historical_alert_data <- function() {
  readRDS("cleaned_service_alerts.rds")
}

# Aggregated by the event causing the alerts
get_historical_event_data <- function() {
  # Fetch data
  # https://catalog.data.gov/dataset/mta-service-alerts-beginning-april-2020
  service_alerts <- 
    read_csv("../MTA_Service_Alerts__Beginning_April_2020.csv") %>%
    janitor::clean_names() %>%
    filter(agency == "NYCT Subway") %>%
    # TODO: REMOVE THIS
    filter(str_detect(affected, "1") | str_detect(affected, "Q") | str_detect(affected, "D"))
    # sample_n(1000)
  
  # Clean duration data
  # TODO: save the cleaned data to decrease latency
  service_alerts %>%
    mutate(
      status_label = str_split(status_label, fixed(" | ")),
      affected = str_split(affected, fixed(" | ")),
      date = mdy_hms(date)) %>%
    mutate_service(date) %>%
    unnest(status_label) %>%
    unnest(affected) %>%
    factorize_status_label(status_label) %>%
    group_by(event_id, service, affected) %>%
    summarise(
      n_updates = n_distinct(alert_id),
      start_time = min(date, na.rm = T),
      end_time = max(date, na.rm = T),
      most_minor_alert = 
        levels(simplified_status)[min(as.integer(simplified_status))],
      most_major_alert = 
        levels(simplified_status)[max(as.integer(simplified_status))],
      all_alert_statuses = paste(simplified_status, collapse = ",")) %>%
    ungroup() %>%
    mutate(duration_est = end_time - start_time) %>%
    filter(start_time != end_time) 
}

get_historical_active_alert_days <- function(selected_route_data){
  
  date_range <- selected_route_data %>%
    ungroup() %>%
    summarise(
      min_date = date(min(start_time, na.rm = T)),
      max_date = date(max(end_time, na.rm = T))) 
  
  all_dates <- 
    seq.Date(
      from = date_range$min_date,
      to = date_range$max_date,
      by = 1)
  
  event_day <- selected_route_data %>%
    distinct(event_id) %>%
    crossing(date = all_dates)
  
  # active_event_days <- 
  event_day %>%
    inner_join(
      selected_route_data, by = "event_id"
    ) %>%
    filter(date >= date(start_time) & date <= date(end_time)) %>%
    group_by(affected, service, date) %>%
    summarise(
      n_events = n(),
      first_update_at = min(start_time, na.rm = T),
      last_update_at = max(end_time, na.rm = T)) 
  
}

get_gtfs_rt_alerts <- function() {
  source_python("gtfs_rt.py")
  
  alerts_json <- get_mta_alerts() %>%
    jsonlite::fromJSON(flatten = T)
  
  alerts_tbl <- 
    alerts_json %>%
    as_tibble() %>%
    tidyr::unnest_wider(everything(), names_sep = "_") %>%
    janitor::clean_names() %>%
    rename(
      id = id_1, 
      route_id = alert_informed_entity_route_id,
      header = alert_header_text_translation_text,
      header_avail_translations = alert_header_text_translation_language,
      description = alert_description_text_translation_text,
      description_avail_translations = alert_description_text_translation_language)  %>%
    mutate(date = now())
  
  affected_routes <- 
    alerts_tbl %>%
    select(id, route_id, date) %>%
    unnest(route_id)  %>%
    filter(!is.na(route_id))
  
  headers <-
    alerts_tbl %>% 
    select(id, contains("header")) %>%
    # If they don't all have the same length, use and extra ID and the 
    #  id_col arg in pivot wider to keep everything matched up.
    # mutate(temp_id = row_number()) %>% 
    unnest(-contains("id")) %>%
    mutate(
      header_avail_translations = snakecase::to_snake_case(
        header_avail_translations)) %>%
    pivot_wider(
      names_from = header_avail_translations,
      values_from = header,
      names_prefix = "header__"
    ) 
  
  descriptions <- 
    alerts_tbl %>% 
    select(id, contains("description")) %>%
    unnest(-contains("id")) %>%
    mutate(
      description_avail_translations = snakecase::to_snake_case(
        description_avail_translations)) %>%
    pivot_wider(
      names_from = description_avail_translations,
      values_from = description,
      names_prefix = "description__") 
  
  affected_routes %>% 
    inner_join(headers, by = "id") %>%
    inner_join(descriptions, by = "id") %>%
    mutate(
      status_label = str_extract(id, ":(.+):", group = 1),
      id_raw = id,
      id = str_extract(id_raw, ":(\\d+)$", group = 1)) %>%
    mutate_service(date)
}

get_all_routes <- function(past_alerts) {
  past_alerts %>%
    ungroup() %>%
    distinct(route_id = affected) %>%
    enrich_routes(route_id) %>%
    select(-route_grouping_fct)
}

get_real_window_size <- function(window_size, current_service = get_current_service()) {
  current_service_days <- c(
    "weekday" = 5,
    "weeknight" = 5,
    "weekend" = 2
  )
  
  ceiling(7 / current_service_days[[current_service]] * window_size)
  
}

mutate_date_parts <- function(.tbl, date_col) {
  .tbl %>%
    mutate(
      year = year({{ date_col }}),
      month = month({{ date_col }}),
      week_of_month = ceiling(day({{ date_col }}) / 7),
      day_of_week = wday({{ date_col }}, week_start = 7))
}
