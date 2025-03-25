library(tidyverse)
library(reticulate)
library(jsonlite)

use_virtualenv("r-nyc_transit")

mutate_service <- function(.tbl, date_col) {
  .tbl %>%
    mutate(
      service = case_when(
        wday({{ date_col }}, week_start = 1) >= 6 ~ "weekend",
        hour({{ date_col }}) >= 5 ~ "weekday",
        TRUE ~ "weeknight"))   
}


factorize_status_label <- function(.tbl, status_col) {
  
  # Create the join specification
  status_col_expr <- enquo(status_col)
  status_col_name <- quo_name(status_col_expr)
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
  .tbl %>%
    mutate(
      route_grouping = case_when(
        {{ route_col }} %in% c("1", "2", "3") ~ "red",
        {{ route_col }} %in% c("4", "5", "6", "6X") ~ "darkgreen",
        {{ route_col }} %in% c("7", "7X") ~ "purple",
        {{ route_col }} %in% c("A", "C", "E") ~ "blue",
        {{ route_col }} %in% c("B", "D", "F", "M") ~ "darkorange",
        {{ route_col }} %in% c("N", "Q", "R", "W") ~ "yellow",
        {{ route_col }} %in% c("J", "Z") ~ "brown",
        {{ route_col }} == "L" ~ "gray",
        {{ route_col }} == "G" ~ "green",
        TRUE ~ "other")) 
}


get_historical_alert_data <- function() {
  # Fetch data
  # https://catalog.data.gov/dataset/mta-service-alerts-beginning-april-2020
  service_alerts <- 
    read_csv("../MTA_Service_Alerts__Beginning_April_2020.csv") %>%
    janitor::clean_names() %>%
    filter(agency == "NYCT Subway") 
  
  # Clean duration data
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

get_gtfs_rt_alerts <- function() {
  source_python("../gtfs_rt.py")
  
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