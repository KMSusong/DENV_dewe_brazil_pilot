#' --- 
#' title: "02 correlation between outbreak variable and disaster types" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-25" 
#' --- 

#' Overview: 
#'  calculate the correlation between the outbreak variable (true/false) at 
#'  different years for the mean with 
#'  the different disaster types at all of the time lags

#' Timeline: 
#'   2026-06-25 
#' 


library(tidyverse)
outbreak_5 <- read_csv("00_Data/5_year_outbreak_data.csv")
outbreak_7 <- read_csv("00_Data/7_year_outbreak_data.csv")
outbreak_10 <- read_csv("00_Data/10_year_outbreak_data.csv")

disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")


# 5-year mean function for correlating outbreak with nat dis types:
calc_lag_cors_outbreak_5 <- function(search_term = NULL,
                                   data        = disasters_plot,
                                   min_months  = 0) {
  
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  #choose mean years here
  dengue_lagged <- outbreak_5 |>
    # Drop months where outbreak could not be calculated (incomplete 5yr window)
    filter(!is.na(outbreak)) |>
    left_join(dis_monthly,
              by = c("adm_1_name", "join_year", "join_month")) |>
    mutate(
      n_events  = replace_na(n_events, 0),
      outbreak  = as.integer(outbreak)
    ) |>
    arrange(adm_1_name, join_year, join_month) |>
    group_by(adm_1_name) |>
    mutate(
      lag1 = lag(n_events, 1),
      lag2 = lag(n_events, 2),
      lag3 = lag(n_events, 3),
      lag4 = lag(n_events, 4),
      lag5 = lag(n_events, 5)
    ) |>
    ungroup()
  
  cors <- dengue_lagged |>
    filter(!is.na(adm_1_name)) |>
    group_by(adm_1_name) |>
    summarise(
      cor_lag0 = suppressWarnings(
        cor(outbreak, n_events, use = "complete.obs", method = "spearman")
      ),
      cor_lag1 = suppressWarnings(cor(outbreak, lag1, use = "complete.obs", method = "spearman")),
      cor_lag2 = suppressWarnings(cor(outbreak, lag2, use = "complete.obs", method = "spearman")),
      cor_lag3 = suppressWarnings(cor(outbreak, lag3, use = "complete.obs", method = "spearman")),
      cor_lag4 = suppressWarnings(cor(outbreak, lag4, use = "complete.obs", method = "spearman")),
      cor_lag5 = suppressWarnings(cor(outbreak, lag5, use = "complete.obs", method = "spearman")),
      n_outbreak_months = sum(outbreak, na.rm = TRUE),
      n_event_months    = sum(n_events > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(if (min_months > 0) n_event_months >= min_months else TRUE)
  
  cors |>
    pivot_longer(
      cols      = starts_with("cor_lag"),
      names_to  = "lag",
      values_to = "correlation"
    ) |>
    mutate(lag = as.integer(str_extract(lag, "[0-9]+")))
}

# Usage:
lag_cors_outbreak_floods_5 <- calc_lag_cors_outbreak_5("inunda")
lag_cors_outbreak_movement_5 <- calc_lag_cors_outbreak_5("massa")
lag_cors_outbreak_urb_flood_5 <- calc_lag_cors_outbreak_5("alaga")
lag_cors_outbreak_drought_5 <- calc_lag_cors_outbreak_5("seca")
lag_cors_outbreak_flash_5 <- calc_lag_cors_outbreak_5("enxu")

plot_lag_heatmap(lag_cors_outbreak_floods_5, "Floods(inunda) vs outbreak months (5-year)")
plot_lag_heatmap(lag_cors_outbreak_urb_flood_5, "Urban Floods(alagam) vs outbreak months (5-year)")
plot_lag_heatmap(lag_cors_outbreak_movement_5, "Mass movement vs outbreak months (5-year)")
plot_lag_heatmap(lag_cors_outbreak_drought_5, "Drought vs outbreak months (5-year)")
plot_lag_heatmap(lag_cors_outbreak_flash_5, "Flash Floods vs outbreak months (5-year)")




# 7-year mean function for correlating outbreak with nat dis types:
calc_lag_cors_outbreak_7 <- function(search_term = NULL,
                                     data        = disasters_plot,
                                     min_months  = 0) {
  
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  #choose mean years here
  dengue_lagged <- outbreak_7 |>
    # Drop months where outbreak could not be calculated (incomplete 5yr window)
    filter(!is.na(outbreak)) |>
    left_join(dis_monthly,
              by = c("adm_1_name", "join_year", "join_month")) |>
    mutate(
      n_events  = replace_na(n_events, 0),
      outbreak  = as.integer(outbreak)
    ) |>
    arrange(adm_1_name, join_year, join_month) |>
    group_by(adm_1_name) |>
    mutate(
      lag1 = lag(n_events, 1),
      lag2 = lag(n_events, 2),
      lag3 = lag(n_events, 3),
      lag4 = lag(n_events, 4),
      lag5 = lag(n_events, 5)
    ) |>
    ungroup()
  
  cors <- dengue_lagged |>
    filter(!is.na(adm_1_name)) |>
    group_by(adm_1_name) |>
    summarise(
      cor_lag0 = suppressWarnings(
        cor(outbreak, n_events, use = "complete.obs", method = "spearman")
      ),
      cor_lag1 = suppressWarnings(cor(outbreak, lag1, use = "complete.obs", method = "spearman")),
      cor_lag2 = suppressWarnings(cor(outbreak, lag2, use = "complete.obs", method = "spearman")),
      cor_lag3 = suppressWarnings(cor(outbreak, lag3, use = "complete.obs", method = "spearman")),
      cor_lag4 = suppressWarnings(cor(outbreak, lag4, use = "complete.obs", method = "spearman")),
      cor_lag5 = suppressWarnings(cor(outbreak, lag5, use = "complete.obs", method = "spearman")),
      n_outbreak_months = sum(outbreak, na.rm = TRUE),
      n_event_months    = sum(n_events > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(if (min_months > 0) n_event_months >= min_months else TRUE)
  
  cors |>
    pivot_longer(
      cols      = starts_with("cor_lag"),
      names_to  = "lag",
      values_to = "correlation"
    ) |>
    mutate(lag = as.integer(str_extract(lag, "[0-9]+")))
}

# Usage:
lag_cors_outbreak_floods_7 <- calc_lag_cors_outbreak_7("inunda")
lag_cors_outbreak_movement_7 <- calc_lag_cors_outbreak_7("massa")
lag_cors_outbreak_urb_flood_7 <- calc_lag_cors_outbreak_7("alaga")
lag_cors_outbreak_drought_7 <- calc_lag_cors_outbreak_7("seca")
lag_cors_outbreak_flash_7 <- calc_lag_cors_outbreak_7("enxu")

plot_lag_heatmap(lag_cors_outbreak_floods_7, "Floods(inunda) vs outbreak months (7-year)")
plot_lag_heatmap(lag_cors_outbreak_urb_flood_7, "Urban Floods(alagam) vs outbreak months (7-year)")
plot_lag_heatmap(lag_cors_outbreak_movement_7, "Mass movement vs outbreak months (7-year)")
plot_lag_heatmap(lag_cors_outbreak_drought_7, "Drought vs outbreak months (7-year)")
plot_lag_heatmap(lag_cors_outbreak_flash_7, "Flash Floods vs outbreak months (7-year)")





# 10-year mean function for correlating outbreak with nat dis types:
calc_lag_cors_outbreak_10 <- function(search_term = NULL,
                                     data        = disasters_plot,
                                     min_months  = 0) {
  
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  #choose mean years here
  dengue_lagged <- outbreak_10 |>
    # Drop months where outbreak could not be calculated (incomplete 5yr window)
    filter(!is.na(outbreak)) |>
    left_join(dis_monthly,
              by = c("adm_1_name", "join_year", "join_month")) |>
    mutate(
      n_events  = replace_na(n_events, 0),
      outbreak  = as.integer(outbreak)
    ) |>
    arrange(adm_1_name, join_year, join_month) |>
    group_by(adm_1_name) |>
    mutate(
      lag1 = lag(n_events, 1),
      lag2 = lag(n_events, 2),
      lag3 = lag(n_events, 3),
      lag4 = lag(n_events, 4),
      lag5 = lag(n_events, 5)
    ) |>
    ungroup()
  
  cors <- dengue_lagged |>
    filter(!is.na(adm_1_name)) |>
    group_by(adm_1_name) |>
    summarise(
      cor_lag0 = suppressWarnings(
        cor(outbreak, n_events, use = "complete.obs", method = "spearman")
      ),
      cor_lag1 = suppressWarnings(cor(outbreak, lag1, use = "complete.obs", method = "spearman")),
      cor_lag2 = suppressWarnings(cor(outbreak, lag2, use = "complete.obs", method = "spearman")),
      cor_lag3 = suppressWarnings(cor(outbreak, lag3, use = "complete.obs", method = "spearman")),
      cor_lag4 = suppressWarnings(cor(outbreak, lag4, use = "complete.obs", method = "spearman")),
      cor_lag5 = suppressWarnings(cor(outbreak, lag5, use = "complete.obs", method = "spearman")),
      n_outbreak_months = sum(outbreak, na.rm = TRUE),
      n_event_months    = sum(n_events > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(if (min_months > 0) n_event_months >= min_months else TRUE)
  
  cors |>
    pivot_longer(
      cols      = starts_with("cor_lag"),
      names_to  = "lag",
      values_to = "correlation"
    ) |>
    mutate(lag = as.integer(str_extract(lag, "[0-9]+")))
}

# Usage:
lag_cors_outbreak_floods_10 <- calc_lag_cors_outbreak_10("inunda")
lag_cors_outbreak_movement_10 <- calc_lag_cors_outbreak_10("massa")
lag_cors_outbreak_urb_flood_10 <- calc_lag_cors_outbreak_10("alaga")
lag_cors_outbreak_drought_10 <- calc_lag_cors_outbreak_10("seca")
lag_cors_outbreak_flash_10 <- calc_lag_cors_outbreak_10("enxu")

plot_lag_heatmap(lag_cors_outbreak_floods_10, "Floods(inunda) vs outbreak months (10-year)")
plot_lag_heatmap(lag_cors_outbreak_urb_flood_10, "Urban Floods(alagam) vs outbreak months (10-year)")
plot_lag_heatmap(lag_cors_outbreak_movement_10, "Mass movement vs outbreak months (10-year)")
plot_lag_heatmap(lag_cors_outbreak_drought_10, "Drought vs outbreak months (10-year)")
plot_lag_heatmap(lag_cors_outbreak_flash_10, "Flash Floods vs outbreak months (10-year)")

