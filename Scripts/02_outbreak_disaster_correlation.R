#' --- 
#' title: "02 correlation between outbreak variable and disaster types" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-25" 
#' --- 

#' Overview: 
#'  calculate the correlation between the outbreak variable (true/false) at 
#'  different years for the mean with 
#'  the different disaster types at all of the time lags;
#'  calc how many disasters post 2020

#' Timeline: 
#'   2026-06-25 
#'   2026-07-4 - add correlation by admin 2 level
#' 


library(tidyverse)
outbreak_5 <- read_csv("00_Data/5_year_outbreak_data.csv")
outbreak_7 <- read_csv("00_Data/7_year_outbreak_data.csv")
outbreak_10 <- read_csv("00_Data/10_year_outbreak_data.csv")
adm2_outbreak <-read_csv("00_Data/adm2_outbreak_data.csv")

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


#how many disasters post 2020
disasters_plot |>
  mutate(post_2020 = year(date) >= 2020) |>
  summarise(
    n_total     = n(),
    n_post_2020 = sum(post_2020, na.rm = TRUE),
    pct_post_2020 = round(100 * n_post_2020 / n_total, 1)
  )

disasters_plot |>
  mutate(post_2020 = year(date) >= 2020) |>
  group_by(descricao_tipologia) |>
  summarise(
    n_total       = n(),
    n_post_2020   = sum(post_2020, na.rm = TRUE),
    pct_post_2020 = round(100 * n_post_2020 / n_total, 1)
  ) |>
  arrange(desc(pct_post_2020))

######-----------admin 2 level correlation
calc_lag_cors_adm2 <- function(search_term = NULL,
                               data        = disasters_plot,
                               min_months  = 0) {
  
  # Filter to disaster type if specified
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  # Aggregate disasters to municipality-month
  dis_monthly <- dis |>
    filter(!is.na(cod_ibge_mun)) |>
    mutate(
      IBGE_code  = as.character(cod_ibge_mun),
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(IBGE_code, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  
  # Join to Admin2 outbreak data and calculate lags
  combined <- adm2_outbreak |>
    mutate(IBGE_code = as.character(IBGE_code)) |>
    filter(!is.na(outbreak)) |>
    left_join(dis_monthly,
              by = c("IBGE_code", "join_year", "join_month")) |>
    mutate(
      n_events = replace_na(n_events, 0),
      outbreak = as.integer(outbreak)
    ) |>
    arrange(IBGE_code, join_year, join_month) |>
    group_by(IBGE_code) |>
    mutate(
      lag0 = n_events,
      lag1 = lag(n_events, 1),
      lag2 = lag(n_events, 2),
      lag3 = lag(n_events, 3),
      lag4 = lag(n_events, 4),
      lag5 = lag(n_events, 5)
    ) |>
    ungroup()
  
  # Correlations per municipality
  cors <- combined |>
    filter(!is.na(IBGE_code)) |>
    group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
    summarise(
      cor_lag0  = safe_cor(outbreak, lag0),
      cor_lag1  = safe_cor(outbreak, lag1),
      cor_lag2  = safe_cor(outbreak, lag2),
      cor_lag3  = safe_cor(outbreak, lag3),
      cor_lag4  = safe_cor(outbreak, lag4),
      cor_lag5  = safe_cor(outbreak, lag5),
      n_event_months    = sum(lag0 > 0,   na.rm = TRUE),
      n_outbreak_months = sum(outbreak,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(if (min_months > 5) n_event_months >= min_months else TRUE) 
  cors |>
    pivot_longer(
      cols      = starts_with("cor_lag"),
      names_to  = "lag",
      values_to = "correlation"
    ) |>
    mutate(lag = as.integer(str_extract(lag, "[0-9]+")))
}


safe_cor <- function(x, y) {
  tryCatch(
    suppressWarnings(cor(x, y, use = "complete.obs", method = "spearman")),
    error = function(e) NA_real_
  )
}
# Usage:
cors_adm2_floods  <- calc_lag_cors_adm2("inunda")
cors_adm2_drought <- calc_lag_cors_adm2("seca")
cors_adm2_all     <- calc_lag_cors_adm2()

# Filter to municipalities with at least 6 months of disaster events
cors_adm2_floods_filtered <- calc_lag_cors_adm2("inunda", min_months = 6)

# Mean correlation by state and lag
cors_adm2_floods |>
  group_by(adm_1_name, lag) |>
  summarise(
    mean_cor       = round(mean(correlation, na.rm = TRUE), 3),
    n_municipalities = n_distinct(IBGE_code),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = lag, values_from = mean_cor,
              names_prefix = "lag")

# Which municipalities have the strongest correlation at any lag?
cors_adm2_floods |>
  group_by(IBGE_code, adm_2_name, adm_1_name) |>
  summarise(
    max_cor  = max(abs(correlation), na.rm = TRUE),
    best_lag = if (all(is.na(correlation))) NA_integer_ else lag[which.max(abs(correlation))],
    .groups = "drop"
  ) |>
  arrange(desc(max_cor)) |>
  head(20)
