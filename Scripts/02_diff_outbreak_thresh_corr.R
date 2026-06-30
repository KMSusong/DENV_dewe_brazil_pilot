#' --- 
#' title: "02 correlation of outbreak and disasters by diff outbreak thresholds" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-25" 
#' --- 

#' Overview: 
#'  calculate different outbreak thresholds (1.25, 1.5 and 2 times sd) and compare the
#'  resulting outbreak correlation with different disaster types

#' Timeline: 
#'   2026-06-25 
#' 


library(tidyverse)
disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")
outbreak_5 <- read_csv("00_Data/5_year_outbreak_data.csv")
outbreak_7 <- read_csv("00_Data/7_year_outbreak_data.csv")
outbreak_10 <- read_csv("00_Data/10_year_outbreak_data.csv")

calc_threshold_cors <- function(sd_multipliers = c(1.25, 1.5, 2.0),
                                search_term    = NULL,
                                data           = disasters_plot,
                                min_months     = 0) {
  
  # --- Add outbreak column for each SD multiplier ---
  dengue_multi <- outbreak_5 |>
    filter(!is.na(mean_prev_5yr), !is.na(sd_prev_5yr))
  
  for (m in sd_multipliers) {
    col_name <- paste0("outbreak_", gsub("\\.", "_", as.character(m)), "sd")
    dengue_multi <- dengue_multi |>
      mutate(!!col_name := as.integer(
        dengue_total > (mean_prev_5yr + m * sd_prev_5yr)
      ))
  }
  
  # --- Filter and aggregate disasters ---
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(join_year = year(date), join_month = month(date)) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  
  # --- Join and calculate lags ---
  combined <- dengue_multi |>
    left_join(dis_monthly,
              by = c("adm_1_name", "join_year", "join_month")) |>
    mutate(n_events = replace_na(n_events, 0)) |>
    arrange(adm_1_name, join_year, join_month) |>
    group_by(adm_1_name) |>
    mutate(
      lag0 = n_events,
      lag1 = lag(n_events, 1),
      lag2 = lag(n_events, 2),
      lag3 = lag(n_events, 3),
      lag4 = lag(n_events, 4),
      lag5 = lag(n_events, 5)
    ) |>
    ungroup()
  
  outbreak_cols <- paste0("outbreak_",
                          gsub("\\.", "_", as.character(sd_multipliers)), "sd")
  lag_cols      <- paste0("lag", 0:5)
  
  # --- Loop over each SD multiplier and lag, calculate correlation ---
  map_dfr(seq_along(sd_multipliers), function(i) {
    m       <- sd_multipliers[i]
    out_col <- outbreak_cols[i]
    
    map_dfr(0:5, function(l) {
      lag_col <- paste0("lag", l)
      
      combined |>
        filter(!is.na(.data[[lag_col]])) |>
        group_by(adm_1_name) |>
        summarise(
          sd_multiplier   = m,
          threshold_label = paste0(m, " SD"),
          lag             = l,
          correlation     = suppressWarnings(
            cor(as.integer(.data[[out_col]]), .data[[lag_col]],
                use = "complete.obs", method = "spearman")
          ),
          n_event_months  = sum(.data[[lag_col]] > 0, na.rm = TRUE),
          n_outbreak_months = sum(.data[[out_col]], na.rm = TRUE),
          .groups = "drop"
        )
    })
  }) |>
    filter(if (min_months > 0) n_event_months >= min_months else TRUE)
}


# --- Plot 1: national mean correlation by lag and threshold ---
plot_threshold_cors <- function(search_term    = NULL,
                                sd_multipliers = c(1.25, 1.5, 2.0),
                                data           = disasters_plot,
                                min_months     = 0) {
  
  results <- calc_threshold_cors(sd_multipliers, search_term, data, min_months)
  
  national <- results |>
    group_by(threshold_label, sd_multiplier, lag) |>
    summarise(
      mean_correlation = mean(correlation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(threshold_label = factor(threshold_label,
                                    levels = paste0(sort(sd_multipliers), " SD")))
  
  ggplot(national, aes(x = lag, y = mean_correlation,
                       colour = threshold_label, group = threshold_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_label(aes(label = round(mean_correlation, 2)),
               nudge_y = 0.015, size = 2.8, show.legend = FALSE) +
    scale_x_continuous(breaks = 0:5, labels = paste0("Lag ", 0:5)) +
    scale_colour_manual(
      values = c("1.25 SD" = "#2166ac",
                 "1.5 SD"  = "#f4a582",
                 "2 SD"    = "#d6604d"),
      name   = "Outbreak threshold"
    ) +
    ylim(-1, 1) +
    labs(
      title    = paste0("Spearman correlation: disaster events vs outbreak flag",
                        " by SD threshold and lag",
                        if (!is.null(search_term))
                          paste0("\nDisaster type: ", search_term) else ""),
      subtitle = "Mean correlation across all states. Higher SD = stricter outbreak definition.",
      x        = "Lag (months)",
      y        = "Mean Spearman correlation"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}



# Line plot: national mean correlation by lag, one line per threshold
plot_threshold_cors("inunda")
plot_threshold_cors("seca")
plot_threshold_cors("alagamento")
plot_threshold_cors("mass")
plot_threshold_cors("enxu")
plot_threshold_cors()   # all types

# Raw numbers
calc_threshold_cors(search_term = "inunda") |>
  group_by(threshold_label, lag) |>
  summarise(mean_cor = round(mean(correlation, na.rm = TRUE), 3),
            .groups = "drop")