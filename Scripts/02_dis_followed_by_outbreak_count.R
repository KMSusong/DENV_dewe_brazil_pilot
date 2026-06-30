#' --- 
#' title: "02 the number of natural disasters followed by an outbreak month" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-26" 
#' --- 

#' Overview: calculate and tabulate the number of natural disasters, by type, that
#' are followed by an outbreak, and then copy to clipboard to paste to excel;
#' graph the percentages by disaster type and lag at a national level
#'  

#' Timeline: 
#'   2026-06-26 
#' 


library(tidyverse)
disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")
outbreak_5 <- read_csv("00_Data/5_year_outbreak_data.csv")
outbreak_7 <- read_csv("00_Data/7_year_outbreak_data.csv")
outbreak_10 <- read_csv("00_Data/10_year_outbreak_data.csv")

calc_outbreak_after_disaster_5 <- function(search_term = NULL,
                                         data        = disasters_plot,
                                         min_events  = 0) {
  
  # Filter to disaster type if specified
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  # Aggregate disasters to state-month
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  
  # Join to dengue outbreak data and calculate lags
  combined <- outbreak_5 |>
    filter(!is.na(outbreak)) |>
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
  
  # For each lag, count:
  #   - months where >=1 disaster event occurred (disaster month)
  #   - of those, how many were followed by an outbreak at that lag
  #   - total disaster events in those months
  map_dfr(0:5, function(l) {
    
    lag_col <- paste0("lag", l)
    
    combined |>
      filter(!is.na(.data[[lag_col]])) |>
      mutate(had_disaster = .data[[lag_col]] > min_events) |>
      filter(had_disaster) |>
      group_by(adm_1_name) |>
      summarise(
        lag                    = l,
        n_disaster_months      = n(),
        n_events_total         = sum(.data[[lag_col]], na.rm = TRUE),
        n_followed_by_outbreak = sum(outbreak, na.rm = TRUE),
        pct_followed_by_outbreak = round(
          100 * n_followed_by_outbreak / n_disaster_months, 1
        ),
        .groups = "drop"
      )
  }) |>
    arrange(adm_1_name, lag)
}

# Usage:
results_floods <- calc_outbreak_after_disaster_5("inunda")
results_urb_fl <- calc_outbreak_after_disaster_5("alaga")
results_drought <- calc_outbreak_after_disaster_5("seca")
results_movement <- calc_outbreak_after_disaster_5("mass")
results_flash_fl <- calc_outbreak_after_disaster_5("enx")
results_all    <- calc_outbreak_after_disaster_5()

# Filter to a minimum number of disaster events for reliability
results_floods_min10 <- calc_outbreak_after_disaster("inunda", min_events = 10)

# View results for a specific state
results_floods |> filter(adm_1_name == "SAO PAULO")

# Summary across all states by lag
floods_nat_5 <- results_floods |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1)
   )

floods_nat_5 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)


#for droughts
drought_nat_5 <- results_drought |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

drought_nat_5 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

#for urb floods
urb_fl_nat_5 <- results_urb_fl |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

urb_fl_nat_5 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

#flash floods
flash_fl_nat_5 <- results_flash_fl |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

flash_fl_nat_5 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)


#movement
move_nat_5 <- results_movement |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

move_nat_5 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)



# Calculate results for each type and combine
types <- c("inunda", "seca", "alaga", "enxu")


combined_results <- map_dfr(types, function(t) {
  calc_outbreak_after_disaster_5(t) |>
    group_by(lag) |>
    summarise(
      pct_followed_by_outbreak = round(
        100 * sum(n_followed_by_outbreak) / sum(n_disaster_months), 1
      ),
      .groups = "drop"
    ) |>
    mutate(disaster_type = t)
})

ggplot(combined_results, aes(x = lag, y = pct_followed_by_outbreak,
                             colour = disaster_type, group = disaster_type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_label(aes(label = paste0(pct_followed_by_outbreak, "%")),
             nudge_y = 1.5, size = 2.5, show.legend = FALSE) +
  scale_x_continuous(breaks = 0:5, labels = paste0("Lag ", 0:5)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Percentage of disaster months followed by a dengue outbreak by disaster type",
    x        = "Lag (months)",
    y        = "% followed by outbreak",
    colour   = "Disaster type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )





######---same thing but for 10 year mean
calc_outbreak_after_disaster_10 <- function(search_term = NULL,
                                           data        = disasters_plot,
                                           min_events  = 0) {
  
  # Filter to disaster type if specified
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
  }
  
  # Aggregate disasters to state-month
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  
  # Join to dengue outbreak data and calculate lags
  combined <- outbreak_10 |>
    filter(!is.na(outbreak)) |>
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
  
  # For each lag, count:
  #   - months where >=1 disaster event occurred (disaster month)
  #   - of those, how many were followed by an outbreak at that lag
  #   - total disaster events in those months
  map_dfr(0:5, function(l) {
    
    lag_col <- paste0("lag", l)
    
    combined |>
      filter(!is.na(.data[[lag_col]])) |>
      mutate(had_disaster = .data[[lag_col]] > min_events) |>
      filter(had_disaster) |>
      group_by(adm_1_name) |>
      summarise(
        lag                    = l,
        n_disaster_months      = n(),
        n_events_total         = sum(.data[[lag_col]], na.rm = TRUE),
        n_followed_by_outbreak = sum(outbreak, na.rm = TRUE),
        pct_followed_by_outbreak = round(
          100 * n_followed_by_outbreak / n_disaster_months, 1
        ),
        .groups = "drop"
      )
  }) |>
    arrange(adm_1_name, lag)
}

# Usage:
results_floods <- calc_outbreak_after_disaster_10("inunda")
results_urb_fl <- calc_outbreak_after_disaster_10("alaga")
results_drought <- calc_outbreak_after_disaster_10("seca")
results_movement <- calc_outbreak_after_disaster_10("mass")
results_flash_fl <- calc_outbreak_after_disaster_10("enx")
results_all    <- calc_outbreak_after_disaster_10()

# Filter to a minimum number of disaster events for reliability
results_floods_min10 <- calc_outbreak_after_disaster("inunda", min_events = 10)

# View results for a specific state
results_floods |> filter(adm_1_name == "SAO PAULO")

# Summary across all states by lag
floods_nat_10 <- results_floods |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1)
  )

floods_nat_10 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)


#for droughts
drought_nat_10 <- results_drought |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

drought_nat_10 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

#for urb floods
urb_fl_nat_10 <- results_urb_fl |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

urb_fl_nat_10 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

#flash floods
flash_fl_nat_10 <- results_flash_fl |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

flash_fl_nat_10 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)


#movement
move_nat_10 <- results_movement |>
  group_by(lag) |>
  summarise(
    total_disaster_months      = sum(n_disaster_months),
    total_events               = sum(n_events_total),
    total_followed_by_outbreak = sum(n_followed_by_outbreak),
    pct_followed_by_outbreak   = round(
      100 * total_followed_by_outbreak / total_disaster_months, 1
    )
  )

move_nat_10 |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)



# Calculate results for each type and combine
types <- c("inunda", "seca", "alaga", "enxu")


combined_results_10 <- map_dfr(types, function(t) {
  calc_outbreak_after_disaster_10(t) |>
    group_by(lag) |>
    summarise(
      pct_followed_by_outbreak = round(
        100 * sum(n_followed_by_outbreak) / sum(n_disaster_months), 1
      ),
      .groups = "drop"
    ) |>
    mutate(disaster_type = t)
})

ggplot(combined_results_10, aes(x = lag, y = pct_followed_by_outbreak,
                             colour = disaster_type, group = disaster_type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_label(aes(label = paste0(pct_followed_by_outbreak, "%")),
             nudge_y = 1.5, size = 2.5, show.legend = FALSE) +
  scale_x_continuous(breaks = 0:5, labels = paste0("Lag ", 0:5)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Percentage of disaster months followed by a dengue outbreak by disaster type (10 mean)",
    x        = "Lag (months)",
    y        = "% followed by outbreak",
    colour   = "Disaster type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )
