##' --- 
#' title: "02 create time lags and calculate correlations with different
#' disaster types" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-19" 
#' --- 

#' Overview: 
#'   create time lags for 1-5 months and calculate correlations with different
#' disaster types; graph the correlations by state on a heat map table

#' Timeline: 
#'   2026-06-19
#' 



library(tidyverse)
library(lubridate)
library(slider)

merged <- read_csv("00_Data/merged_week_month.csv")
disasters <- read_csv("00_Data/nat_dis_bra.csv")

#filter disasters for plotting
disasters_plot <- disasters |>
  mutate(
    cod_ibge_mun        = as.character(cod_ibge_mun),
    date                = as.Date(data_evento),
    descricao_tipologia = as.character(descricao_tipologia)
  ) |>
  filter(!is.na(date), !is.na(descricao_tipologia),
         descricao_tipologia != "") |>
  left_join(adm1_lookup, by = c("cod_ibge_mun" = "IBGE_code")) |>
  filter(!is.na(adm_1_name)) |>
  # Keep only dates within the dengue date range
  semi_join(dengue_adm1 |> distinct(adm_1_name),
            by = "adm_1_name") |>
  filter(date >= min(dengue_adm1$date),
         date <= max(dengue_adm1$date))


#calculate dengue monthly stats

dengue_monthly_stats <- merged |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(adm_1_name, join_month, join_year) |>
  group_by(adm_1_name, join_month) |>
  mutate(
    mean_prev_5yr = slide_dbl(dengue_total, mean, .before = 5, .after = 0,
                              .complete = FALSE),
    sd_prev_5yr   = slide_dbl(dengue_total, sd,   .before = 5, .after = 0,
                              .complete = FALSE),
    threshold     = mean_prev_5yr + 1.25 * sd_prev_5yr,
    outbreak      = dengue_total > threshold
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))


calc_lag_cors <- function(search_term = NULL,
                          data        = disasters_plot,
                          min_months  = 0,
                          n_lags      = 5) {
  
  # --- 3a. Filter to disaster type if specified ---
  dis <- data
  if (!is.null(search_term)) {
    dis <- dis |>
      filter(str_detect(tolower(descricao_tipologia), tolower(search_term)))
    cat("Disaster type filter:", search_term, "\n")
    cat("Matching rows in disasters_plot:", nrow(dis), "\n")
  }
  
  # --- 3b. Aggregate filtered disasters to state-month ---
  dis_monthly <- dis |>
    filter(!is.na(adm_1_name)) |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(adm_1_name, join_year, join_month) |>
    summarise(n_events = n(), .groups = "drop")
  
  # --- 3c. Join to dengue monthly stats and calculate lags ---
  dengue_lagged <- dengue_monthly_stats |>
    left_join(dis_monthly,
              by = c("adm_1_name", "join_year", "join_month")) |>
    mutate(n_events = replace_na(n_events, 0)) |>
    arrange(adm_1_name, join_year, join_month) |>
    group_by(adm_1_name) |>
    mutate(across(
      n_events,
      list(
        lag1 = ~ lag(.x, 1),
        lag2 = ~ lag(.x, 2),
        lag3 = ~ lag(.x, 3),
        lag4 = ~ lag(.x, 4),
        lag5 = ~ lag(.x, 5)
      ),
      .names = "{.fn}"
    )[seq_len(n_lags)]) |>
    ungroup()
  
  # Re-derive lag columns dynamically based on n_lags
  lag_cols <- paste0("lag", seq_len(n_lags))
  
  # --- 3d. Calculate Spearman correlations per state ---
  cors <- dengue_lagged |>
    filter(!is.na(adm_1_name)) |>
    group_by(adm_1_name) |>
    summarise(
      # Lag 0 (concurrent)
      cor_lag0 = suppressWarnings(
        cor(dengue_total, n_events, use = "complete.obs", method = "spearman")
      ),
      # Lags 1-5
      across(
        all_of(lag_cols),
        ~ suppressWarnings(
          cor(dengue_total, .x, use = "complete.obs", method = "spearman")
        ),
        .names = "cor_{.col}"
      ),
      # Count months with at least one event (for filtering)
      n_event_months = sum(n_events > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- 3e. Apply minimum event month filter ---
  if (min_months > 0) {
    n_before <- nrow(cors)
    cors <- cors |> filter(n_event_months >= min_months)
    cat(sprintf("States removed (< %d event months): %d\n",
                min_months, n_before - nrow(cors)))
  }
  
  cat("States retained:", nrow(cors), "\n\n")
  
  # --- 3f. Pivot to long format ---
  cors |>
    pivot_longer(
      cols      = starts_with("cor_lag"),
      names_to  = "lag",
      values_to = "correlation"
    ) |>
    mutate(lag = as.integer(str_extract(lag, "[0-9]+")))
}

# =============================================================================
# 4. PLOT FUNCTIONS
# =============================================================================

# --- 4a. Single state: correlation by lag ---
plot_lag_cor <- function(state, data = lag_cors, title_suffix = "") {
  
  d <- data |> filter(adm_1_name == state)
  
  if (nrow(d) == 0) stop("State not found: ", state)
  
  ggplot(d, aes(x = lag, y = correlation)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = 0, ymax = correlation),
                fill = "#2166ac", alpha = 0.15) +
    geom_line(colour = "#2166ac", linewidth = 0.8) +
    geom_point(size = 3, colour = "#2166ac") +
    geom_label(aes(label = round(correlation, 2)),
               nudge_y = 0.04, size = 3) +
    scale_x_continuous(breaks = 0:max(data$lag),
                       labels = paste0("Lag ", 0:max(data$lag))) +
    ylim(-1, 1) +
    labs(
      title    = paste0("Spearman correlation: disaster events vs dengue — ",
                        str_to_title(state),
                        if (title_suffix != "") paste0(" (", title_suffix, ")") else ""),
      subtitle = "Lag 0 = concurrent month; Lag 1 = disasters 1 month prior, etc.",
      x        = "Lag (months)",
      y        = "Spearman correlation"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

# --- 4b. All states: heatmap of correlations by lag ---
plot_lag_heatmap <- function(data = lag_cors, title_suffix = "") {
  
  # Order states by their peak absolute correlation across lags
  state_order <- data |>
    group_by(adm_1_name) |>
    summarise(max_abs_cor = max(abs(correlation), na.rm = TRUE)) |>
    arrange(desc(max_abs_cor)) |>
    pull(adm_1_name)
  
  data |>
    mutate(adm_1_name = factor(adm_1_name, levels = rev(state_order))) |>
    ggplot(aes(x = lag, y = adm_1_name, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(is.na(correlation), "NA",
                                 sprintf("%.2f", correlation))),
              size = 2.5) +
    scale_fill_gradient2(
      low      = "#d73027",
      mid      = "white",
      high     = "#1a9850",
      midpoint = 0,
      limits   = c(-1, 1),
      na.value = "grey80",
      name     = "Spearman r"
    ) +
    scale_x_continuous(breaks = unique(data$lag),
                       labels = paste0("Lag ", unique(data$lag))) +
    labs(
      title    = paste0("Spearman correlation: disaster events vs dengue by state and lag",
                        if (title_suffix != "") paste0("\n", title_suffix) else ""),
      subtitle = "States ordered by peak absolute correlation. Grey = insufficient data.",
      x        = "Lag (months)",
      y        = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid  = element_blank(),
      axis.text.y = element_text(size = 8)
    )
}

# =============================================================================
# 5. USAGE EXAMPLES
# =============================================================================

# All disaster types
lag_cors <- calc_lag_cors()

# Specific typologies (partial string match, case-insensitive)
lag_cors_floods    <- calc_lag_cors("inunda")
lag_cors_drought   <- calc_lag_cors("seca")
lag_cors_movement <- calc_lag_cors("massa")
lag_cors_urb_flood    <- calc_lag_cors("alagam")

# With minimum event month filter (exclude states with <12 months of events)
lag_cors_floods_filtered <- calc_lag_cors("inunda", min_months = 12)
lag_cors_urb_flood_filtered <- calc_lag_cors("alagam", min_months = 12)
lag_cors_drought_filtered <- calc_lag_cors("seca", min_months = 12)
# --- Single state plots ---
plot_lag_cor("SAO PAULO", data = lag_cors_floods,  title_suffix = "Floods")
plot_lag_cor("SAO PAULO",  data = lag_cors_drought, title_suffix = "Drought")
plot_lag_cor("SAO PAULO",  data = lag_cors_urb_flood, title_suffix = "Urban Floods")

# --- Heatmap across all states ---
plot_lag_heatmap(data = lag_cors_floods,          title_suffix = "Floods (Inundações)")
plot_lag_heatmap(data = lag_cors_floods_filtered, title_suffix = "Floods — states with ≥12 event months")
plot_lag_heatmap(data = lag_cors_urb_flood_filtered, title_suffix = "Urban Floods — states with ≥12 event months")
plot_lag_heatmap(data = lag_cors_drought_filtered, title_suffix = "Droughts — states with ≥12 event months")
# --- Save heatmap ---
ggsave("outputs/lag_cor_heatmap_floods.png",
       plot_lag_heatmap(lag_cors_floods, "Floods"),
       width = 10, height = 10, dpi = 150)







