#' --- 
#' title: "02 Merge all datasets for regression " 
#' author: "Natalia Cuartas" 
#' date: "2026-07-06" 
#' --- 

#' Overview: 
#'  merge disaster, climate, and outbreak data in preparation for regression,
#'  at the admin 2 level;save as regression_data

#' Timeline: 
#'   2026-07-06 

library(tidyverse)
adm2_outbreak <-read_csv("00_Data/adm2_outbreak_data.csv")
climate_monthly <- read_csv("00_Data/climate_monthly.csv")
merged <- read_csv("00_Data/merged_week_month.csv")
disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")
#extract outbreak collumns 
outbreak_flags <- adm2_outbreak |>
  mutate(IBGE_code = as.character(IBGE_code)) |>
  select(IBGE_code, join_year, join_month,
         mean_prev_5yr, sd_prev_5yr, threshold, outbreak)

#join to merged dataset
regression_data <- merged |>
  mutate(IBGE_code = as.character(IBGE_code)) |>
  left_join(outbreak_flags,
            by = c("IBGE_code", "join_year", "join_month")) |>
  filter(!is.na(outbreak)) |>
  arrange(IBGE_code, join_year, join_month)

#add disaster lags

regression_data <- regression_data |>
  group_by(IBGE_code) |>
  mutate(
    pub_water_lag1            = lag(pub_pot_water_supply_sum, 1),
    pub_water_lag2            = lag(pub_pot_water_supply_sum, 2),
    pub_sewage_lag1           = lag(pub_sewage_sum,        1),
    pub_vector_lag1           = lag(pub_vector_control_sum,1),
    pub_total_lag1            = lag(pub_total_sum,         1),
    pub_total_lag2            = lag(pub_total_sum,         2),
    pub_total_lag3            = lag(pub_total_sum,         3),
    priv_total_lag1           = lag(priv_total_sum,        1),
    priv_total_lag2           = lag(priv_total_sum,        2),
    priv_total_lag3           = lag(priv_total_sum,        3),
    priv_pub_lag1             = lag(priv_pub_sum,          1),
    priv_pub_lag2             = lag(priv_pub_sum,          2),
    priv_pub_lag3             = lag(priv_pub_sum,          3),
    flag_water_deplete_lag1   = lag(flag_water_deplete,    1),
    flag_water_deplete_lag2   = lag(flag_water_deplete,    2),
    flag_water_deplete_lag3   = lag(flag_water_deplete,    3),
    flag_water_contam_lag1    = lag(flag_water_contam,     1),
    flag_water_contam_lag2    = lag(flag_water_contam,     2),
    flag_water_contam_lag3    = lag(flag_water_contam,     3),
    dm_pub_infra_lag1         = lag(dm_pub_infra_damaged_sum, 1),
    dh_displaced_lag1         = lag(dh_displaced_sum,      1),
    dh_displaced_lag2         = lag(dh_displaced_sum,      2),
    dh_displaced_lag3         = lag(dh_displaced_sum,      3),
    dh_homeless_lag1          = lag(dh_homeless_sum,       1),
    dh_homeless_lag2          = lag(dh_homeless_sum,       2),
    dh_homeless_lag3          = lag(dh_homeless_sum,       3)
  ) |>
  ungroup()

#separate disaster types, make all NA's zeros
regression_data <- regression_data |>
  mutate(
    n_inundacoes    = replace_na(str_count(tolower(tipologias), "inunda"),    0),
    n_seca          = replace_na(str_count(tolower(tipologias), "seca"),      0),
    n_chuvas        = replace_na(str_count(tolower(tipologias), "chuva"),     0),
    n_enxurradas    = replace_na(str_count(tolower(tipologias), "enxurr"),    0),
    n_alagamentos   = replace_na(str_count(tolower(tipologias), "alagam"),    0),
    n_massa = replace_na(str_count(tolower(tipologias), "massa"),     0),
    n_vendavais     = replace_na(str_count(tolower(tipologias), "vendav"),    0),
    n_granizo       = replace_na(str_count(tolower(tipologias), "granizo"),   0),
    n_tornado       = replace_na(str_count(tolower(tipologias), "tornado"),   0),
    n_incendio      = replace_na(str_count(tolower(tipologias), "incendio|incêndio"), 0)
  )

#add lags
regression_data <- regression_data |>
  arrange(IBGE_code, join_year, join_month) |>
  group_by(IBGE_code) |>
  mutate(
    n_inunda_lag1    = lag(n_inundacoes,    1),
    n_inunda_lag2    = lag(n_inundacoes,    2),
    n_inunda_lag3    = lag(n_inundacoes,    3),
    n_inunda_lag4    = lag(n_inundacoes,    4),
    n_inunda_lag5    = lag(n_inundacoes,    5),
    n_seca_lag1          = lag(n_seca,          1),
    n_seca_lag2          = lag(n_seca,          2),
    n_seca_lag3          = lag(n_seca,          3),
    n_seca_lag4          = lag(n_seca,          4),
    n_seca_lag5          = lag(n_seca,          5),
    n_chuvas_lag1        = lag(n_chuvas,        1),
    n_chuvas_lag2        = lag(n_chuvas,        2),
    n_chuvas_lag3        = lag(n_chuvas,        3),
    n_chuvas_lag4        = lag(n_chuvas,        4),
    n_chuvas_lag5        = lag(n_chuvas,        5),
    n_enxu_lag1    = lag(n_enxurradas,    1),
    n_enxu_lag2    = lag(n_enxurradas,    2),
    n_enxu_lag3    = lag(n_enxurradas,    3),
    n_enxu_lag4    = lag(n_enxurradas,    4),
    n_alaga_lag1   = lag(n_alagamentos,   1),
    n_alaga_lag2   = lag(n_alagamentos,   2),
    n_alaga_lag3   = lag(n_alagamentos,   3),
    n_alaga_lag4   = lag(n_alagamentos,   4),
    n_alaga_lag5   = lag(n_alagamentos,   5),
    n_massa_lag1 = lag(n_massa, 1),
    n_massa_lag2 = lag(n_massa, 2),
    n_massa_lag3 = lag(n_massa, 3),
    n_massa_lag4 = lag(n_massa, 4),
    n_massa_lag5 = lag(n_massa, 5)
  ) |>
  ungroup()

# add climate data
if (exists("climate_monthly")) {
  regression_data <- regression_data |>
    left_join(
      climate_monthly |> mutate(IBGE_code = as.character(IBGE_code)),
      by = c("IBGE_code", "join_year", "join_month")
    ) |>
    group_by(IBGE_code) |>
    mutate(
      pr_lag1   = lag(pr_sum,   1),
      pr_lag2   = lag(pr_sum,   2),
      pr_lag3   = lag(pr_sum,   3),
      tmax_lag1 = lag(tmax_mean, 1),
      tmax_lag2 = lag(tmax_mean, 2),
      tmax_lag3 = lag(tmax_mean, 3),
      tmin_lag1 = lag(tmin_mean, 1),
      tmin_lag2 = lag(tmin_mean, 2),
      tmin_lag3 = lag(tmin_mean, 3)
    ) |>
    ungroup()
}


#check data
cat("Dimensions:", nrow(regression_data), "x", ncol(regression_data), "\n")

cat("\nOutbreak distribution:\n")
print(table(regression_data$outbreak, useNA = "always"))

cat("\nDate range:\n")
cat("  From:", as.character(min(regression_data$calendar_start_date, na.rm = TRUE)), "\n")
cat("  To:  ", as.character(max(regression_data$calendar_start_date, na.rm = TRUE)), "\n")

cat("\nMissing values in key columns:\n")
regression_data |>
  select(outbreak, n_events, pub_total_sum, priv_total_sum,
         flag_water_deplete, dh_displaced_sum) |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  print()

#filter to only cover disaster data dates (up to end of 2023)
max(disasters_plot$date, na.rm = TRUE)

disaster_end <- max(disasters_plot$date, na.rm = TRUE)
#filter regression data to disaster dates:
regression_data <- regression_data |>
  filter(calendar_start_date <= disaster_end)

cat("Date range after filtering:\n")
cat("  From:", as.character(min(regression_data$calendar_start_date, na.rm = TRUE)), "\n")
cat("  To:  ", as.character(max(regression_data$calendar_start_date, na.rm = TRUE)), "\n")
cat("  Rows:", nrow(regression_data), "\n")



#make a standard deviation anomaly var 
regression_data <- regression_data |>
  mutate(
    # Standard deviation anomaly: how many SDs above/below the 5yr mean
    # Positive = above mean, negative = below mean
    sd_anomaly = (dengue_total - mean_prev_5yr) / sd_prev_5yr
  )

# Check distribution
summary(regression_data$sd_anomaly)
ggplot(regression_data |> filter(!is.na(sd_anomaly)), 
       aes(x = sd_anomaly)) +
  geom_histogram(bins = 50, fill = "#2166ac", alpha = 0.8) +
  geom_vline(xintercept = 1.25, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 0,    colour = "grey50", linetype = "dashed") +
  labs(
    title    = "Distribution of dengue SD anomaly",
    subtitle = "Red dashed line = outbreak threshold (1.25 SD)",
    x        = "Standard deviations from 5-year monthly mean",
    y        = "Count"
  ) +
  theme_bw()




######-------Save Data
write_csv(regression_data, "00_Data/regression_data_v4.csv")
cat("\nSaved: 00_Data/regression_data_v4.csv\n")


