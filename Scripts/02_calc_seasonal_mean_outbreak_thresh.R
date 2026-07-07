#' --- 
#' title: "02 calculating cross seasonal monthly mean" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-20" 
#' --- 

#' Overview: 
#'  calculate a cross seasonal monthly mean and from that 
#'  an outbreak threshold; to graph the dengue cases showing
#'  the outbreak threshold and means; save the outbreak data as a binary true/
#'  false

#' Timeline: 
#'   2026-06-20
#'   2026-06-25 - added mean calculations for different year amounts (7-10)
#'   2026-07-03 - add calculation for outbreak at admin 2 level
#'  

library(tidyverse)
install.packages("slider")
library(slider)
merged <- read_csv("00_Data/merged_week_month.csv")

#mean using past 5 years of data
dengue_monthly_mean_5 <- merged |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(adm_1_name, join_month, join_year) |>
  group_by(adm_1_name, join_month) |>
  mutate(
    # Mean of the same calendar month over the previous 5 years
    # 
    mean_prev_5yr = slide_dbl(
      dengue_total,
      mean,
      .before = 5,  # look back 5 steps (years)
      .after  = 0,
      .complete = TRUE  # return NA if fewer than 5 years available
    )
  ) |>
  ungroup()

# Calculate monthly mean + SD across previous 5 years
dengue_monthly_stats_5 <- merged |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(adm_1_name, join_month, join_year) |>
  group_by(adm_1_name, join_month) |>
  mutate(
    mean_prev_5yr = slide_dbl(dengue_total, mean, .before = 5, .after = 0,
                              .complete = TRUE),
    sd_prev_5yr   = slide_dbl(dengue_total, sd,   .before = 5, .after = 0,
                              .complete = TRUE),
    threshold     = mean_prev_5yr + 1.25 * sd_prev_5yr,
    outbreak      = dengue_total > threshold
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))






# Calculate monthly mean + SD across previous 7 years
dengue_monthly_stats_7 <- merged |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(adm_1_name, join_month, join_year) |>
  group_by(adm_1_name, join_month) |>
  mutate(
    mean_prev_7yr = slide_dbl(dengue_total, mean, .before = 7, .after = 0,
                              .complete = TRUE),
    sd_prev_7yr   = slide_dbl(dengue_total, sd,   .before = 7, .after = 0,
                              .complete = TRUE),
    threshold     = mean_prev_7yr + 1.25 * sd_prev_7yr,
    outbreak      = dengue_total > threshold
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))



#across previous 10 years
dengue_monthly_stats_10 <- merged |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(adm_1_name, join_month, join_year) |>
  group_by(adm_1_name, join_month) |>
  mutate(
    mean_prev_10yr = slide_dbl(dengue_total, mean, .before = 10, .after = 0,
                              .complete = TRUE),
    sd_prev_10yr   = slide_dbl(dengue_total, sd,   .before = 10, .after = 0,
                              .complete = TRUE),
    threshold     = mean_prev_10yr + 1.25 * sd_prev_10yr,
    outbreak      = dengue_total > threshold
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))




# Plot function: one state at a time
plot_outbreak_threshold_5 <- function(state, data = dengue_monthly_stats) {
  
  d <- data |> filter(adm_1_name == state, !is.na(mean_prev_5yr))
  
  ggplot(d, aes(x = date)) +
    # Ribbon showing mean +/- 1.25 SD
    geom_ribbon(aes(ymin = mean_prev_5yr, ymax = threshold),
                fill = "#FDD0A2", alpha = 0.6) +
    # Threshold line
    geom_line(aes(y = threshold),
              colour = "#E6550D", linewidth = 0.7, linetype = "dashed") +
    # Mean line
    geom_line(aes(y = mean_prev_5yr),
              colour = "#3182BD", linewidth = 0.7) +
    # Actual cases
    geom_line(aes(y = dengue_total),
              colour = "black", linewidth = 0.5) +
    # Highlight outbreak months
    geom_point(data = filter(d, outbreak),
               aes(y = dengue_total),
               colour = "red", size = 1.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste("Dengue outbreak threshold —", str_to_title(state)),
      subtitle = "Black = observed  |  Blue = 5-yr mean  |  Red dashed = mean + 1.25 SD  |  Red points = outbreak",
      x        = NULL,
      y        = "Dengue cases"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}



#plot outbreak threshold 7 years
plot_outbreak_threshold_7 <- function(state, data = dengue_monthly_stats_7) {
  
  d <- data |> filter(adm_1_name == state, !is.na(mean_prev_7yr))
  
  ggplot(d, aes(x = date)) +
    # Ribbon showing mean +/- 1.25 SD
    geom_ribbon(aes(ymin = mean_prev_7yr, ymax = threshold),
                fill = "#FDD0A2", alpha = 0.6) +
    # Threshold line
    geom_line(aes(y = threshold),
              colour = "#E6550D", linewidth = 0.7, linetype = "dashed") +
    # Mean line
    geom_line(aes(y = mean_prev_7yr),
              colour = "#3182BD", linewidth = 0.7) +
    # Actual cases
    geom_line(aes(y = dengue_total),
              colour = "black", linewidth = 0.5) +
    # Highlight outbreak months
    geom_point(data = filter(d, outbreak),
               aes(y = dengue_total),
               colour = "red", size = 1.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste("Dengue outbreak threshold —", str_to_title(state)),
      subtitle = "Black = observed  |  Blue = 7-yr mean  |  Red dashed = mean + 1.25 SD  |  Red points = outbreak",
      x        = NULL,
      y        = "Dengue cases"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

#plot outbreak threshold 10 years
plot_outbreak_threshold_10 <- function(state, data = dengue_monthly_stats_10) {
  
  d <- data |> filter(adm_1_name == state, !is.na(mean_prev_10yr))
  
  ggplot(d, aes(x = date)) +
    # Ribbon showing mean +/- 1.25 SD
    geom_ribbon(aes(ymin = mean_prev_10yr, ymax = threshold),
                fill = "#FDD0A2", alpha = 0.6) +
    # Threshold line
    geom_line(aes(y = threshold),
              colour = "#E6550D", linewidth = 0.7, linetype = "dashed") +
    # Mean line
    geom_line(aes(y = mean_prev_10yr),
              colour = "#3182BD", linewidth = 0.7) +
    # Actual cases
    geom_line(aes(y = dengue_total),
              colour = "black", linewidth = 0.5) +
    # Highlight outbreak months
    geom_point(data = filter(d, outbreak),
               aes(y = dengue_total),
               colour = "red", size = 1.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste("Dengue outbreak threshold —", str_to_title(state)),
      subtitle = "Black = observed  |  Blue = 10-yr mean  |  Red dashed = mean + 1.25 SD  |  Red points = outbreak",
      x        = NULL,
      y        = "Dengue cases"
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}




# Usage:
plot_outbreak_threshold_5("SAO PAULO")
plot_outbreak_threshold_5("RIO DE JANEIRO")

plot_outbreak_threshold_7("SAO PAULO")
plot_outbreak_threshold_7("RIO DE JANEIRO")

plot_outbreak_threshold_10("SAO PAULO")
plot_outbreak_threshold_10("RIO DE JANEIRO")

# How many outbreak months per state per year?
dengue_monthly_stats_5 |>
  filter(outbreak) |>
  count(adm_1_name, join_year)


#save the outbreak count data:
write_csv(dengue_monthly_stats_5, "00_Data/5_year_outbreak_data.csv")
write_csv(dengue_monthly_stats_7, "00_Data/7_year_outbreak_data.csv")
write_csv(dengue_monthly_stats_10, "00_Data/10_year_outbreak_data.csv")



#calc outbreak threshold with 5 year mean using Admin 2 level data:
#set min case for threshold, 10 for now
dengue_monthly_stats_adm2 <- merged |>
  filter(!is.na(IBGE_code), !is.na(adm_2_name)) |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  arrange(IBGE_code, join_month, join_year) |>
  group_by(IBGE_code, join_month) |>
  mutate(
    mean_prev_5yr = slide_dbl(dengue_total, mean, .before = 5, .after = 0,
                              .complete = TRUE),
    sd_prev_5yr   = slide_dbl(dengue_total, sd,   .before = 5, .after = 0,
                              .complete = TRUE),
    threshold     = mean_prev_5yr + 1.25 * sd_prev_5yr,
    # Explicitly return NA when threshold cannot be calculated
    outbreak      = case_when(
      is.na(threshold)    ~ NA,
      dengue_total < 10   ~ FALSE,
      dengue_total > threshold ~ TRUE,
      TRUE                ~ FALSE
    )
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))

#check data sparcity:
dengue_monthly_stats_adm2 |>
  group_by(IBGE_code, adm_2_name) |>
  summarise(
    n_months_with_threshold = sum(!is.na(threshold)),
    n_outbreak_months       = sum(outbreak, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(n_months_with_threshold) |>
  print(n = 50)

# Flag municipalities with very low baselines
dengue_monthly_stats_adm2 |>
  group_by(IBGE_code, adm_2_name) |>
  summarise(mean_dengue = mean(dengue_total, na.rm = TRUE), .groups = "drop") |>
  filter(mean_dengue < 5) |>
  nrow()


write_csv(dengue_monthly_stats_adm2, "00_Data/adm2_outbreak_data.csv")



#check adm2 data
dengue_monthly_stats_adm2 |>
  count(outbreak)


