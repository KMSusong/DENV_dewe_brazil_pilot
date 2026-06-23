#' --- 
#' title: "02 calculating cross seasonal monthly mean" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-20" 
#' --- 

#' Overview: 
#'  calculate a cross seasonal monthly mean and from that 
#'  an outbreak threshold; a function to graph the dengue cases showing
#'  the outbreak threshold and means

#' Timeline: 
#'   2025-06-20

library(tidyverse)
install.packages("slider")
library(slider)
merged <- read_csv("00_Data/merged_week_month.csv")

dengue_monthly_mean <- merged |>
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
      .complete = FALSE  # return NA if fewer than 5 years available
    )
  ) |>
  ungroup()

# Calculate monthly mean + SD across previous 5 years
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


# Plot function: one state at a time
plot_outbreak_threshold <- function(state, data = dengue_monthly_stats) {
  
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

# Usage:
plot_outbreak_threshold("SAO PAULO")
plot_outbreak_threshold("RIO DE JANEIRO")
# How many outbreak months per state per year?
dengue_monthly_stats |>
  filter(outbreak) |>
  count(adm_1_name, join_year)
