#' --- 
#' title: "02 Outbreak summary statistics by municipality and state" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-03" 
#' --- 

#' Overview: calculate how many outbreaks per state, the municipalities with
#' the highest number of outbreaks, the percentage of brazil's municipalities
#' with outbreaks, the variation of outbreaks across years and season, and
#' the average magnitude of outbreaks
#'  
#'

#' Timeline: 
#'   2026-08-04
#'   
library(tidyverse)
regression_data <-read_csv("00_Data/regression_data_v4.csv")

#outbreaks per state 

outbreaks_by_state <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    n_outbreak_months     = sum(outbreak, na.rm = TRUE),
    n_total_months        = n(),
    pct_outbreak_months   = round(100 * n_outbreak_months / n_total_months, 1),
    n_municipalities      = n_distinct(IBGE_code),
    n_mun_with_outbreak   = n_distinct(IBGE_code[outbreak == TRUE]),
    pct_mun_with_outbreak = round(100 * n_mun_with_outbreak / n_municipalities, 1),
    # Magnitude: mean dengue cases in outbreak months
    mean_cases_outbreak   = round(mean(dengue_total[outbreak == TRUE],  na.rm = TRUE), 1),
    median_cases_outbreak = median(dengue_total[outbreak == TRUE],       na.rm = TRUE),
    max_cases_outbreak    = max(dengue_total[outbreak == TRUE],          na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n_outbreak_months))

cat("=== Outbreaks by state ===\n")
print(outbreaks_by_state, n = 27)

#top municipalities by outbreak months

outbreaks_by_mun <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  summarise(
    n_outbreak_months     = sum(outbreak,  na.rm = TRUE),
    n_total_months        = n(),
    pct_outbreak_months   = round(100 * n_outbreak_months / n_total_months, 1),
    # Magnitude
    mean_cases_outbreak   = round(mean(dengue_total[outbreak == TRUE],   na.rm = TRUE), 1),
    median_cases_outbreak = median(dengue_total[outbreak == TRUE],        na.rm = TRUE),
    max_cases_outbreak    = max(dengue_total[outbreak == TRUE],           na.rm = TRUE),
    # SD anomaly in outbreak months
    mean_sd_anomaly       = round(mean(sd_anomaly[outbreak == TRUE],      na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(desc(n_outbreak_months))

cat("\n=== Top 20 municipalities by number of outbreak months ===\n")
print(outbreaks_by_mun |> head(20), n = 20)

cat("\n=== Top 20 municipalities by % of months in outbreak ===\n")
outbreaks_by_mun |>
  filter(n_total_months >= 12) |>   # exclude municipalities with very short records
  arrange(desc(pct_outbreak_months)) |>
  head(20) |>
  print(n = 20)

####----percent of municipalities with outbreaks---####

cat("\n=== National outbreak coverage ===\n")

regression_data |>
  filter(!is.na(outbreak)) |>
  summarise(
    n_municipalities_total      = n_distinct(IBGE_code),
    n_municipalities_outbreak   = n_distinct(IBGE_code[outbreak == TRUE]),
    pct_municipalities_outbreak = round(
      100 * n_municipalities_outbreak / n_municipalities_total, 1
    ),
    total_outbreak_months       = sum(outbreak, na.rm = TRUE),
    total_months                = n(),
    pct_months_outbreak         = round(100 * total_outbreak_months / total_months, 1)
  ) |>
  print()

####----Dengue variation across years----####

outbreaks_by_year <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(join_year) |>
  summarise(
    n_outbreak_months       = sum(outbreak,    na.rm = TRUE),
    n_total_months          = n(),
    pct_outbreak            = round(100 * n_outbreak_months / n_total_months, 1),
    n_mun_with_outbreak     = n_distinct(IBGE_code[outbreak == TRUE]),
    n_mun_total             = n_distinct(IBGE_code),
    pct_mun_outbreak        = round(100 * n_mun_with_outbreak / n_mun_total, 1),
    mean_cases_in_outbreak  = round(mean(dengue_total[outbreak == TRUE], na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n=== Outbreak variation by year ===\n")
print(outbreaks_by_year, n = Inf)

# Plot year variation
ggplot(outbreaks_by_year, aes(x = join_year, y = pct_mun_outbreak)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_line(aes(y = pct_outbreak), colour = "red", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(outbreaks_by_year$join_year),
                                  max(outbreaks_by_year$join_year), by = 2)) +
  labs(
    title    = "Annual Variation in Dengue Outbreaks",
    subtitle = "Bars = % municipalities with ≥1 outbreak month | Red line = % of all municipality-months in outbreak",
    x        = NULL,
    y        = "Percentage (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####-----Magnitude of outbreaks----####
cat("\n=== Outbreak magnitude (national) ===\n")

 regression_data |>
  filter(!is.na(outbreak)) |>
  summarise(
    # Absolute case counts
    mean_cases_outbreak      = round(mean(dengue_total[outbreak == TRUE],   na.rm = TRUE), 1),
    median_cases_outbreak    = median(dengue_total[outbreak == TRUE],        na.rm = TRUE),
    sd_cases_outbreak        = round(sd(dengue_total[outbreak == TRUE],     na.rm = TRUE), 1),
    max_cases_outbreak       = max(dengue_total[outbreak == TRUE],           na.rm = TRUE),
    # SD anomaly (how far above the threshold)
    mean_sd_anomaly_outbreak = round(mean(sd_anomaly[outbreak == TRUE],     na.rm = TRUE), 2),
    median_sd_anomaly_outbreak = median(sd_anomaly[outbreak == TRUE],        na.rm = TRUE),
    max_sd_anomaly_outbreak  = round(max(sd_anomaly[outbreak == TRUE],      na.rm = TRUE), 2),
    # Non-outbreak months for comparison
    mean_cases_non_outbreak  = round(mean(dengue_total[outbreak == FALSE],  na.rm = TRUE), 1),
    median_cases_non_outbreak = median(dengue_total[outbreak == FALSE],      na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "metric", values_to = "value") |>
  print(n = Inf)

# Distribution of outbreak magnitude
ggplot(regression_data |> filter(outbreak == TRUE), 
       aes(x = join_year, y= mean_cases_outbreak)) +
  geom_histogram(bins = 50, fill = "#d6604d", alpha = 0.8) +
  geom_vline(xintercept = 1.25, linetype = "dashed", colour = "black") +
  labs(
    title    = "Distribution of outbreak magnitude (SD anomaly)",
    subtitle = "Only outbreak months shown. Dashed line = threshold (1.25 SD)",
    x        = "SD anomaly (SDs above 5-year monthly mean)",
    y        = "Count"
  ) +
  theme_bw()

######----Seasonal timing of outbreaks----####

outbreaks_by_month <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(season_month) |>
  summarise(
    n_outbreak_months     = sum(outbreak,  na.rm = TRUE),
    n_total_months        = n(),
    mean_dengue           = round(mean(dengue_total)),
    pct_outbreak          = round(100 * n_outbreak_months / n_total_months, 1),
    mean_cases_outbreak   = round(mean(dengue_total[outbreak == TRUE],  na.rm = TRUE), 1),
    mean_sd_anomaly       = round(mean(sd_anomaly[outbreak == TRUE],    na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(season_month)

cat("\n=== Seasonal distribution of outbreaks (epi month 1 = October) ===\n")
print(outbreaks_by_month, n = 12)

# Plot seasonal pattern of outbreaks
ggplot(outbreaks_by_month, aes(x = season_month, y = pct_outbreak)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                "Apr", "May", "Jun", "Jul", "Aug", "Sep")
                     ) +
  labs(
    title    = "Seasonal distribution of dengue outbreaks 2006 - 2023",
    x        = "Epidemiological month (October = 1)",
    y        = "% of municipality-months in outbreak",
    
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

# Combined graph of pct outbreak and mean case number in outbreak
range_cases <- range(outbreaks_by_month$mean_dengue, na.rm = TRUE)
range_pct   <- range(outbreaks_by_month$pct_outbreak, na.rm = TRUE)

scale_factor <- diff(range_cases) / diff(range_pct)
offset <- range_cases[1] - range_pct[1] * scale_factor

bar_col  <- "#2C7FB8"   # blue
line_col <- "#D95F02"   # orange

bar_col  <- "#2C7FB8"   # blue
line_col <- "#D95F02"   # orange

ggplot(outbreaks_by_month,
       aes(x = season_month)) +
  geom_col(aes(y = mean_dengue),
           fill = bar_col,
           alpha = 0.8) +
  geom_line(aes(y = pct_outbreak * scale_factor + offset),
            colour = line_col,
            linewidth = 0.8,
            group = 1) +
  scale_y_continuous(
    name = "Mean # of Dengue Cases per Month",
    sec.axis = sec_axis(
      ~ (. - offset) / scale_factor,
      name = "% of Municipality-Months in Outbreak"
    )
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
               "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  ) +
  labs(
    title = "Seasonal Distribution of Dengue Outbreaks 2006-2023",
    subtitle = "Bars = mean number of dengue cases per month | Line = % of municipality-months in outbreak",
    x = "Epidemiological month (October = 1)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(color = bar_col),
    axis.text.y = element_text(color = bar_col),
    axis.title.y.right = element_text(color = line_col),
    axis.text.y.right = element_text(color = line_col)
  )


#####-----save

write_csv(outbreaks_by_state, "Results/outbreaks_by_state.csv")
write_csv(outbreaks_by_mun,   "Results/outbreaks_by_municipality.csv")
write_csv(outbreaks_by_year,  "Results/outbreaks_by_year.csv")
write_csv(outbreaks_by_month, "Results/outbreaks_by_season.csv")

cat("\nSaved outbreak summary tables to outputs/\n")
