#' --- 
#' title: "02 Summary statistics by municipality" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-03" 
#' --- 

#' Overview: calculate municipalities with highest disaster and dengue burden
#'  
#'

#' Timeline: 
#'   2026-08-04
#'   
library(tidyverse)
regression_data <-read_csv("00_Data/regression_data_v4.csv")


#municipalities with greates dengue burden

dengue_by_mun <- regression_data |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  summarise(
    total_cases     = sum(dengue_total,  na.rm = TRUE),
    mean_cases      = round(mean(dengue_total, na.rm = TRUE), 1),
    median_cases    = median(dengue_total, na.rm = TRUE),
    max_cases       = max(dengue_total,  na.rm = TRUE),
    n_months        = n(),
    n_outbreak_months = sum(outbreak, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_cases))

cat("=== Top 20 municipalities by total dengue cases ===\n")
print(dengue_by_mun |> head(20), n = 20)

#municipalities with greatest disaster burden

disaster_by_mun <- regression_data |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  summarise(
    total_events        = sum(n_events,         na.rm = TRUE),
    total_inundacoes    = sum(n_inundacoes,      na.rm = TRUE),
    total_seca          = sum(n_seca,            na.rm = TRUE),
    total_chuvas        = sum(n_chuvas,          na.rm = TRUE),
    total_alagamentos   = sum(n_alagamentos,     na.rm = TRUE),
    total_massa         = sum(n_massa, na.rm = TRUE),

    .groups = "drop"
  )

cat("\n=== Top 20 municipalities by total disaster events ===\n")
disaster_by_mun |>
  arrange(desc(total_events)) |>
  head(20) |>
  print(n = 20)

cat("\n=== Top 10 municipalities by floods (inundações) ===\n")
disaster_by_mun |>
  arrange(desc(total_inundacoes)) |>
  head(10) |>
  select(adm_2_name, adm_1_name, sigla_uf, total_inundacoes) |>
  print(n = 10)

cat("\n=== Top 10 municipalities by drought (seca) ===\n")
disaster_by_mun |>
  arrange(desc(total_seca)) |>
  head(10) |>
  select(adm_2_name, adm_1_name, sigla_uf, total_seca) |>
  print(n = 10)

cat("\n=== Top 10 municipalities by alagamentos ===\n")
disaster_by_mun |>
  arrange(desc(total_alagamentos)) |>
  head(10) |>
  select(adm_2_name, adm_1_name, sigla_uf, total_alagamentos) |>
  print(n = 10)

cat("\n=== Top 10 municipalities by mass movement ===\n")
disaster_by_mun |>
  arrange(desc(total_massa)) |>
  head(10) |>
  select(adm_2_name, adm_1_name, sigla_uf, total_massa) |>
  print(n = 10)

###Mean and median dengue cases across municipalities

cat("\n=== Summary statistics across all municipalities ===\n")

dengue_by_mun |>
  summarise(
    mean_total_cases   = round(mean(total_cases,   na.rm = TRUE), 1),
    median_total_cases = median(total_cases,        na.rm = TRUE),
    mean_monthly_cases = round(mean(mean_cases,    na.rm = TRUE), 1),
    median_monthly_cases = median(mean_cases,       na.rm = TRUE),
    sd_total_cases     = round(sd(total_cases,     na.rm = TRUE), 1),
    sd_monthly_cases = round(sd(mean_cases, na.rm=T), 1)
  ) |>
  print()

disaster_by_mun |>
  summarise(
    mean_inunda   = round(mean(total_inundacoes, na.rm = TRUE), 1),
    median_inunda = median(total_inundacoes,     na.rm = TRUE),
    sd_inunda = round(sd(total_inundacoes, na.rm=T),1),
    mean_drought  = round(mean(total_seca,       na.rm = TRUE), 1),
    median_drought= median(total_seca,           na.rm = TRUE),
    sd_seca = round(sd(total_seca, na.rm=T),1),
    mean_alaga  = round(mean(total_alagamentos,       na.rm = TRUE), 1),
    median_alaga= median(total_alagamentos,           na.rm = TRUE),
    sd_alaga = round(sd(total_alagamentos, na.rm=T),1),
  ) |>
  print()

#####---Highest combined dengue and disaster burden

combined_burden <- dengue_by_mun |>
  left_join(disaster_by_mun, by = c("IBGE_code", "adm_2_name",
                                    "adm_1_name", "sigla_uf")) |>
  # Rank each municipality by dengue and disaster burden
  mutate(
    rank_dengue   = rank(desc(total_cases)),
    rank_disaster = rank(desc(total_events)),
    # Combined rank: lower = higher burden on both
    combined_rank = rank_dengue + rank_disaster
  ) |>
  arrange(combined_rank)

cat("\n=== Top 20 municipalities with highest combined dengue and disaster burden ===\n")
combined_burden |>
  select(adm_2_name, adm_1_name, sigla_uf, total_cases,
         total_events, rank_dengue, rank_disaster, combined_rank) |>
  head(20) |>
  print(n = 20)

####----State level summary

cat("\n=== Dengue burden by state ===\n")
dengue_by_mun |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    total_cases        = sum(total_cases,    na.rm = TRUE),
    mean_cases_per_mun = round(mean(total_cases, na.rm = TRUE), 1),
    median_cases_per_mun = median(total_cases,   na.rm = TRUE),
    n_municipalities   = n(),
    .groups = "drop"
  ) |>
  arrange(desc(total_cases)) |>
  print(n = 27)

cat("\n=== Disaster burden by state ===\n")
disaster_by_mun |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    total_events         = sum(total_events,    na.rm = TRUE),
    mean_events_per_mun  = round(mean(total_events, na.rm = TRUE), 1),
    median_events_per_mun = median(total_events,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_events)) |>
  print(n = 27)

#save

write_csv(dengue_by_mun,    "Results/dengue_burden_by_municipality.csv")
write_csv(disaster_by_mun,  "Results/disaster_burden_by_municipality.csv")
write_csv(combined_burden,  "Results/combined_burden_by_municipality.csv")

cat("\nSaved burden tables to Results/\n")








#####---disaster impact variables---####
regression_data_mill <- regression_data |>
  mutate(
    pub_total_million  = pub_total_sum  / 1e6,
    priv_pub_million   = priv_pub_sum   / 1e6
  )

impact_summary <- regression_data_mill |>
  filter(!is.na(adm_1_name),outbreak==T) |>
  summarise(
    # --- public losses ---
    pub_total_mean    = round(mean(pub_total_million,   na.rm = TRUE), 2),
    pub_total_median  = round(median(pub_total_million, na.rm = TRUE), 2),
    pub_total_sd      = round(sd(pub_total_million,     na.rm = TRUE), 2),
    pub_total_min     = round(min(pub_total_million,    na.rm = TRUE), 2),
    pub_total_max     = round(max(pub_total_million,    na.rm = TRUE), 2),
    pub_total_sum     = round(sum(pub_total_million,    na.rm = TRUE), 2),
    
    # --- public + private losses ---
    priv_pub_mean     = round(mean(priv_pub_million,    na.rm = TRUE), 2),
    priv_pub_median   = round(median(priv_pub_million,  na.rm = TRUE), 2),
    priv_pub_sd       = round(sd(priv_pub_million,      na.rm = TRUE), 2),
    priv_pub_min      = round(min(priv_pub_million,     na.rm = TRUE), 2),
    priv_pub_max      = round(max(priv_pub_million,     na.rm = TRUE), 2),
    priv_pub_sum      = round(sum(priv_pub_million,     na.rm = TRUE), 2),
    
    # --- homelessness ---
    homeless_mean     = round(mean(dh_homeless_sum, na.rm = TRUE), 2),
    homeless_median   = round(median(dh_homeless_sum, na.rm = TRUE), 2),
    homeless_sd       = round(sd(dh_homeless_sum,   na.rm = TRUE), 2),
    homeless_min      = round(min(dh_homeless_sum,  na.rm = TRUE), 2),
    homeless_max      = round(max(dh_homeless_sum,  na.rm = TRUE), 2),
    homeless_sum      = round(sum(dh_homeless_sum,  na.rm = TRUE), 2)
  ) |>
  pivot_longer(
    everything(),
    names_to  = "metric",
    values_to = "value"
  ) |>
  separate(metric, into = c("variable", "statistic"), sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  mutate(variable = case_when(
    variable == "pub_total" ~ "Public sector losses (BRL)",
    variable == "priv_pub"  ~ "Public + private losses (BRL)",
    variable == "homeless"  ~ "People left homeless (n)"
  ))

cat("=== Summary statistics for key impact variables (2006-2023) ===\n")
print(impact_summary, n = Inf)

#long format table
impact_summary_long <- regression_data |>
  filter(!is.na(adm_1_name)) |>
  select(pub_total_sum, priv_pub_sum, dh_homeless_sum) |>
  rename(
    `Public losses (BRL)`          = pub_total_sum,
    `Public + private losses (BRL)` = priv_pub_sum,
    `People homeless (n)`           = dh_homeless_sum
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    Mean     = round(mean(value,   na.rm = TRUE), 2),
    Median   = round(median(value, na.rm = TRUE), 2),
    SD       = round(sd(value,     na.rm = TRUE), 2),
    Min      = round(min(value,    na.rm = TRUE), 2),
    Max      = round(max(value,    na.rm = TRUE), 2),
    Total    = round(sum(value,    na.rm = TRUE), 2),
    n_nonzero = sum(value > 0,     na.rm = TRUE),
    pct_zero  = round(100 * mean(value == 0, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n=== Impact variable summary (long format) ===\n")
print(impact_summary_long, n = Inf)

#yearly summary
impact_by_year <- regression_data |>
  filter(!is.na(adm_1_name)) |>
  group_by(join_year) |>
  summarise(
    pub_total_sum  = sum(pub_total_sum,   na.rm = TRUE),
    priv_pub_sum   = sum(priv_pub_sum,    na.rm = TRUE),
    homeless_sum   = sum(dh_homeless_sum, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Annual totals for key impact variables ===\n")
print(impact_by_year, n = Inf)

#state summary
impact_by_state <- regression_data |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    pub_total_sum  = round(sum(pub_total_sum,   na.rm = TRUE), 2),
    priv_pub_sum   = round(sum(priv_pub_sum,    na.rm = TRUE), 2),
    homeless_sum   = round(sum(dh_homeless_sum, na.rm = TRUE), 2),
    pub_total_mean = round(mean(pub_total_sum,  na.rm = TRUE), 2),
    priv_pub_mean  = round(mean(priv_pub_sum,   na.rm = TRUE), 2),
    homeless_mean  = round(mean(dh_homeless_sum,na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(desc(priv_pub_sum))

cat("\n=== Impact variable totals by state ===\n")
print(impact_by_state, n = 27)

#copy to clipboard
 impact_summary_long |>
 write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)


