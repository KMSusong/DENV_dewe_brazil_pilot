#' --- 
#' title: "02 missing data summary statistics" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-09" 
#' --- 

#' Overview: 
#'  Calculate how many municipalities are missing data and in what years from the
#'  regression dataset

#' Timeline: 
#'   2026-08-09 



library(tidyr)
regression_data <-read_csv("00_Data/regression_data_v4.csv")


complete_grid <- regression_data |>
  distinct(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  crossing(
    join_year = 2006:2023,
    join_month = 1:12
  )

coverage <- complete_grid |>
  left_join(
    regression_data |>
      select(IBGE_code, join_year, join_month, outbreak),
    by = c("IBGE_code", "join_year", "join_month")
  ) |>
  mutate(
    missing = is.na(outbreak)
  )
missing_by_year <- coverage |>
  group_by(join_year) |>
  summarise(
    missing_muni_months = sum(missing),
    total_muni_months = n(),
    pct_missing = round(100 * missing_muni_months / total_muni_months, 2),
    .groups = "drop"
  )

missing_by_year
missing_by_state_year <- coverage |>
  group_by(adm_1_name, join_year) |>
  summarise(
    missing_muni_months = sum(missing),
    total_muni_months = n(),
    pct_missing = round(100 * missing_muni_months / total_muni_months, 2),
    .groups = "drop"
  )

missing_by_state_year
library(tidyr)

missing_table <- missing_by_state_year |>
  mutate(
    value = sprintf("%.1f%%", pct_missing)
  ) |>
  select(adm_1_name, join_year, value) |>
  pivot_wider(
    names_from = join_year,
    values_from = value
  ) |>
  arrange(adm_1_name)

print(missing_table, n=15)

install.packages("clipr")   # only once
library(clipr)

write_clip(missing_table)
library(flextable)
library(officer)

ft <- flextable(missing_table) |>
  autofit() |>
  fontsize(size = 8)

read_docx() |>
  body_add_flextable(ft) |>
  print(target = "Results/missing_table.docx")
