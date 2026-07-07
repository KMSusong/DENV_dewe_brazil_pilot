#' --- 
#' title: "02 Download Climate Data and join to outbreak" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-05" 
#' --- 

#' Overview: 
#'  Download brazil climate data and investigate/summarize it;join it to the 
#'  outbreak data

#' Timeline: 
#'   2026-07-05 
#' 

install.packages("brclimr")
library(brclimr)
library(tidyverse)
library(lubridate)
library(arrow)
library(dplyr)
merged <- read_csv("00_Data/merged_week_month.csv")
adm2_outbreak <-read_csv("00_Data/adm2_outbreak_data.csv")


# Check date range for a single municipality
test_climate <- fetch_data(
  code_muni  = 3304557,   # Rio de Janeiro as a test
  product    = "brdwgd",
  indicator  = "tmax",
  statistics = "mean",
  date_start = as.Date("1900-01-01"),  # set very early to find the true start
  date_end   = Sys.Date()             # today to find the true end
)

cat("Climate data date range:\n")
cat("  From:", as.character(min(test_climate$date)), "\n")
cat("  To:  ", as.character(max(test_climate$date)), "\n")
cat("  N days:", nrow(test_climate), "\n")

# Get unique IBGE codes from your dengue data
ibge_codes <- merged |>
  filter(!is.na(IBGE_code)) |>
  pull(IBGE_code) |>
  unique() |>
  as.integer()

#set date range
date_start <- as.Date("2001-01-01")  
date_end   <- as.Date("2020-07-31") 


download.file(
  "https://brdwgd.nyc3.cdn.digitaloceanspaces.com/parquet/tmax.parquet",
  "00_Data/climate/tmax.parquet", mode = "wb"
)
download.file(
  "https://brdwgd.nyc3.cdn.digitaloceanspaces.com/parquet/tmin.parquet",
  "00_Data/climate/tmin.parquet", mode = "wb"
)
download.file(
  "https://brdwgd.nyc3.cdn.digitaloceanspaces.com/parquet/pr.parquet",
  "00_Data/climate/pr.parquet", mode = "wb"
)

#check var names
open_dataset("00_Data/climate/tmin.parquet") |>
  distinct(name) |>
  collect()

#fetch data, change name filter for each data file before fetching 
fetch_parquet <- function(file_path, indicator_name,
                          codes, d_start, d_end) {
  
  open_dataset(file_path) |>
    filter(
      code_muni %in% codes,
      name == "Tmin_mean",
      date >= d_start,
      date <= d_end
    ) |>
    select(code_muni, date, value) |>
    collect() |>                          # pulls filtered data into R
    rename(IBGE_code = code_muni,
           !!indicator_name := value) |>
    mutate(IBGE_code = as.character(IBGE_code))
}

cat("Reading tmax...\n")
tmax_raw <- fetch_parquet("00_Data/climate/tmax.parquet", "tmax",
                          ibge_codes, date_start, date_end)

cat("Reading tmin...\n")
tmin_raw <- fetch_parquet("00_Data/climate/tmin.parquet", "tmin",
                          ibge_codes, date_start, date_end)

cat("Reading precipitation...\n")
pr_raw   <- fetch_parquet("00_Data/climate/pr.parquet",   "pr",
                          ibge_codes, date_start, date_end)


#aggregate daily to monthly
aggregate_to_monthly <- function(df, value_col, fun = mean) {
  df |>
    mutate(
      join_year  = year(date),
      join_month = month(date)
    ) |>
    group_by(IBGE_code, join_year, join_month) |>
    summarise(
      !!value_col := fun(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

tmax_monthly <- aggregate_to_monthly(tmax_raw, "tmax", mean)
tmin_monthly <- aggregate_to_monthly(tmin_raw, "tmin", mean)
pr_monthly   <- aggregate_to_monthly(pr_raw,   "pr",   sum)   # sum precip

# Combine into one table
climate_monthly <- tmax_monthly |>
  left_join(tmin_monthly, by = c("IBGE_code", "join_year", "join_month")) |>
  left_join(pr_monthly,   by = c("IBGE_code", "join_year", "join_month")) |>
  mutate(temp_range = tmax - tmin)

# Save
write_csv(climate_monthly, "00_Data/climate_monthly.csv")



###---join to outbreak data and add lags
adm2_outbreak_climate <- adm2_outbreak |>
  mutate(IBGE_code = as.character(IBGE_code)) |>
  left_join(climate_monthly, by = c("IBGE_code", "join_year", "join_month")) |>
  arrange(IBGE_code, join_year, join_month) |>
  group_by(IBGE_code) |>
  mutate(
    pr_lag1   = lag(pr,   1),
    pr_lag2   = lag(pr,   2),
    pr_lag3   = lag(pr,   3),
    tmax_lag1 = lag(tmax, 1),
    tmax_lag2 = lag(tmax, 2),
    tmax_lag3 = lag(tmax, 3),
    tmin_lag1 = lag(tmin, 1),
    tmin_lag2 = lag(tmin, 2),
    tmin_lag3 = lag(tmin, 3)
  ) |>
  ungroup()

# Quick checks
cat("Rows with climate data:", sum(!is.na(adm2_outbreak_climate$tmax)), "\n")
cat("Rows without:          ", sum( is.na(adm2_outbreak_climate$tmax)), "\n")

summary(climate_monthly$tmax)
summary(climate_monthly$tmin)
summary(climate_monthly$pr)


#save merged climate/outbreak data
write_csv(adm2_outbreak_climate, "00_Data/adm2_outbreak_climate.csv")
