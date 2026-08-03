#' --- 
#' title: "02 3 month rolling mean" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-10" 
#' --- 

#' Overview: 
#'  create a 3 month rolling mean to account for autocorrelation

#' Timeline: 
#'   2026-07-10
#'   2026-07-29 change to regression data v3 after anscombe transform
#'   2026-08-3 change back to outbreak exclusion threshold for v4

library(tidyverse)
library(dplyr)
library(zoo)
regression_data <-read_csv("00_Data/regression_data_v4.csv")

regression_data <- regression_data %>%
  arrange(join_year, join_month) %>%
  mutate(
    sd_anomaly_mean3 = rollmean(
      lag(sd_anomaly, 1),   # exclude current month
      k = 3,
      fill = NA,
      align = "right"
    )
  )
write_csv(regression_data, "00_Data/regression_data_v4.csv")
