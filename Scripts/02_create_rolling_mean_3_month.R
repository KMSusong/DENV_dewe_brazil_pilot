#' --- 
#' title: "02 3 month rolling mean" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-10" 
#' --- 

#' Overview: 
#'  create a 3 month rolling mean to account for autocorrelation

#' Timeline: 
#'   2026-07-10

library(tidyverse)
library(dplyr)
library(zoo)
regression_data <-read_csv("00_Data/regression_data_v2.csv")

regression_data <- regression_data %>%
  # Crucial: Arrange by time first so the window slides in chronological order
  arrange(join_year, join_month) %>% 
  mutate(
    # Calculates the average of the current month and the 2 preceding months
    sd_anomaly_mean3 = rollmean(sd_anomaly, k = 3, fill = NA, align = "right")
  )

write_csv(regression_data, "00_Data/regression_data_v2.csv")
