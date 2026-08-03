#' --- 
#' title: "02 Make Season Variable" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-010" 
#' --- 

#' Overview: 
#'  Make the seasonal variable by assigning october as month 1 in the season.
#'

#' Timeline: 
#'   2026-07-10
#'   2026-07-29 change to v3 after anscombe transform
#'   2026-08-03 change to v4 after going back to outbreak exclusion theshold
#'   
 library(tidyverse)
 library(dplyr)
regression_data <-read_csv("00_Data/regression_data_v4.csv")



#----- Align data from calendar year to dengue season 

# Function 
circular_encode <- function(x, shift, cycle_length) {
  (((x) - shift) %% cycle_length) + 1
}

regression_data_season_aligned <- regression_data %>% 
  mutate(season_month= circular_encode(join_month, 
                                        10, 
                                        12))
is.factor(regression_data_season_aligned$season_month)



regression_data_season_aligned <- regression_data_season_aligned %>%
  mutate(season_month = factor(season_month, ordered = TRUE))


write_csv(regression_data_season_aligned, "00_Data/regression_data_v4.csv")
