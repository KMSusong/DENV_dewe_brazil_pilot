#' --- 
#' title: "02 correlation of climate variables and outbreak" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-09" 
#' --- 

#' Overview: 
#'  calculate the correlations between the climate variables and outbreak
#'  from the regression dataset

#' Timeline: 
#'
library(tidyverse)
regression_data <-read_csv("00_Data/regression_data_v4.csv")
model_data <- regression_data |>
  select(
    # outcome
    outbreak, join_year, join_month,
    # random effect
    adm_1_name, IBGE_code,
    # fixed effects you plan to test (add chuvas? might remove pre-2013) see merge
    #script for lag names
    n_inunda_lag1, n_inunda_lag2, n_inunda_lag3, n_inunda_lag4, n_inunda_lag5,
    n_seca_lag1, n_seca_lag2, n_seca_lag3, n_seca_lag4, n_seca_lag5,
    n_alaga_lag1, n_alaga_lag2, n_alaga_lag3, n_alaga_lag4, n_alaga_lag5,
    pub_total_lag1, pub_total_lag2, pub_total_lag3,
    priv_total_lag1, priv_total_lag2, priv_total_lag3,
    priv_pub_lag1,  priv_pub_lag2,  priv_pub_lag3,
    dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
    sd_anomaly, sd_prev_5yr, sd_anomaly_mean3, season_month,
    tmin_lag1, tmin_lag2, tmin_lag3, tmax_lag1, tmax_lag2, tmax_lag3, pr_lag1,
    pr_lag2, pr_lag3
    
  ) |>
  drop_na()



#spearman correlation
climate_vars <- c("tmin_lag1", "tmax_lag2", "pr_lag1",
                  "tmin_lag2", "tmax_lag1", "pr_lag2", "pr_lag3")

# Correlation of each climate variable with sd_anomaly
cor_results <- model_data |>
  select(sd_anomaly, all_of(climate_vars)) |>
  drop_na() |>
  summarise(across(
    all_of(climate_vars),
    ~ cor(.x, sd_anomaly, method = "spearman"),
    .names = "cor_{.col}"
  )) |>
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "spearman_r") |>
  mutate(variable = str_remove(variable, "cor_")) |>
  arrange(desc(abs(spearman_r)))

print(cor_results)

###correlation matrix
model_data |>
  select(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  drop_na() |>
  cor(method = "spearman") |>
  round(3)

##significance test
cor_tests <- map_dfr(climate_vars, function(var) {
  
  d <- model_data |>
    select(sd_anomaly, all_of(var)) |>
    drop_na()
  
  test <- cor.test(d[[var]], d$sd_anomaly, method = "spearman")
  
  tibble(
    variable   = var,
    spearman_r = round(test$estimate, 3),
    p_value    = round(test$p.value, 4)
  )
}) |>
  arrange(desc(abs(spearman_r)))

print(cor_tests)

