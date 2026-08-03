#' --- 
#' title: "03 Regression with nlme" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-18" 
#' --- 

#' Overview: 
#'  regression with the nlme package to account for spatial and 
#'  temporal autocorrelation; test for performace

#' Timeline: 
#'   2026-07-18 
#' 
install.packages("nlme")
library(tidyr)
library(tidyverse)
library(nlme)
library(lme4)
library(performance)
library(dplyr)
library(lmtest) 
library(broom.mixed)

regression_data_v2 <-read_csv("00_Data/regression_data_v2.csv")

#### ----prepare data ----
model_data <- regression_data_v2 |>
  select(
    # outcome
    outbreak, join_year, join_month, calendar_start_date,
    # random effect
    adm_1_name, IBGE_code,
    # fixed effects you plan to test (add chuvas? might remove pre-2013) see merge
    #script for lag names
    n_inunda_lag1, n_inunda_lag2, n_inunda_lag3, n_inunda_lag4, n_inunda_lag5,
    n_seca_lag1, n_seca_lag2, n_seca_lag3, n_seca_lag4, n_seca_lag5,
    n_enxu_lag1, n_enxu_lag2, n_enxu_lag3,
    n_alaga_lag1, n_alaga_lag2, n_alaga_lag3, n_alaga_lag4, n_alaga_lag5,
    n_massa_lag1, n_massa_lag2, n_massa_lag3, n_massa_lag4, n_massa_lag5,
    pub_total_lag1, pub_total_lag2, pub_total_lag3,
    priv_total_lag1, priv_total_lag2, priv_total_lag3,
    priv_pub_lag1,  priv_pub_lag2,  priv_pub_lag3,
    pub_water_lag1,
    pub_vector_lag1,
    flag_water_deplete_lag1, flag_water_deplete_lag2, flag_water_deplete_lag3,
    flag_water_contam_lag1, flag_water_contam_lag2, flag_water_contam_lag3,
    dh_displaced_lag1, dh_displaced_lag2, dh_displaced_lag3,
    dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
    sd_anomaly, sd_prev_5yr, season_month, sd_anomaly_mean3,
    tmin_lag1, tmax_lag1, pr_lag1
    
  ) |>
  drop_na()

cat("Rows available for modelling:", nrow(model_data), "\n")
cat("Outbreak months:", sum(model_data$outbreak), "\n")
cat("Non-outbreak months:", sum(!model_data$outbreak), "\n")


#### ----  VIEW AND EXPORT THE LOG ----

# View in console
print(model_log, n = Inf)

# Copy to clipboard for Excel (Mac)
model_log |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

# Save as CSV
write_csv(model_log, "Results/model_selection_log.csv")

log_model("m1", "n_inunda_lag1", m1, m0, kept = TRUE)


#####start of regression####

m1 <- lmer(sd_anomaly ~ n_inunda_lag1 + (1 | adm_1_name),
                data   = model_data)


summary(m1)
arm::display(m1)
aug <- broom.mixed::augment(m1)

#testing random slopes
m_int <-  lmer(sd_anomaly ~ n_inunda_lag1 + (1 | adm_1_name),
               data   = model_data)
m_slopes <-  lmer(sd_anomaly ~ n_inunda_lag1 + (1 + n_inunda_lag1 | adm_1_name), 
                  data = model_data)

model_data$predict_int <- predict(m_int)
model_data$predict_slopes <- predict(m_slopes)

ggplot(model_data, aes(n_inunda_lag1, sd_anomaly, colour = adm_1_name, group = adm_1_name)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(y = predict_int), colour = "grey") +
  geom_line(aes(y = predict_slopes), colour = "black") # exercise

arm::display(m_int)
arm::display(m_slopes)

tidy(m_slopes, conf.int = TRUE)
tidy(m_int, conf.int = TRUE)

check_model(m1)


###viewing 
ggplot(aug, aes(n_inunda_lag1, `sd_anomaly`)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~adm_1_name) +
  geom_line(aes(x = n_inunda_lag1, y = .fitted), colour = "red")

ggplot(aug, aes(.fitted, .resid)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~adm_1_name) +
  geom_hline(yintercept = 0, colour = "red")

aug$date <- model_data$calendar_start_date
ggplot(aug, aes(date, .resid)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~adm_1_name) +
  geom_hline(yintercept = 0, colour = "red")

####lme model#####
m2 <- lme(sd_anomaly ~ n_inunda_lag1, random = ~ 1| adm_1_name, 
          data= model_data)
m2 <- lme(sd_anomaly ~ priv_pub_lag1, random = ~ 1| adm_1_name, 
          data= model_data)

m2 <- lme(sd_anomaly ~ n_inunda_lag1,
          random  = ~ 1 | adm_1_name,
          data    = model_data,
          control = lmeControl(opt = "optim"))

check_model(m2)

model_data <- model_data |>
  mutate(n_inunda_lag1_scaled = scale(n_inunda_lag1))

m2 <- lme(sd_anomaly ~ n_inunda_lag1_scaled,
          random = ~ 1 | adm_1_name,
          data   = model_data)


plot(ACF(m2, resType = "normalized"))

m3 <- lme(sd_anomaly ~ priv_pub_lag1,
          random = ~ 1 | adm_1_name, data = model_data, correlation = corAR1())

m3 <- lme(sd_anomaly ~ n_inunda_lag1,
          random = ~ 1 | adm_1_name, data = model_data, correlation = corAR1())
###not working so switch back to lmer
# Add a time index variable
model_data <- model_data |>
  mutate(time_index = join_year * 12 + join_month)

# Random intercept for state + random intercept for time
m3_ar <- lmer(sd_anomaly ~ n_inunda_lag1 +
                (1 | adm_1_name) +
                (1 | time_index),
              data = model_data,
              REML = FALSE)

summary(m3_ar)
model_performance(m3_ar)
lrtest(m1, m3_ar)
AIC(m1, m3_ar) 



####back to lmer for disaster estimates######
model_data <- model_data |>
  mutate(across(
    c(priv_pub_lag1, priv_pub_lag2, priv_pub_lag3, pub_total_lag1, pub_total_lag2,
      pub_total_lag3,  priv_total_lag1, priv_total_lag2, priv_total_lag3),
    ~ scale(.x)[, 1],   # scale() returns a matrix, [,1] extracts the vector
    .names = "{.col}_scaled"
  ))
#if doesnt work
model_data <- model_data |>
  mutate(priv_pub_lag1_scaled = scale(priv_pub_lag1))

m5 <- lmer(sd_anomaly ~ priv_pub_lag1 + (1 | adm_1_name),
           data   = model_data)
m5 <- lmer(sd_anomaly ~ priv_pub_lag1 + season_month + sd_anomaly_mean3
             + (1 | adm_1_name),
           data    = model_data,
           REML    = FALSE)
summary(m5)
model_performance(m5)
lrtest(m1, m5)
AIC(m1, m5)
check_model(m5)
broom.mixed::tidy(m5,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

m6 <- lmer(sd_anomaly ~ priv_pub_lag3 + 
             (1 | adm_1_name) ,
           data   = model_data,
           control = lmerControl(autoscale = TRUE))

summary(m6)
model_performance(m6)
lrtest(m5, m6)
AIC(m5, m6)
check_model(m6)
broom.mixed::tidy(m6)
broom.mixed::tidy(m6,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

m7 <- lmer(sd_anomaly ~ priv_total_lag3 + 
             (1 | adm_1_name) + season_month + sd_anomaly_mean3,
           data   = model_data,
           control = lmerControl(autoscale = TRUE))

summary(m7)
model_performance(m7)
lrtest(m5, m7)
AIC(m5, m7)
check_model(m7)
broom.mixed::tidy(m7)
broom.mixed::tidy(m7,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)



m8 <- lmer(sd_anomaly ~ pub_total_lag3 + 
             (1 | adm_1_name) + season_month + sd_anomaly_mean3,
           data   = model_data,
           control = lmerControl(autoscale = TRUE))


broom.mixed::tidy(m8,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

####test water depletion#####
m9 <- lmer(sd_anomaly ~ flag_water_deplete_lag3 + 
             (1 | adm_1_name) + season_month + sd_anomaly_mean3
             +pr_lag1 + tmin_lag1 +tmax_lag1,
           data   = model_data)

broom.mixed::tidy(m9,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)
check_model(m9)


m10 <- lmer(sd_anomaly ~  flag_water_contam_lag2
           +  (1 | adm_1_name) + season_month + sd_anomaly_mean3,
           data   = model_data)

broom.mixed::tidy(m10,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

####displaced/homeless####


m11 <- lmer(sd_anomaly ~  dh_displaced_lag3
            +  (1 | adm_1_name) + season_month + sd_anomaly_mean3,
            data   = model_data)

broom.mixed::tidy(m11,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)


m12 <- lmer(sd_anomaly ~  dh_homeless_lag3
            +  (1 | adm_1_name) + season_month + sd_anomaly_mean3,
            data   = model_data)

broom.mixed::tidy(m12,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)


####test disasters again#####
m13 <- lmer(sd_anomaly ~ n_inunda_lag2 + 
             (1 | adm_1_name) + season_month + sd_anomaly_mean3
           +pr_lag1 + tmin_lag1 +tmax_lag1,
           data   = model_data)

broom.mixed::tidy(m13,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)


m14 <- lmer(sd_anomaly ~ n_alaga_lag3 + 
              (1 | adm_1_name) + season_month + sd_anomaly_mean3
            +pr_lag1 + tmin_lag1 +tmax_lag1,
            data   = model_data)

broom.mixed::tidy(m14,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

m15 <- lmer(sd_anomaly ~ n_massa_lag2 + 
              (1 | adm_1_name) + season_month + sd_anomaly_mean3
            +pr_lag1 + tmin_lag1 +tmax_lag1,
            data   = model_data)

broom.mixed::tidy(m15,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

m16 <- lmer(sd_anomaly ~ n_seca_lag5 + 
              (1 | adm_1_name) + season_month + sd_anomaly_mean3
            +pr_lag1 + tmin_lag1 +tmax_lag1,
            data   = model_data)

broom.mixed::tidy(m16,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

