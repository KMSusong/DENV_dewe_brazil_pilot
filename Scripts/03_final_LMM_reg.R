#' --- 
#' title: "03 Final LMM Regression" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-03" 
#' --- 

#' Overview: final LMM regression after fixing rolling window leakage, adding 
#' municipality nested random effects
#'  
#'

#' Timeline: 
#'   2026-08-03

regression_data_v4 <-read_csv("00_Data/regression_data_v4.csv")
install.packages("broom.mixed")
install.packages("lme4")
install.packages("performance")
install.packages("lmtest")
library(lme4)
library(performance)
library(dplyr)
library(tidyr)
library(lmtest) 
library(broom.mixed)
#### ----prepare data ----
model_data <- regression_data_v4 |>
  select(
    # outcome
    outbreak,
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
    sd_anomaly, sd_prev_5yr, sd_anomaly_mean3, season_month,
    tmin_lag1, tmin_lag2, tmin_lag3, tmax_lag1, tmax_lag2, tmax_lag3, pr_lag1,
    pr_lag2, pr_lag3
    
  ) |>
  drop_na()

cat("Rows available for modelling:", nrow(model_data), "\n")
cat("Outbreak months:", sum(model_data$outbreak), "\n")
cat("Non-outbreak months:", sum(!model_data$outbreak), "\n")

#####---Model Log---#####
# View in console
print(model_log, n = Inf)

# Copy to clipboard for Excel (Mac)
model_log |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

# Save as CSV
write_csv(model_log, "Results/model_selection_log.csv")

#load
model_log <- read_csv("Results/model_selection_log.csv")

# Check Variance Inflation Factors (VIF)
# Values above 5 or 10 indicate problematic multicollinearity
check_collinearity(m5_lmer)

#check convergence
check_convergence(m3_lmer)

# Checks if your data is overdispersed
check_overdispersion(m3_lmer)

# Checks if the random effects variance is safely above zero
check_singularity(m1_lmer)

#checks model fit graphically
check_model(m1_lmer)

#to check colinearity?
pairs()

##get estimates and CI
broom.mixed::tidy(m3_lmer,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

####----Testing climate variables---####

# Linear mixed model with sd_anomaly as outcome

m0_lmer <- lmer(sd_anomaly ~ 1 + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)   # use ML not REML for AIC comparison
perf_m0 <- model_performance(m0_lmer)
print(model_log, n = Inf)
model_log <- bind_rows(model_log, tibble(
  step             = 41L,
  model_name       = "m0",
  variable_added   = "null (intercept + random effect)",
  AIC              = round(perf_m0$AIC, 2),
  BIC              = round(perf_m0$BIC, 2),
  R2_marginal      = round(perf_m0$R2_marginal,    4),
  R2_conditional   = round(perf_m0$R2_conditional, 4),
  LRT_chisq        = NA_real_,
  LRT_df           = NA_integer_,
  LRT_p            = NA_real_,
  delta_AIC        = NA_real_,
  kept             = TRUE,
  notes            = "baseline"
))
print(model_log, n = Inf)
broom.mixed::tidy(m0_lmer)

m1_lmer <- lmer(sd_anomaly ~ tmin_lag1 + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)

summary(m1_lmer)
model_performance(m1_lmer)
lrtest(m0_lmer, m1_lmer)
AIC(m0_lmer, m1_lmer)

log_model("m1", "tmin_lag1", m1_lmer, m0_lmer, kept = TRUE)

broom.mixed::tidy(m1_lmer,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

m2_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag2 
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m1_lmer, m2_lmer)
AIC(m1_lmer, m2_lmer)

log_model("m2", "tmin_lag2", m2_lmer, m1_lmer, kept = FALSE)

m3_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m1_lmer, m3_lmer)
AIC(m1_lmer, m3_lmer)

log_model("m3", "tmin_lag3", m3_lmer, m1_lmer, kept = TRUE)

broom.mixed::tidy(m3_lmer,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)


##comparing tmin lags
m_tmin_lag1 <- lmer(
  sd_anomaly ~ tmin_lag1 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_tmin_lag2 <- lmer(
  sd_anomaly ~ tmin_lag2 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_tmin_lag3 <- lmer(
  sd_anomaly ~ tmin_lag3 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(m_tmin_lag1, m_tmin_lag2, m_tmin_lag3)
tidy(m_tmin_lag1, effects = "fixed")
tidy(m_tmin_lag2, effects = "fixed")
tidy(m_tmin_lag3, effects = "fixed")
#--- tmin_lag1 is the best --

#df     AIC
#m_tmin_lag1  5 4665770 #the best
#m_tmin_lag2  5 4665883
#m_tmin_lag3  5 4666034



#testing tmax with tmin

m4_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m3_lmer, m4_lmer)
AIC(m3_lmer, m4_lmer)

log_model("m4", "tmax_lag1", m4_lmer, m3_lmer, kept = TRUE)

m5_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m4_lmer, m5_lmer)
AIC(m4_lmer, m5_lmer)

log_model("m5", "tmax_lag2", m5_lmer, m4_lmer, kept = TRUE)

m6_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2 + tmax_lag3
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m5_lmer, m6_lmer)
AIC(m5_lmer, m6_lmer)

log_model("m6", "tmax_lag3", m6_lmer, m5_lmer, kept = FALSE)


####comparing tmax lags
m_tmax_lag1 <- lmer(
  sd_anomaly ~ tmax_lag1 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_tmax_lag2 <- lmer(
  sd_anomaly ~ tmax_lag2 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_tmax_lag3 <- lmer(
  sd_anomaly ~ tmax_lag3 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(m_tmax_lag1, m_tmax_lag2, m_tmax_lag3)
#            df     AIC
#m_tmax_lag1  5 4665941
#m_tmax_lag2  5 4665897  ##the best
#m_tmax_lag3  5 4665990

###testing pr
m7_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2 + pr_lag1
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m5_lmer, m7_lmer)
AIC(m5_lmer, m7_lmer)

log_model("m7", "pr_lag1", m7_lmer, m5_lmer, kept = T)


m8_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2 + pr_lag1 + pr_lag2
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m7_lmer, m8_lmer)
AIC(m7_lmer, m8_lmer)

log_model("m8", "pr_lag2", m8_lmer, m7_lmer, kept = FALSE)

m9_lmer <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2 + pr_lag1 + pr_lag3
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)



lrtest(m7_lmer, m9_lmer)
AIC(m7_lmer, m9_lmer)

log_model("m9", "pr_lag3", m9_lmer, m7_lmer, kept = FALSE)

###comparing pr lags
m_pr_lag1 <- lmer(
  sd_anomaly ~ pr_lag1 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_pr_lag2 <- lmer(
  sd_anomaly ~ pr_lag2 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

m_pr_lag3 <- lmer(
  sd_anomaly ~ pr_lag3 + (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(m_pr_lag1, m_pr_lag2, m_pr_lag3)
#          df     AIC
#m_pr_lag1  5 4666034 ##best
#m_pr_lag2  5 4666038
#m_pr_lag3  5 4666040

####final climate models
#final climate model with multiple lags (same as m7_lmer:
final_clim_all_lag <- lmer(sd_anomaly ~ tmin_lag1 + tmin_lag3 + tmax_lag1
                + tmax_lag2 + pr_lag1
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)
#--final climate model with only one lag per variable
final_clim <- lmer(sd_anomaly ~ tmin_lag1  + tmax_lag2
                    + pr_lag1
                   + (1 | adm_1_name/IBGE_code),
                   data   = model_data,
                   REML   = FALSE)

broom.mixed::tidy(final_clim,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)
check_collinearity(final_clim)
log_model("final_clim", "just tmin_lag1, tmax_lag2, pr_lag1", final_clim, m0_lmer, kept = T)

###---Testing Disaster Variables----####
d1_lmer <- lmer(sd_anomaly ~ n_inunda_lag1
             + tmin_lag1  + tmax_lag2 + pr_lag1
                   + (1 | adm_1_name/IBGE_code),
                   data   = model_data,
                   REML   = FALSE)
lrtest(final_clim, d1_lmer)
AIC(final_clim, d1_lmer)

log_model("d1", "n_inunda_lag1", d1_lmer, final_clim, kept = T)

d2_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag2
                + tmin_lag1  + tmax_lag2 + pr_lag1
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)
lrtest(d1_lmer, d2_lmer)
AIC(d1_lmer, d2_lmer)

log_model("d2", "n_inunda_lag2", d2_lmer, d1_lmer, kept = F)

d3_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag5
                + tmin_lag1  + tmax_lag2 + pr_lag1
                + (1 | adm_1_name/IBGE_code),
                data   = model_data,
                REML   = FALSE)
lrtest(d1_lmer, d3_lmer)
AIC(d1_lmer, d3_lmer)

log_model("d3", "n_inunda_lag5", d3_lmer, d1_lmer, kept = F)




######---inunda lags---####

d1 <- lmer(
  sd_anomaly ~ n_inunda_lag1 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

d2 <- lmer(
  sd_anomaly ~ n_inunda_lag2 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

d3 <- lmer(
  sd_anomaly ~ n_inunda_lag3 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

d4 <- lmer(
  sd_anomaly ~ n_inunda_lag4 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

d5 <- lmer(
  sd_anomaly ~ n_inunda_lag5 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(d1, d2, d3, d4, d5)
bind_rows(
  tidy(d1, effects = "fixed") |> dplyr::filter(term == "n_inunda_lag1"),
  tidy(d2, effects = "fixed") |> dplyr::filter(term == "n_inunda_lag2"),
  tidy(d3, effects = "fixed") |> dplyr::filter(term == "n_inunda_lag3"),
  tidy(d4, effects = "fixed") |> dplyr::filter(term == "n_inunda_lag4"),
  tidy(d5, effects = "fixed") |> dplyr::filter(term == "n_inunda_lag5")
)

broom.mixed::tidy(d5,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

###stepwise addition
base_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

inunda_lag1_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    n_inunda_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

inunda_lag12_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    n_inunda_lag1 + n_inunda_lag2 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

inunda_lag123_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    n_inunda_lag1 + n_inunda_lag2 + n_inunda_lag3 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

inunda_lag1234_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    n_inunda_lag1 + n_inunda_lag2 + n_inunda_lag3 + n_inunda_lag4 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

inunda_lag12345_lmer <- lmer(
  sd_anomaly ~ tmin_lag1 + tmax_lag2 + pr_lag1 +
    n_inunda_lag1 + n_inunda_lag2 + n_inunda_lag3 +
    n_inunda_lag4 + n_inunda_lag5 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(
  base_lmer,
  inunda_lag1_lmer,
  inunda_lag12_lmer,
  inunda_lag123_lmer,
  inunda_lag1234_lmer,
  inunda_lag12345_lmer
)

anova(base_lmer, inunda_lag1_lmer)
anova(inunda_lag1_lmer, inunda_lag12_lmer)
anova(inunda_lag12_lmer, inunda_lag123_lmer)
anova(inunda_lag123_lmer, inunda_lag1234_lmer)
anova(inunda_lag1234_lmer, inunda_lag12345_lmer)

#######--alaga lags--####

alaga1_lmer <- lmer(
  sd_anomaly ~ n_alaga_lag1 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

alaga2_lmer <- lmer(
  sd_anomaly ~ n_alaga_lag2 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

alaga3_lmer <- lmer(
  sd_anomaly ~ n_alaga_lag3 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

alaga4_lmer <- lmer(
  sd_anomaly ~ n_alaga_lag4 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

alaga5_lmer <- lmer(
  sd_anomaly ~ n_alaga_lag5 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(alaga1_lmer, alaga2_lmer, alaga3_lmer, alaga4_lmer, alaga5_lmer)
alaga_estimates <- bind_rows(
  tidy(alaga1_lmer, effects = "fixed") %>% filter(term == "n_alaga_lag1"),
  tidy(alaga2_lmer, effects = "fixed") %>% filter(term == "n_alaga_lag2"),
  tidy(alaga3_lmer, effects = "fixed") %>% filter(term == "n_alaga_lag3"),
  tidy(alaga4_lmer, effects = "fixed") %>% filter(term == "n_alaga_lag4"),
  tidy(alaga5_lmer, effects = "fixed") %>% filter(term == "n_alaga_lag5")
) %>%
  mutate(lag = 1:5)

alaga_estimates

###testing additively

######---Seca/drought lags----####

seca1_lmer <- lmer(
  sd_anomaly ~ n_seca_lag1 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

seca2_lmer <- lmer(
  sd_anomaly ~ n_seca_lag2 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

seca3_lmer <- lmer(
  sd_anomaly ~ n_seca_lag3 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

seca4_lmer <- lmer(
  sd_anomaly ~ n_seca_lag4 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

seca5_lmer <- lmer(
  sd_anomaly ~ n_seca_lag5 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(seca1_lmer, seca2_lmer, seca3_lmer, seca4_lmer, seca5_lmer)

seca_estimates <- bind_rows(
  tidy(seca1_lmer, effects = "fixed") %>% filter(term == "n_seca_lag1"),
  tidy(seca2_lmer, effects = "fixed") %>% filter(term == "n_seca_lag2"),
  tidy(seca3_lmer, effects = "fixed") %>% filter(term == "n_seca_lag3"),
  tidy(seca4_lmer, effects = "fixed") %>% filter(term == "n_seca_lag4"),
  tidy(seca5_lmer, effects = "fixed") %>% filter(term == "n_seca_lag5")
) %>%
  mutate(lag = 1:5)

seca_estimates

######--Massa lags----####
massa1_lmer <- lmer(
  sd_anomaly ~ n_massa_lag1 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

massa2_lmer <- lmer(
  sd_anomaly ~ n_massa_lag2 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

massa3_lmer <- lmer(
  sd_anomaly ~ n_massa_lag3 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

massa4_lmer <- lmer(
  sd_anomaly ~ n_massa_lag4 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

massa5_lmer <- lmer(
  sd_anomaly ~ n_massa_lag5 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = model_data,
  REML = FALSE
)

AIC(massa1_lmer, massa2_lmer, massa3_lmer, massa4_lmer, massa5_lmer)
massa_estimates <- bind_rows(
  tidy(massa1_lmer, effects = "fixed") %>% filter(term == "n_massa_lag1"),
  tidy(massa2_lmer, effects = "fixed") %>% filter(term == "n_massa_lag2"),
  tidy(massa3_lmer, effects = "fixed") %>% filter(term == "n_massa_lag3"),
  tidy(massa4_lmer, effects = "fixed") %>% filter(term == "n_massa_lag4"),
  tidy(massa5_lmer, effects = "fixed") %>% filter(term == "n_massa_lag5")
) %>%
  mutate(lag = 1:5)

massa_estimates


###---Testing Disaster Estimates---####
#filter to outbreak only months
outbreak_data <- model_data %>%
  filter(outbreak == TRUE)
table(model_data$outbreak)
nrow(outbreak_data)

summary(e1_lmer)
broom.mixed::tidy(e1_lmer,effects = "fixed",  exponentiate = FALSE,
                  conf.int = T, p.value = T)

#check scale
sapply(outbreak_data[, c("priv_pub_lag1", "pub_total_lag1", "dh_homeless_lag1", "tmin_lag1", "tmax_lag2", "pr_lag1")], sd, na.rm = TRUE)
#divide damage estimate by 1 million
outbreak_data <- outbreak_data %>%
  mutate(
    priv_pub_lag1_million = priv_pub_lag1 / 1e6,
    priv_pub_lag2_million = priv_pub_lag2 / 1e6,
    priv_pub_lag3_million = priv_pub_lag3 / 1e6,
    
    pub_total_lag1_million = pub_total_lag1 / 1e6,
    pub_total_lag2_million = pub_total_lag2 / 1e6,
    pub_total_lag3_million = pub_total_lag3 / 1e6
  )

#######---private public total damage---######
priv_pub_lag1_lmer <- lmer(
  sd_anomaly ~ priv_pub_lag1_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

priv_pub_lag2_lmer <- lmer(
  sd_anomaly ~ priv_pub_lag2_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

priv_pub_lag3_lmer <- lmer(
  sd_anomaly ~ priv_pub_lag3_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

AIC(
  priv_pub_lag1_lmer,
  priv_pub_lag2_lmer,
  priv_pub_lag3_lmer
)

library(broom.mixed)
library(dplyr)

priv_pub_estimates <- bind_rows(
  tidy(priv_pub_lag1_lmer, effects = "fixed") %>% 
    filter(term == "priv_pub_lag1_million"),
  
  tidy(priv_pub_lag2_lmer, effects = "fixed") %>% 
    filter(term == "priv_pub_lag2_million"),
  
  tidy(priv_pub_lag3_lmer, effects = "fixed") %>% 
    filter(term == "priv_pub_lag3_million")
)

priv_pub_estimates
confint(priv_pub_lag1_lmer, parm = "priv_pub_lag1_million")
confint(priv_pub_lag2_lmer, parm = "priv_pub_lag2_million")
confint(priv_pub_lag3_lmer, parm = "priv_pub_lag3_million")

#######---public total damage---######

pub_total_lag1_lmer <- lmer(
  sd_anomaly ~ pub_total_lag1_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

pub_total_lag2_lmer <- lmer(
  sd_anomaly ~ pub_total_lag2_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

pub_total_lag3_lmer <- lmer(
  sd_anomaly ~ pub_total_lag3_million +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

AIC(
  pub_total_lag1_lmer,
  pub_total_lag2_lmer,
  pub_total_lag3_lmer
)

pub_total_estimates <- bind_rows(
  tidy(pub_total_lag1_lmer, effects = "fixed") %>% 
    filter(term == "pub_total_lag1_million"),
  
  tidy(pub_total_lag2_lmer, effects = "fixed") %>% 
    filter(term == "pub_total_lag2_million"),
  
  tidy(pub_total_lag3_lmer, effects = "fixed") %>% 
    filter(term == "pub_total_lag3_million")
)

pub_total_estimates

#######---homeless---######
homeless_lag1_lmer <- lmer(
  sd_anomaly ~ dh_homeless_lag1 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

homeless_lag2_lmer <- lmer(
  sd_anomaly ~ dh_homeless_lag2 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

homeless_lag3_lmer <- lmer(
  sd_anomaly ~ dh_homeless_lag3 +
    tmin_lag1 + tmax_lag2 + pr_lag1 +
    (1 | adm_1_name/IBGE_code),
  data = outbreak_data,
  REML = FALSE
)

AIC(
  homeless_lag1_lmer,
  homeless_lag2_lmer,
  homeless_lag3_lmer
)

homeless_estimates <- bind_rows(
  tidy(homeless_lag1_lmer, effects = "fixed") %>%
    filter(term == "dh_homeless_lag1"),
  
  tidy(homeless_lag2_lmer, effects = "fixed") %>%
    filter(term == "dh_homeless_lag2"),
  
  tidy(homeless_lag3_lmer, effects = "fixed") %>%
    filter(term == "dh_homeless_lag3")
)

homeless_estimates
confint(homeless_lag1_lmer, parm = "dh_homeless_lag1")
confint(homeless_lag2_lmer, parm = "dh_homeless_lag2")
confint(homeless_lag3_lmer, parm = "dh_homeless_lag3")
