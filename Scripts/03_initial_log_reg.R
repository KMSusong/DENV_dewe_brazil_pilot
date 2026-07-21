#' --- 
#' title: "03 Initial Stepwise logistic regression" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-06" 
#' --- 

#' Overview: 
#'  initial stepwise logistic regression; test for performace

#' Timeline: 
#'   2026-07-06 
#' 
regression_data_v2 <-read_csv("00_Data/regression_data_v2.csv")
install.packages("broom.mixed")
install.packages("lme4")
install.packages("performance")
install.packages("lmtest")
library(lme4)
library(performance)
library(dplyr)
library(tidyr)
library(lmtest) 

#### ----prepare data ----
model_data <- regression_data_v2 |>
  select(
    # outcome
    outbreak,
    # random effect
    adm_1_name,
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


##climate variables:    tmax, tmax_lag1, tmax_lag2, tmax_lag3, tmin, tmin_lag1, tmin_lag2, tmin_lag3,
#pr, pr_lag1, pr_lag2, pr_lag3

# SD variables:   sd_anomaly, sd_prev_5yr, sd_anomaly_mean3

####--make log of models

model_log <- tibble(
  step             = integer(),
  model_name       = character(),
  variable_added   = character(),
  AIC              = numeric(),
  BIC              = numeric(),
  R2_marginal      = numeric(),
  R2_conditional   = numeric(),
  LRT_chisq        = numeric(),
  LRT_df           = numeric(),
  LRT_p            = numeric(),
  delta_AIC        = numeric(),
  kept             = logical(),
  notes            = character()
)


#### --- LOGGING FUNCTION ----

# Call this after testing each new variable.
# Arguments:
#   model_name    : string label e.g. "m1"
#   variable_added: string describing what was added e.g. "n_inundacoes_lag1"
#   new_model     : the glmer model object just fitted
#   prev_model    : the previous best model to compare against
#   kept          : did you decide to keep this variable? TRUE/FALSE
#   notes         : any notes e.g. "borderline, kept on biological grounds"

log_model <- function(model_name,
                      variable_added,
                      new_model,
                      prev_model,
                      kept  = TRUE,
                      notes = "") {
  
  # Extract performance metrics
  perf    <- model_performance(new_model)
  lrt     <- lrtest(prev_model, new_model)
  
  new_row <- tibble(
    step             = nrow(model_log) + 1L,
    model_name       = model_name,
    variable_added   = variable_added,
    AIC              = round(perf$AIC, 2),
    BIC              = round(perf$BIC, 2),
    R2_marginal      = round(perf$R2_marginal,    4),
    R2_conditional   = round(perf$R2_conditional, 4),
    LRT_chisq        = round(lrt$Chisq[2],        3),
    LRT_df           = lrt$Df[2],
    LRT_p            = round(lrt$`Pr(>Chisq)`[2], 4),
    delta_AIC        = round(perf$AIC - AIC(prev_model), 2),
    kept             = kept,
    notes            = notes
  )
  
  # Append to log in global environment
  model_log <<- bind_rows(model_log, new_row)
  
  # Print summary to console
  cat(sprintf("\n--- %s: %s ---\n", model_name, variable_added))
  cat(sprintf("  AIC: %.2f  (delta: %.2f)\n", perf$AIC,
              perf$AIC - AIC(prev_model)))
  cat(sprintf("  LRT: chi2=%.3f, df=%d, p=%.4f\n",
              lrt$Chisq[2], lrt$Df[2], lrt$`Pr(>Chisq)`[2]))
  cat(sprintf("  R2 marginal=%.4f, conditional=%.4f\n",
              perf$R2_marginal, perf$R2_conditional))
  cat(sprintf("  Decision: %s\n", if (kept) "KEEP" else "DROP"))
  if (notes != "") cat(sprintf("  Notes: %s\n", notes))
}


#### ----  VIEW AND EXPORT THE LOG ----

# View in console
print(model_log, n = Inf)

# Copy to clipboard for Excel (Mac)
model_log |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

# Save as CSV
write_csv(model_log, "Results/model_selection_log.csv")



##---------some functions for comparing models ----
# Compare performance metrics side-by-side in a table (change names)
compare_performance(m0,m1, rank = TRUE)

# Check Variance Inflation Factors (VIF)
# Values above 5 or 10 indicate problematic multicollinearity
check_collinearity(m0)

# Checks if your data is overdispersed
check_overdispersion(m0)

# Checks if the random effects variance is safely above zero
check_singularity(m0)






###### --- START OF MODELS ----


### --- m0 ----
m0 <- glmer(outbreak ~ 1 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

summary(m0)
model_performance(m0)

# Null model (log separately since no LRT comparison)
perf_m0 <- model_performance(m0)
model_log <- bind_rows(model_log, tibble(
  step             = 1L,
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

#### ---- m1 ------
m1 <- glmer(outbreak ~ n_inunda_lag1 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

summary(m1)

# --- Evaluate m1 ---
model_performance(m1)   # AIC, BIC, R2, ICC

# Likelihood ratio test vs null model
lrtest(m0, m1)
# p < 0.05 = m1 significantly better than m0 → keep variable

# Compare AIC directly
AIC(m0, m1)

log_model("m1", "n_inunda_lag1", m1, m0, kept = TRUE)


####---- m2 -----
m2 <- glmer(outbreak ~ n_inunda_lag1 + n_inunda_lag2 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

summary(m2)
model_performance(m2)
lrtest(m1, m2)   # compare against m1, not m0
AIC(m0, m1, m2)
log_model("m2", "n_inunda_lag2", m2, m1, kept = TRUE)


###---- m3 ----
m3 <- glmer(outbreak ~ n_inunda_lag1 + n_inunda_lag2 +
              n_inunda_lag3 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m2, m3)
AIC(m2, m3)
log_model("m3", "n_inunda_lag3", m3, m2, kept = FALSE)

#### --- m4 ----
m4 <- glmer(outbreak ~ n_inunda_lag1 + n_inunda_lag2 +
              n_inunda_lag4 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m2, m4)
AIC(m2, m4)
log_model("m4", "n_inunda_lag4", m4, m2, kept = FALSE)

##### ---- m5 -----
m5 <- glmer(outbreak ~  n_inunda_lag2 +
               (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m2, m5)
AIC(m2, m5)
log_model("m5", "remove n_inunda_lag1", m5, m2, kept = FALSE, "removed lag1, just lag2")

####---- m6 -----
m6 <- glmer(outbreak ~ n_inunda_lag2 +
              n_inunda_lag3 +
              (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m2, m6)
AIC(m2, m6)
log_model("m6", "remove n_inunda_lag1, add lag3", m6, m2, kept = FALSE, "removed lag1, add lag2 and lag3")

####---- m7 -----
m7 <- glmer(outbreak ~ n_inunda_lag1 +
              n_inunda_lag2 + n_inunda_lag5 +
              (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m2, m7)
AIC(m2, m7)
log_model("m7", "n_inunda_lag5", m7, m2, kept = TRUE)

###---- m8 ----
m8 <- glmer(outbreak ~ n_inunda_lag1 +
              n_inunda_lag2 + n_inunda_lag5 +
              n_alaga_lag1 +
              (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m7, m8)
AIC(m7, m8)
log_model("m8", "n_alaga_lag1", m8, m7, kept = TRUE)

###---- m9 ----
m9 <- glmer(outbreak ~ n_inunda_lag1 +
              n_inunda_lag2 + n_inunda_lag5 +
              n_alaga_lag1 + n_alaga_lag2 +
              (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m8, m9)
AIC(m8, m9)
log_model("m9", "n_alaga_lag2", m9, m8, kept = TRUE)

###---- m10 ----
m10 <- glmer(outbreak ~ n_inunda_lag1 +
              n_inunda_lag2 + n_inunda_lag5 +
              n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
              (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m9, m10)
AIC(m9, m10)
log_model("m10", "n_alaga_lag3", m10, m9, kept = TRUE)

###---- m11 ----
m11 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 +
               (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m10, m11)
AIC(m10, m11)

log_model("m11", "n_alaga_lag4", m11, m10, kept = TRUE)

###---- m12 ----
m12 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5
               + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m11, m12)
AIC(m11, m12)

log_model("m12", "n_alaga_lag5", m12, m11, kept = TRUE)

###---- m13 ----
m13 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
               + n_seca_lag1
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m12, m13)
AIC(m12, m13)

log_model("m13", "n_seca_lag1", m13, m12, kept = FALSE)

###---- m14 ----
m14 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
              + n_seca_lag2
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m12, m14)
AIC(m12, m14)

log_model("m14", "n_seca_lag2", m14, m12, kept = FALSE)

###---- m15 ----
m15 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag3
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m12, m15)
AIC(m12, m15)

log_model("m15", "n_seca_lag3", m15, m12, kept = FALSE)


###---- m16 ----
m16 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag4
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m12, m16)
AIC(m12, m16)

log_model("m16", "n_seca_lag4", m16, m12, kept = FALSE)

###---- m17 ----
m17 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag5
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m12, m17)
AIC(m12, m17)

log_model("m17", "n_seca_lag5", m17, m12, kept = TRUE)

###---- m18 ----
m18 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag5
             +  n_massa_lag1
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m17, m18)
AIC(m17, m18)

log_model("m18", "n_massa_lag1", m18, m17, kept = FALSE)

###---- m19 ----
m19 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag5
             +  n_massa_lag2
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m17, m19)
AIC(m17, m19)

log_model("m19", "n_massa_lag2", m19, m17, kept = TRUE)

###---- m20 ----
m20 <- glmer(outbreak ~ n_inunda_lag1 +
               n_inunda_lag2 + n_inunda_lag5 +
               n_alaga_lag1 + n_alaga_lag2 + n_alaga_lag3 +
               n_alaga_lag4 + n_alaga_lag5  
             + n_seca_lag5
             +  n_massa_lag2 + n_massa_lag3
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m19, m20)
AIC(m19, m20)

log_model("m20", "n_massa_lag3", m20, m19, kept = TRUE)




#####----- adding season to glmer -----

m21 <- glmer(outbreak ~ season_nMonth + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(m0, m21)
AIC(m0, m21)

log_model("m21", "season_nMonth", m21, m0, kept = TRUE)



m22 <- glmer(outbreak ~ season_nMonth + n_inunda_lag1 
             + (1 | adm_1_name),
             data   = model_data,
             family = binomial)

lrtest(m21, m22)
AIC(m21, m22)

log_model("m22", "n_inunda_lag1", m22, m21, kept = TRUE)
###----final m model----
final_model <- m20   # replace with your actual final model

# Odds ratios with 95% CI
exp(fixef(final_model))                      # odds ratios
exp(confint(final_model, method = "Wald"))   # confidence intervals

# Tidy output
broom.mixed::tidy(final_model, effects = "fixed", exponentiate = TRUE,
                  conf.int = TRUE)

#results of m1 to compare to climate models
exp(fixef(m1))                      # odds ratios
exp(confint(m1, method = "Wald"))   # confidence intervals


broom.mixed::tidy(m1, effects = "fixed", exponentiate = TRUE,
                  conf.int = TRUE)






#######---MODEl With CLIMATE-----
#add climate var to model_data before running
### --- m0 ----
c0 <- glmer(outbreak ~ 1 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)
# Null model (log separately since no LRT comparison)
perf_c0 <- model_performance(c0)
model_log <- bind_rows(model_log, tibble(
  step             = 22L,
  model_name       = "c0",
  variable_added   = "null (intercept + random effect)",
  AIC              = round(perf_c0$AIC, 2),
  BIC              = round(perf_c0$BIC, 2),
  R2_marginal      = round(perf_c0$R2_marginal,    4),
  R2_conditional   = round(perf_c0$R2_conditional, 4),
  LRT_chisq        = NA_real_,
  LRT_df           = NA_integer_,
  LRT_p            = NA_real_,
  delta_AIC        = NA_real_,
  kept             = TRUE,
  notes            = "baseline"
))

####---- c1 ----
c1 <- glmer(outbreak ~ n_inunda_lag1 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(c0, c1)
AIC(c0, c1)

log_model("c1", "n_inunda_lag1", c1, c0, kept = FALSE, "using only rows with climate data")

####---- c2 ----
c2 <- glmer(outbreak ~  n_inunda_lag2 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(c0, c2)
AIC(c0, c2)

log_model("c2", "n_inunda_lag2", c2, c0, kept = FALSE)

####--- c3 ---
c3 <- glmer(outbreak ~  n_inunda_lag1 + tmax_lag1 + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(c1, c3)
AIC(c1, c3)

log_model("c3", "tmax", c3, c1, kept = TRUE, "added tmax to c1 model")

c4 <- glmer(outbreak ~  n_inunda_lag1 + tmax_lag1 + tmin_lag1 + pr_lag1
            + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(c3, c4)
AIC(c3, c4)

log_model("c4", "tmin_lag1, pr_lag1", c4, c3, kept = TRUE)

c5 <- glmer(outbreak ~  n_inunda_lag1 + tmax_lag1 + tmin_lag1 + pr_lag1
            + season_month + sd_anomaly_mean3
            + (1 | adm_1_name),
            data   = model_data,
            family = binomial)

lrtest(c4, c5)
AIC(c4, c5)

log_model("c5", "season_month, sd_anomaly_mean3", c5, c4, kept = TRUE)
#results of m1 to compare to climate models
exp(fixef(c1))                      # odds ratios
exp(confint(c1, method = "Wald"))   # confidence intervals


broom.mixed::tidy(c5, effects = "fixed", exponentiate = TRUE,
                  conf.int = TRUE)


####---- (linear mixed model) ----
# Linear mixed model with sd_anomaly as outcome
##add sd var to model_data (with climate too)
m0_lmer <- lmer(sd_anomaly ~ 1 + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)   # use ML not REML for AIC comparison
perf_m0 <- model_performance(m0_lmer)
model_log <- bind_rows(model_log, tibble(
  step             = 22L,
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


m1_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

summary(m1_lmer)
model_performance(m1_lmer)
lrtest(m0_lmer, m1_lmer)
log_model("m1", "n_inunda_lag1", m1_lmer, m0_lmer, kept = TRUE)


m2_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + sd_anomaly_mean3
                 + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m2_lmer)
performance(m2_lmer)
lrtest(m1_lmer, m2_lmer)
AIC(m0_lmer, m2_lmer)
log_model("m2", "sd_anomaly_mean3", m2_lmer, m1_lmer, kept = TRUE)


m3_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + season_nMonth
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m3_lmer)
lrtest(m1_lmer, m3_lmer)
AIC(m1_lmer, m3_lmer)
log_model("m3", "season_nMonth", m2_lmer, m1_lmer, kept = TRUE, "replaced sd_anomaly_mean3 
          with season_nMonth")

m4_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + season_nMonth
                + sd_anomaly_mean3
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m4_lmer)
lrtest(m2_lmer, m4_lmer)
AIC(m2_lmer, m4_lmer)
log_model("m4", "season_nMonth add to m2", m4_lmer, m2_lmer, kept = TRUE)


m5_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag2
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m5_lmer)
lrtest(m4_lmer, m5_lmer)
AIC(m4_lmer, m5_lmer)
log_model("m5", "n_inunda_lag2", m5_lmer, m4_lmer, kept = FALSE)

m6_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag3
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m6_lmer)
lrtest(m4_lmer, m6_lmer)
AIC(m4_lmer, m6_lmer)
log_model("m6", "n_inunda_lag3", m6_lmer, m4_lmer, kept = FALSE)


m7_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag4
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m7_lmer)
lrtest(m4_lmer, m7_lmer)
AIC(m4_lmer, m7_lmer)
log_model("m7", "n_inunda_lag4", m7_lmer, m4_lmer, kept = FALSE)

m8_lmer <- lmer(sd_anomaly ~ n_inunda_lag1 + n_inunda_lag5
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(m8_lmer)
lrtest(m4_lmer, m8_lmer)
AIC(m4_lmer, m8_lmer)
log_model("m8", "n_inunda_lag5", m8_lmer, m4_lmer, kept = FALSE)

a1_lmer <- lmer(sd_anomaly ~ n_alaga_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a1_lmer)
summary(a1_lmer)
lrtest(m0_lmer, a1_lmer)
AIC(m0_lmer, a1_lmer)
log_model("a1", "n_alaga_lag1", a1_lmer, m0_lmer, kept = TRUE)

a2_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + n_alaga_lag2
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a2_lmer)
summary(a2_lmer)
lrtest(a1_lmer, a2_lmer)
AIC(a1_lmer, a2_lmer)
log_model("a2", "n_alaga_lag2", a2_lmer, a1_lmer, kept = FALSE)

a3_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + n_alaga_lag3
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a3_lmer)
summary(a3_lmer)
lrtest(a1_lmer, a3_lmer)
AIC(a1_lmer, a3_lmer)
log_model("a3", "n_alaga_lag3", a3_lmer, a1_lmer, kept = FALSE)

a4_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + n_alaga_lag4
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a4_lmer)
summary(a4_lmer)
lrtest(a1_lmer, a4_lmer)
AIC(a1_lmer, a4_lmer)
log_model("a4", "n_alaga_lag4", a4_lmer, a1_lmer, kept = FALSE)


a5_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + n_alaga_lag5
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a5_lmer)
summary(a5_lmer)
lrtest(a1_lmer, a5_lmer)
AIC(a1_lmer, a5_lmer)
log_model("a5", "n_alaga_lag5", a5_lmer, a1_lmer, kept = FALSE)

s1_lmer <- lmer(sd_anomaly ~ n_seca_lag1 
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s1_lmer)
summary(s1_lmer)
lrtest(m0_lmer, s1_lmer)
AIC(m0_lmer, s1_lmer)
log_model("s1", "n_seca_lag1", s1_lmer, m0_lmer, kept = TRUE)

s2_lmer <- lmer(sd_anomaly ~ n_seca_lag1 + n_seca_lag2 
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s2_lmer)
summary(s2_lmer)
lrtest(s1_lmer, s2_lmer)
AIC(s1_lmer, s2_lmer)
log_model("s2", "n_seca_lag2", s2_lmer, s1_lmer, kept = FALSE)

s3_lmer <- lmer(sd_anomaly ~ n_seca_lag1 + n_seca_lag3 
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s3_lmer)
summary(s3_lmer)
lrtest(s1_lmer, s3_lmer)
AIC(s1_lmer, s3_lmer)
log_model("s3", "n_seca_lag3", s3_lmer, s1_lmer, kept = FALSE)

s4_lmer <- lmer(sd_anomaly ~ n_seca_lag1 + n_seca_lag4 
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s4_lmer)
summary(s4_lmer)
lrtest(s1_lmer, s4_lmer)
AIC(s1_lmer, s4_lmer)
log_model("s4", "n_seca_lag4", s4_lmer, s1_lmer, kept = FALSE)


s5_lmer <- lmer(sd_anomaly ~ n_seca_lag1 + n_seca_lag5
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s5_lmer)
summary(s5_lmer)
lrtest(s1_lmer, s5_lmer)
AIC(s1_lmer, s5_lmer)
log_model("s5", "n_seca_lag5", s5_lmer, s1_lmer, kept = FALSE)

s6_lmer <- lmer(sd_anomaly ~ n_seca_lag1 + n_massa_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s6_lmer)
summary(s6_lmer)
lrtest(s1_lmer, s6_lmer)
AIC(s1_lmer, s6_lmer)
log_model("s6", "n_massa_lag1", s6_lmer, s1_lmer, kept = FALSE)

s7_lmer <- lmer(sd_anomaly ~ n_seca_lag1*n_massa_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(s7_lmer)
summary(s7_lmer)
lrtest(s6_lmer, s7_lmer)
AIC(s6_lmer, s7_lmer)
log_model("s7", "n_massa_lag1 as interaction", s7_lmer, s6_lmer, kept = FALSE, "add 
          massa1 as interaction with seca1")


####mass movement

d1_lmer <- lmer(sd_anomaly ~ n_massa_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(d1_lmer)
summary(d1_lmer)
lrtest(m0_lmer, d1_lmer)
AIC(m0_lmer, d1_lmer)
log_model("d1", "n_massa_lag1", d1_lmer, m0_lmer, kept = TRUE)


d2_lmer <- lmer(sd_anomaly ~ n_massa_lag1 + n_massa_lag2
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(d2_lmer)
summary(d2_lmer)
lrtest(d1_lmer, d2_lmer)
AIC(d1_lmer, d2_lmer)
log_model("d2", "n_massa_lag2", d2_lmer, d1_lmer, kept = FALSE)

d3_lmer <- lmer(sd_anomaly ~ n_massa_lag1 + n_massa_lag3
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(d3_lmer)
summary(d3_lmer)
lrtest(d1_lmer, d3_lmer)
AIC(d1_lmer, d3_lmer)
log_model("d3", "n_massa_lag3", d3_lmer, d1_lmer, kept = FALSE)

#######---- testing interactions with lmer ----

a6_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + n_massa_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a6_lmer)
summary(a6_lmer)
lrtest(a1_lmer, a6_lmer)
AIC(a1_lmer, a6_lmer)
log_model("a6", "n_massa_lag1", a6_lmer, a1_lmer, kept = FALSE, "add massa1 to alaga1")

a7_lmer <- lmer(sd_anomaly ~ n_alaga_lag1*n_massa_lag1
                + season_nMonth
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(a7_lmer)
summary(a7_lmer)
lrtest(a6_lmer, a7_lmer)
AIC(a6_lmer, a7_lmer)
log_model("a7", "n_massa_lag1 as interaction", a7_lmer, a6_lmer, kept = FALSE, "add massa1 to alaga1 as interaction")

####test new climate variables######

c0_lmer <- lmer(sd_anomaly ~ 1
                + season_month
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(c0_lmer)
summary(c0_lmer)
lrtest(c0_lmer, c0_lmer)
AIC(c0_lmer, c0_lmer)
log_model("c0", "baseline with new clim data", c0_lmer, c0_lmer, kept = TRUE, "new baseline model using new clim data")

c1_lmer <- lmer(sd_anomaly ~ n_alaga_lag1
                + season_month
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(c1_lmer)
summary(c1_lmer)
lrtest(c0_lmer, c1_lmer)
AIC(c0_lmer, c1_lmer)
log_model("c1", "n_alaga_lag1", c1_lmer, c0_lmer, kept = TRUE)

c2_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + pr_lag1
                + season_month
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(c2_lmer)
summary(c2_lmer)
lrtest(c1_lmer, c2_lmer)
AIC(c1_lmer, c2_lmer)
log_model("c2", "pr_lag1", c2_lmer, c1_lmer, kept = TRUE)

c3_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + pr_lag1 + tmin_lag1
                + season_month
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(c3_lmer)
summary(c3_lmer)
lrtest(c2_lmer, c3_lmer)
AIC(c2_lmer, c3_lmer)
log_model("c3", "tmin_lag1", c3_lmer, c2_lmer, kept = TRUE)

c4_lmer <- lmer(sd_anomaly ~ n_alaga_lag1 + pr_lag1 + tmin_lag1 + tmax_lag1
                + season_month
                + sd_anomaly_mean3 
                + (1 | adm_1_name),
                data   = model_data,
                REML   = FALSE)

model_performance(c4_lmer)
summary(c4_lmer)
lrtest(c3_lmer, c4_lmer)
AIC(c3_lmer, c4_lmer)
log_model("c4", "tmax_lag1", c4_lmer, c3_lmer, kept = TRUE)

# Final model: refit with REML = TRUE for best parameter estimates
final_lmer <- lmer(sd_anomaly ~ n_massa_lag1 + n_massa_lag2 + n_massa_lag3 
            
                   + pr_lag1 + tmin_lag1 + tmax_lag1
                   + season_month
                   + sd_anomaly_mean3 
                   + (1 | adm_1_name),
                   data   = model_data,
                   REML   = TRUE)

summary(final_lmer)
model_performance(final_lmer)

# Coefficients (interpreted as change in SDs from mean, not odds ratios)
fixef(final_lmer)
confint(final_lmer, method = "Wald")
broom.mixed::tidy(final_lmer,effects = "fixed",  exponentiate = FALSE,
                  conf.int = F, p.value = T)
broom.mixed::tidy(m2_lmer)
summary(m1_lmer)
# Check residuals
plot(final_lmer)                      # residuals vs fitted
qqnorm(resid(final_lmer))             # normality of residuals
qqline(resid(final_lmer), col = "red")
