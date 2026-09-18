#check lmer model
# Residuals vs fitted (checks linearity and homoscedasticity)
plot(final_clim)

# Normality of residuals
qqnorm(resid(final_clim))
qqline(resid(final_clim), col = "red")

# Normality of random effects
ranef_vals <- ranef(final_clim)
qqnorm(ranef_vals$`IBGE_code:adm_1_name`[[1]])
qqline(ranef_vals$`IBGE_code:adm_1_name`[[1]], col = "red")

# Residuals over time (checks temporal autocorrelation)
model_data |>
  filter(!is.na(tmin_lag1), !is.na(tmax_lag2), !is.na(pr_lag1)) |>
  mutate(residual = resid(final_clim)) |>
  group_by(join_year, join_month) |>
  summarise(mean_resid = mean(residual, na.rm = TRUE), .groups = "drop") |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-"))) |>
  ggplot(aes(x = date, y = mean_resid)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Mean residuals over time", x = NULL, y = "Mean residual") +
  theme_bw()