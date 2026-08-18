###dengue case count map and graph
# =============================================================================
# 1. TOTAL DENGUE CASES BY STATE MAP
# =============================================================================
regression_data <-read_csv("00_Data/regression_data_v4.csv")
merged_data <-read_csv("00_Data/merged_week_month.csv")
dengue_by_state <- regression_data |>
  filter(!is.na(adm_1_name)) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    total_cases      = sum(dengue_total, na.rm = TRUE),
    mean_monthly     = round(mean(dengue_total, na.rm = TRUE), 1),
    n_municipalities = n_distinct(IBGE_code),
    .groups = "drop"
  )

map_dengue_state <- states_sf |>
  left_join(dengue_by_state, by = c("abbrev_state" = "sigla_uf"))

library(ggspatial)
install.packages("shadowtext")
library(shadowtext)

# State label points
state_labels <- map_dengue_state |>
  st_point_on_surface()

p_total_cases_map <- ggplot(map_dengue_state) +
  
  geom_sf(
    aes(fill = total_cases),
    colour = "white",
    linewidth = 0.3
  ) +
  
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    trans = "log1p",
    na.value = "grey80",
    name = "Total cases",
    breaks = c(
      25000,
      50000,
      100000,
      250000,
      500000,
      1000000,
      2000000,
      3000000
    ),
    labels = scales::comma,
    oob = scales::squish,
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(5.5, "cm"),
      barwidth = unit(0.8, "cm"),
      ticks = TRUE,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  
  # State abbreviations with white halo
  geom_shadowtext(
    data = state_labels,
    aes(
      label = abbrev_state,
      geometry = geometry
    ),
    stat = "sf_coordinates",
    colour = "grey15",
    bg.colour = "white",
    bg.r = 0.18,
    size = 2.5,
    fontface = "bold"
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.4, "cm"),
    pad_y = unit(1.8, "cm"),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  
  labs(
    title = "Total dengue cases by Brazilian state 2006 – 2023",
    subtitle = "Log-transformed scale."
  ) +
  
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

p_total_cases_map



# =============================================================================
# 2. TOTAL DENGUE CASES OVER TIME: NATIONAL TIME SERIES
# =============================================================================

national_timeseries <- merged_data |>
  filter(!is.na(dengue_total)) |>
  group_by(join_year, join_month) |>
  summarise(
    total_cases     = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-")))

# Annual totals for bar chart overlay
annual_totals <- merged_data |>
  filter(
    join_year %in% 2006:2023,
    !is.na(dengue_total)
  ) |>
  group_by(join_year) |>
  summarise(
    total_cases = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  )

# --- Monthly time series line plot ---
p_timeseries <- ggplot(national_timeseries, aes(x = date, y = total_cases)) +
  # Shade outbreak months
  geom_col(aes(fill = n_outbreak_months > 0),
           width = 31, alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "grey90", "TRUE" = "#fc8d59")) +
  geom_line(colour = "#2166ac", linewidth = 0.6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Total monthly dengue cases, Brazil (2006–2023)",
    subtitle = "Orange shading = months where ≥1 municipality classified as outbreak",
    x        = NULL,
    y        = "Total dengue cases"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# --- Annual bar chart ---
p_annual <- ggplot(annual_totals, aes(x = join_year, y = total_cases)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_text(aes(label = scales::comma(total_cases)),
            vjust = -0.4, size = 2.8) +
  scale_x_continuous(breaks = min(annual_totals$join_year):max(annual_totals$join_year)) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Annual total dengue cases, Brazil (2006 – 2023)",
    x     = NULL,
    y     = "Total dengue cases"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# --- Combined figure ---
p_combined <- p_timeseries / p_annual +
  plot_annotation(
    title = "Dengue case burden over time, Brazil",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

p_combined
p_annual
# =============================================================================
# 3. SAVE
# =============================================================================

ggsave("Submission/map_total_dengue_by_state.png",
       p_total_cases_map, width = 8, height = 7, dpi = 150)

ggsave("outputs/plot_dengue_timeseries.png",
       p_timeseries, width = 12, height = 5, dpi = 150)

ggsave("outputs/plot_dengue_annual.png",
       p_annual, width = 10, height = 5, dpi = 150)

ggsave("outputs/plot_dengue_combined.png",
       p_combined, width = 12, height = 10, dpi = 150)

cat("Saved all plots to outputs/\n")