#' --- 
#' title: "02 map of dengue and outbreak by municipality" 
#' author: "Natalia Cuartas" 
#' date: "2026-08-05" 
#' --- 

#' Overview: 
#'  create a map that shows the number of municipalites with full data,
#'  and the number of dengue cases and outbreaks by municipality

#' Timeline: 
#'   2026-07-04 

library(geobr)
library(sf)
library(patchwork)
library(ggspatial)
library(viridis)
regression_data <-read_csv("00_Data/regression_data_v4.csv")
merged_data <-read_csv("00_Data/merged_week_month.csv")

#Load shapefile of municipalities

mun_sf <- read_municipality(year = 2020, showProgress = FALSE)
mun_sf <- mun_sf |>
  mutate(code_muni = as.character(code_muni))

###---Prepare map data

map_data <- regression_data |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  summarise(
    n_months              = n(),
    total_cases           = sum(dengue_total,       na.rm = TRUE),
    n_outbreak_months     = sum(outbreak,           na.rm = TRUE),
    pct_outbreak_months   = round(100 * n_outbreak_months / n_months, 1),
    mean_sd_anomaly       = round(mean(sd_anomaly[outbreak == TRUE],
                                       na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  mutate(
    IBGE_code     = as.character(IBGE_code),
    full_coverage = n_months == 216   # TRUE if municipality has all 216 months
  )
###alternate with merged dataset
map_data <- merged_data |>
  filter(join_year >= 2006, join_year <= 2023) |>
  group_by(IBGE_code, adm_2_name, adm_1_name, sigla_uf) |>
  summarise(
    n_months    = n(),
    total_cases = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    IBGE_code     = as.character(IBGE_code),
    full_coverage = n_months == 216
  )

# Join to shapefile
# geobr municipality codes are 7 digits; IBGE_code may need trimming
mun_map <- mun_sf |>
  left_join(map_data, by = c("code_muni" = "IBGE_code"))

####----Map 1: Data Coverage----####
p_coverage <- mun_map |>
  mutate(
    coverage_group = case_when(
      is.na(n_months)   ~ "No data",
      full_coverage     ~ "Full coverage (216 months)",
      n_months >= 180   ~ "≥180 months",
      n_months >= 120   ~ "120–179 months",
      n_months >= 60    ~ "60–119 months",
      TRUE              ~ "<60 months"
    ) |>
      factor(levels = c(
        "Full coverage (216 months)",
        "≥180 months",
        "120–179 months",
        "60–119 months",
        "<60 months",
        "No data"
      ))
  ) |>
  ggplot() +
  geom_sf(
    aes(fill = coverage_group),
    colour = NA
  ) +
  scale_fill_manual(
    values = c(
      "Full coverage (216 months)" = viridis(5)[1],
      "≥180 months"                = viridis(5)[2],
      "120–179 months"             = viridis(5)[3],
      "60–119 months"              = viridis(5)[4],
      "<60 months"                 = viridis(5)[5],
      "No data"                    = "grey80"
    ),
    name = "Data coverage",
    drop = FALSE
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
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
    title = "Dengue Data Coverage by Brazilian Municipality 2006 – 2023",
    subtitle = "Full coverage = 216 municipality-months (18 years × 12 months)"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

p_coverage
#####----Map 2: Dengue Case Burden-----####

p_cases <- ggplot(mun_map) +
  geom_sf(
    aes(fill = total_cases),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    trans = "log1p",
    na.value = "grey80",
    name = "Total cases",
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::comma,
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(5, "cm"),
      barwidth = unit(0.7, "cm")
    )
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
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
    title = "Total Dengue Cases by Brazilian Municipality 2006 - 2023",
    subtitle = "Log-transformed scale. Grey = no data"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
p_cases
#####----Map 3: outbreak burden----####

p_outbreaks <- ggplot(mun_map) +
  geom_sf(
    aes(fill = n_outbreak_months),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey80",
    name = "Outbreak months",
    labels = scales::comma
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
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
    title = "Number of Outbreak Months by Brazilian Municipality 2006 – 2023",
    subtitle = "Grey = no data or insufficient history for threshold calculation"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

p_outbreaks
#####----Percent of Months Outbreak----####

p_pct_outbreak <- ggplot(mun_map) +
  geom_sf(aes(fill = pct_outbreak_months), colour = NA) +
  scale_fill_gradient(
    low      = "#ffffcc",
    high     = "#800026",
    na.value = "grey80",
    name     = "% months\nin outbreak",
    labels   = scales::label_percent(scale = 1)
  ) +
  labs(
    title    = "Percentage of months in outbreak by municipality",
    subtitle = "Adjusts for municipalities with shorter data coverage",
    caption  = "Source: Dengue surveillance data, Brazil"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "right"
  )

####----combine the maps----####

p_combined <- (p_coverage | p_cases) / (p_outbreaks | p_pct_outbreak) +
  plot_annotation(
    title   = "Dengue data coverage and burden by Brazilian municipality",
    theme   = theme(plot.title = element_text(face = "bold", size = 13))
  )

p_combined

####---check numbers---####

cat("=== Data coverage summary ===\n")
map_data |>
  mutate(coverage_group = case_when(
    full_coverage   ~ "Full (216 months)",
    n_months >= 180 ~ ">=180 months",
    n_months >= 120 ~ "120-179 months",
    n_months >= 60  ~ "60-119 months",
    TRUE            ~ "<60 months"
  )) |>
  count(coverage_group) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(desc(n)) |>
  print()

cat("\nMunicipalities with full coverage:", sum(map_data$full_coverage), "\n")
cat("Total municipalities:", nrow(map_data), "\n")
cat("% with full coverage:",
    round(100 * mean(map_data$full_coverage), 1), "%\n")

#####----Save Maps----#####

ggsave("Submission/map_mun_data_coverage.png",
       p_coverage, width = 8, height = 9, dpi = 150, bg="white")

ggsave("Submission/map_mun_dengue_cases.png",
       p_cases, width = 8, height = 9, dpi = 150,bg="white")

ggsave("Submission/map_mun_outbreak_months.png",
       p_outbreaks, width = 8, height = 9, dpi = 150,bg="white")

ggsave("Submission/map_mun_outbreak_pct.png",
       p_pct_outbreak, width = 8, height = 9, dpi = 150)

ggsave("Submission/map_combined.png",
       p_combined, width = 16, height = 14, dpi = 150)

cat("\nSaved all maps to outputs/\n")