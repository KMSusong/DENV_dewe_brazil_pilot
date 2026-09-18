#' --- 
#' title: "02 map of disasters and outbreak by state" 
#' author: "Natalia Cuartas" 
#' date: "2026-07-04" 
#' --- 

#' Overview: 
#'  create a map that shows the number of total outbreaks and disaster events in brazil
#'  by state, both individually and together in a 2D gradient map

#' Timeline: 
#'   2026-07-04 
#' 2026-08-04 edit maps to make them publishing ready

install.packages("geobr")
install.packages("patchwork")
install.packages("patchwork")
install.packages("biscale")
install.packages("cowplot")
install.packages("ggspatial")   # if needed
install.packages("viridis")

library(tidyverse)
library(geobr)     # Brazil shapefiles - install.packages("geobr")
library(sf)
library(ggplot2)
library(patchwork)
library(biscale)
library(cowplot)
library(ggspatial)
library(viridis)

disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")
merged <- read_csv("00_Data/merged_week_month.csv")
adm2_outbreak <-read_csv("00_Data/adm2_outbreak_data.csv")
outbreak_5 <- read_csv("00_Data/5_year_outbreak_data.csv")


state_crosswalk <- tribble(
  ~adm_1_name,          ~RNE_iso_code, ~sigla_uf,
  "ACRE",               "BR-AC",       "AC",
  "ALAGOAS",            "BR-AL",       "AL",
  "AMAPA",              "BR-AP",       "AP",
  "AMAZONAS",           "BR-AM",       "AM",
  "BAHIA",              "BR-BA",       "BA",
  "CEARA",              "BR-CE",       "CE",
  "DISTRITO FEDERAL",   "BR-DF",       "DF",
  "ESPIRITO SANTO",     "BR-ES",       "ES",
  "GOIAS",              "BR-GO",       "GO",
  "MARANHAO",           "BR-MA",       "MA",
  "MATO GROSSO",        "BR-MT",       "MT",
  "MATO GROSSO DO SUL", "BR-MS",       "MS",
  "MINAS GERAIS",       "BR-MG",       "MG",
  "PARA",               "BR-PA",       "PA",
  "PARAIBA",            "BR-PB",       "PB",
  "PARANA",             "BR-PR",       "PR",
  "PERNAMBUCO",         "BR-PE",       "PE",
  "PIAUI",              "BR-PI",       "PI",
  "RIO DE JANEIRO",     "BR-RJ",       "RJ",
  "RIO GRANDE DO NORTE","BR-RN",       "RN",
  "RIO GRANDE DO SUL",  "BR-RS",       "RS",
  "RONDONIA",           "BR-RO",       "RO",
  "RORAIMA",            "BR-RR",       "RR",
  "SANTA CATARINA",     "BR-SC",       "SC",
  "SAO PAULO",          "BR-SP",       "SP",
  "SERGIPE",            "BR-SE",       "SE",
  "TOCANTINS",          "BR-TO",       "TO"
)



# Dengue outbreak data date range
outbreak_5 |>
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_years  = n_distinct(join_year)
  )

# Disaster data date range
disasters_plot |>
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_years  = n_distinct(year(date))
  )

# Overlapping range (what both datasets share)
cat("Overlapping range:\n")
cat(sprintf("  From: %s\n", max(min(outbreak_5$date, na.rm = TRUE),
                                min(disasters_plot$date, na.rm = TRUE))))
cat(sprintf("  To:   %s\n", min(max(outbreak_5$date, na.rm = TRUE),
                                max(disasters_plot$date, na.rm = TRUE))))

#filter outbreak 5 to disaster dates
outbreak_5_filtered <- outbreak_5 %>%
  filter(
    date >= min(disasters_plot$date, na.rm = TRUE),
    date <= max(disasters_plot$date, na.rm = TRUE)
  )

outbreak_5_filtered |>
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_years  = n_distinct(join_year)
  )

#load brazil shapefile
states_sf <- read_state(year = 2020, showProgress = FALSE)

#outbreak totals by state
outbreak_totals <- outbreak_5_filtered |>
  filter(!is.na(adm_1_name), !is.na(outbreak)) |>
  left_join(
    state_crosswalk |> select(adm_1_name, sigla_uf),
    by = "adm_1_name"
  ) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    n_outbreak_months = sum(outbreak,      na.rm = TRUE),
    n_total_months    = n(),
    pct_outbreak      = round(100 * n_outbreak_months / n_total_months, 1),
    .groups = "drop"
  )

disaster_totals <- disasters_plot |>
  filter(!is.na(adm_1_name)) |>
  left_join(
    state_crosswalk |> select(adm_1_name, sigla_uf),
    by = "adm_1_name"
  ) |>
  filter(!is.na(sigla_uf)) |>
  group_by(adm_1_name, sigla_uf, descricao_tipologia) |>
  summarise(n_events = n(), .groups = "drop")

# Function to get totals for a specific type
get_disaster_totals <- function(search_term, data = disaster_totals) {
  data |>
    filter(str_detect(tolower(descricao_tipologia), tolower(search_term))) |>
    group_by(adm_1_name, sigla_uf) |>
    summarise(n_events = sum(n_events), .groups = "drop")
}

#join to shapefile
# geobr uses abbrev_state as the state abbreviation column
map_outbreaks <- states_sf |>
  left_join(outbreak_totals, by = c("abbrev_state" = "sigla_uf"))

# Function to join disaster totals to shapefile
join_disasters_to_map <- function(search_term) {
  states_sf |>
    left_join(
      get_disaster_totals(search_term),
      by = c("abbrev_state" = "sigla_uf")
    )
}

#change CRS projection to ESRI:102033 – South America Albers Equal Area


map_data <- map_outbreaks |>
  st_transform(5641)

state_labels <- map_data |>
  st_point_on_surface()

# --- 5a. Outbreak map ---
plot_outbreak_map <- function(variable = "n_outbreak_months",
                              label = "Outbreak months") {
  
  ggplot(map_data) +
    
    geom_sf(aes(fill = .data[[variable]]),
            colour = "grey75",
            linewidth = 0.2) +
    
    geom_sf_text(
      data = state_labels,
      aes(label = abbrev_state),
      size = 2.8,
      fontface = "bold"
    ) +
    
    scale_fill_viridis_c(
      option = "C",
      direction = -1,
      na.value = "grey90",
      name = label,
      labels = scales::comma
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.30,
      text_cex = 0.8
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
      title = "Dengue Outbreak Months by Brazilian State (2001–2023)",
      subtitle = "Outbreak defined as monthly dengue incidence > mean + 1.25 SD of the previous 5-year monthly mean",
      fill = "Outbreak\nmonths"
    ) +
    
    coord_sf() +
    
    theme_minimal(base_size = 12) +
    
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      
      legend.position = c(0.88, 0.28),
      legend.background = element_rect(fill = alpha("white", 0.85)),
      legend.title = element_text(face = "bold")
      
    )
}

# --- 5b. Disaster map ---
plot_disaster_map <- function(search_term,
                              title_label = NULL) {
  
  display_label <- if (!is.null(title_label))
    title_label else search_term
  
  map_data <- join_disasters_to_map(search_term) |>
    st_transform(5641)
  
  
  ggplot(map_data) +
    
    geom_sf(aes(fill = n_events),
            colour = "grey75",
            linewidth = 0.2) +
    
    geom_sf_text(aes(label = abbrev_state),
                 size = 2.8,
                 colour = "black",
                 fontface = "bold") +
    
    scale_fill_viridis_c(
      option = "D",
      direction = -1,
      na.value = "grey90",
      name = "Events",
      labels = scales::comma
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.30,
      text_cex = 0.8
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
      title = paste0("Natural Disaster Events by Brazilian State (2001–2023): ", display_label),
      fill = "Events"
    ) +
    
    coord_sf() +
    
    theme_minimal(base_size = 12) +
    
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(face = "bold", size = 16),
      
      legend.position = c(0.88, 0.28),
      legend.background = element_rect(fill = alpha("white", 0.85)),
      legend.title = element_text(face = "bold"),
      
    )
}


# Individual maps
plot_outbreak_map()
plot_outbreak_map("pct_outbreak", "% months as outbreak")

plot_disaster_map("inunda", title_label = "Floods")
plot_disaster_map("seca",   title_label = "Drought")
plot_disaster_map("alaga",  title_label = "Urban Floods")



# Save
ggsave("Dev_Figures/map_pct_outbreaks.png",
       plot_outbreak_map("pct_outbreak", "% months as outbreak"), width = 8, height = 7, dpi = 150)
ggsave("Dev_Figures/map_outbreaks.png",
       plot_outbreak_map(), width = 8, height = 7, dpi = 150)
#save publish ready maps
ggsave(
  "Submission/OutbreakMap.png",
  plot_outbreak_map(),
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  "Submission/DisasterMap_Drought.png",
  plot_disaster_map("seca", "Drought"),
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  "Submission/DisasterMap_Flood.png",
  plot_disaster_map("inunda", "Floods"),
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  "Submission/DisasterMap_Urb_Flood.png",
  plot_disaster_map("alaga", "Urban Floods"),
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  "Submission/DisasterMap_Mass.png",
  plot_disaster_map("massa", "Mass Movement"),
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)

######-----Bivariate cloropleth map

#combine data

bivar_data <- states_sf |>
  left_join(outbreak_totals, by = c("abbrev_state" = "sigla_uf")) |>
  left_join(
    get_disaster_totals("inunda"),
    by = c("abbrev_state" = "sigla_uf")
  ) |>
  rename(n_disasters = n_events) |>
  # bi_class() splits each variable into 3 quantile bins and combines them
  # into a single classification variable (e.g. "1-1", "2-3", "3-3")
  bi_class(
    x       = n_disasters,
    y       = n_outbreak_months,
    style   = "quantile",   # or "equal", "fisher", "jenks"
    dim     = 3             # 3x3 grid = 9 colour combinations
  )



#map function
plot_bivariate_map <- function(search_term,
                               title_label = NULL,
                               pal = "DkBlue",
                               style = "quantile") {
  
  display_label <- if (!is.null(title_label))
    title_label else search_term
  
  ## Build data
  bivar_data <- states_sf |>
    left_join(outbreak_totals,
              by = c("abbrev_state" = "sigla_uf")) |>
    left_join(
      get_disaster_totals(search_term),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    rename(n_disasters = n_events) |>
    mutate(
      n_disasters = replace_na(n_disasters, 0),
      n_outbreak_months = replace_na(n_outbreak_months, 0)
    ) |>
    bi_class(
      x = n_disasters,
      y = n_outbreak_months,
      style = style,
      dim = 3
    ) |>
    st_transform(5641)
  
  ## Label positions
  state_labels <- st_point_on_surface(bivar_data)
  
  ## Legend
  legend <- bi_legend(
    pal = pal,
    dim = 3,
    xlab = paste("More", display_label),
    ylab = "More Outbreaks",
    size = 6
  )
  
  ## Map
  map <- ggplot(bivar_data) +
    
    geom_sf(
      aes(fill = bi_class),
      colour = "grey70",
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    
    bi_scale_fill(pal = pal, dim = 3) +
    
    geom_sf_text(
      data = state_labels,
      aes(label = abbrev_state),
      size = 2.8,
      colour = "black",
      fontface = "bold"
    ) +
    
    annotation_north_arrow(
      location = "bl",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      pad_x = unit(0.4, "cm"),
      pad_y = unit(1.8, "cm"),
      width = unit(0.9, "cm"),
      height = unit(0.9, "cm")
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      pad_x = unit(0.4, "cm"),
      pad_y = unit(0.3, "cm"),
      text_cex = 0.7
    ) +
    
    labs(
      title = paste0(
        "Dengue Outbreak Months and ",
        display_label,
        " Events\nby Brazilian State (2001–2023)"
      ) ) +
    
    coord_sf() +
    
    theme_minimal(base_size = 12) +
    
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        size = 16
      )
    ) 
  
  ggdraw() +
    draw_plot(map,
              x = 0,
              y = 0,
              width = 1,
              height = 1) +
    draw_plot(
      legend,
      x = 0.60,
      y = 0.05,
      width = 0.18,
      height = 0.18
    )
}

# Usage:
plot_bivariate_map("inunda", title_label = "Flooding", pal = "BlueGold")
plot_bivariate_map("seca",   title_label = "Drought", pal = "BlueGold")
plot_bivariate_map("massa",  title_label = "Mass Movement", pal = "BlueGold")
plot_bivariate_map("alaga",  title_label = "Urban Flood", pal = "BlueGold")


ggsave("Submission/bivariate_map_urb_flood.png",
       plot_bivariate_map("alaga",  title_label = "Urban Flooding", pal = "BlueGold"), 
       width = 10, height = 8, dpi = 300,  bg = "white")

ggsave("Submission/bivariate_map_flood.png",
       plot_bivariate_map("inunda",  title_label = "Flooding", pal = "BlueGold"), 
       width = 10, height = 8, dpi = 300,  bg = "white")

ggsave("Submission/bivariate_map_drought.png",
       plot_bivariate_map("seca",  title_label = "Drought", pal = "BlueGold"), 
       width = 10, height = 8, dpi = 300,  bg = "white")

ggsave("Submission/bivariate_map_mass.png",
       plot_bivariate_map("mass",  title_label = "Mass Movement", pal = "BlueGold"), 
       width = 10, height = 8, dpi = 300, bg = "white")







######----Edit so using regression data----####
regression_data <-read_csv("00_Data/regression_data_v4.csv")

outbreak_prop_state <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    n_outbreak_months = sum(outbreak,  na.rm = TRUE),
    n_total_months    = n(),
    pct_outbreak      = round(100 * n_outbreak_months / n_total_months, 1),
    .groups = "drop"
  )

map_outbreak_prop <- states_sf |>
  left_join(outbreak_prop_state, by = c("abbrev_state" = "sigla_uf"))

##map proportion of outbreak months

p_outbreak_prop <- ggplot(map_outbreak_prop) +
  geom_sf(
    aes(fill = pct_outbreak),
    colour = "white",
    linewidth = 0.3
  ) +
  
  scale_fill_viridis_c(
    option   = "magma",
    direction = -1,
    na.value = "grey80",
    name     = "% months\nin outbreak",
    labels   = scales::label_percent(scale = 1)
  ) +
  
  geom_sf_text(
    aes(
      label = paste0(
        abbrev_state,
        "\n",
        pct_outbreak,
        "%"
      )
    ),
    size = 2,
    colour = "grey20"
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm"),
    text_cex = 0.7,
    line_width = 0.5
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
    title = "Proportion of municipality-months classified as outbreaks by state (2006 - 2023)",
    subtitle = paste0(
      "Outbreak defined as dengue > mean + 1.25 SD ",
      "of preceding 5-year monthly mean (minimum 10 cases)."
    )
  ) +
  
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


p_outbreak_prop

###bivariate map

# Recalculate outbreak totals from regression_data
outbreak_totals_reg <- regression_data |>
  filter(!is.na(outbreak)) |>
  group_by(adm_1_name, sigla_uf) |>
  summarise(
    n_outbreak_months   = sum(outbreak,      na.rm = TRUE),
    n_total_months      = n(),
    pct_outbreak        = round(100 * n_outbreak_months / n_total_months, 1),
    .groups = "drop"
  )

# Recalculate disaster totals from regression_data date range
disaster_date_min <- min(regression_data$calendar_start_date, na.rm = TRUE)
disaster_date_max <- max(regression_data$calendar_start_date, na.rm = TRUE)

disaster_totals_reg <- disasters_plot |>
  filter(
    !is.na(adm_1_name),
    date >= disaster_date_min,
    date <= disaster_date_max
  ) |>
  left_join(
    state_crosswalk |> select(adm_1_name, sigla_uf),
    by = "adm_1_name"
  ) |>
  group_by(sigla_uf) |>
  summarise(n_events = n(), .groups = "drop")

# Updated get_disaster_totals function using regression data date range
get_disaster_totals_reg <- function(search_term) {
  disasters_plot |>
    filter(
      !is.na(adm_1_name),
      str_detect(tolower(descricao_tipologia), tolower(search_term)),
      date >= disaster_date_min,
      date <= disaster_date_max
    ) |>
    left_join(
      state_crosswalk |> select(adm_1_name, sigla_uf),
      by = "adm_1_name"
    ) |>
    group_by(sigla_uf) |>
    summarise(n_events = n(), .groups = "drop")
}

# Updated bivariate map function
plot_bivariate_map_reg <- function(search_term,
                                   title_label = NULL,
                                   pal         = "BlueGold",
                                   style       = "quantile") {
  
  display_label <- if (!is.null(title_label)) title_label else search_term
  
  bivar_data <- states_sf |>
    left_join(
      outbreak_totals_reg |> select(sigla_uf, n_outbreak_months),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    left_join(
      get_disaster_totals_reg(search_term),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    rename(n_disasters = n_events) |>
    mutate(
      n_disasters       = replace_na(n_disasters, 0),
      n_outbreak_months = replace_na(n_outbreak_months, 0)
    ) |>
    bi_class(
      x     = n_disasters,
      y     = n_outbreak_months,
      style = style,
      dim   = 3
    )
  
  map <- ggplot(bivar_data) +
    geom_sf(
      aes(fill = bi_class),
      colour = "white",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    bi_scale_fill(pal = pal, dim = 3) +
    
    geom_sf_text(
      aes(label = abbrev_state),
      size = 2,
      colour = "grey20"
    ) +
    
    # Scale bar
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      pad_x = unit(0.3, "cm"),
      pad_y = unit(0.3, "cm"),
      text_cex = 0.7,
      line_width = 0.5
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
      title = paste0(
        "Dengue outbreaks vs ", display_label,
        " events by state (2006 – 2023)"
      ),
      subtitle = "Color shows combination of disaster frequency and outbreak frequency"
    ) +
    
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  legend <- bi_legend(
    pal  = pal,
    dim  = 3,
    xlab = paste0("More ", display_label),
    ylab = "More outbreaks",
    size = 8
  )
  
  ggdraw() +
    draw_plot(
      map,
      x = 0,
      y = 0,
      width = 0.8,
      height = 1
    ) +
    draw_plot(
      legend,
      x = 0.75,
      y = 0.05,
      width = 0.25,
      height = 0.25
    )
}

###combining the maps
plot_bivariate_map_reg <- function(search_term,
                                   title_label = NULL,
                                   pal         = "BlueGold",
                                   style       = "quantile") {
  
  display_label <- if (!is.null(title_label)) title_label else search_term
  
  bivar_data <- states_sf |>
    left_join(
      outbreak_totals_reg |>
        select(sigla_uf, n_outbreak_months),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    left_join(
      get_disaster_totals_reg(search_term),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    rename(n_disasters = n_events) |>
    mutate(
      n_disasters       = replace_na(n_disasters, 0),
      n_outbreak_months = replace_na(n_outbreak_months, 0)
    ) |>
    bi_class(
      x     = n_disasters,
      y     = n_outbreak_months,
      style = style,
      dim   = 3
    )
  
  ggplot(bivar_data) +
    geom_sf(
      aes(fill = bi_class),
      colour = "white",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    
    bi_scale_fill(
      pal = pal,
      dim = 3
    ) +
    
    geom_sf_text(
      aes(label = abbrev_state),
      size = 2,
      colour = "grey20"
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      pad_x = unit(0.3, "cm"),
      pad_y = unit(0.3, "cm"),
      text_cex = 0.7,
      line_width = 0.5
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
      title = paste0(
        "Dengue outbreaks vs ",
        display_label,
        " events"
      )
    ) +
    
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      plot.subtitle = element_text(
        size = 9
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
}

p_flood <- plot_bivariate_map_reg(
  "inunda",
  title_label = "flood"
)

p_drought <- plot_bivariate_map_reg(
  "seca",
  title_label = "drought"
)

p_urban_flood <- plot_bivariate_map_reg(
  "alaga",
  title_label = "urban flood"
)

shared_legend <- bi_legend(
  pal  = "BlueGold",
  dim  = 3,
  xlab = "More disaster events",
  ylab = "More dengue outbreaks",
  size = 8
)

combined_maps <- cowplot::plot_grid(
  p_flood,
  p_drought,
  p_urban_flood,
  ncol = 3,
  labels = c("A", "B", "C"),
  label_size = 13,
  label_fontface = "bold",
  align = "hv",
  axis = "tblr"
)

final_figure <- cowplot::ggdraw() +
  cowplot::draw_plot(
    combined_maps,
    x = 0,
    y = 0,
    width = 0.84,
    height = 1
  ) +
  cowplot::draw_plot(
    shared_legend,
    x = 0.84,
    y = 0.32,
    width = 0.16,
    height = 0.36
  )

##label function
panel_label <- function(label) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      fontface = "bold",
      size = 5
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
}

label_a <- panel_label("A")
label_b <- panel_label("B")
label_c <- panel_label("C")

panel_a <- cowplot::plot_grid(
  label_a,
  p_flood,
  ncol = 1,
  rel_heights = c(0.06, 1)
)

panel_b <- cowplot::plot_grid(
  label_b,
  p_drought,
  ncol = 1,
  rel_heights = c(0.06, 1)
)

panel_c <- cowplot::plot_grid(
  label_c,
  p_urban_flood,
  ncol = 1,
  rel_heights = c(0.06, 1)
)

combined_maps <- cowplot::plot_grid(
  panel_a,
  panel_b,
  panel_c,
  ncol = 3,
  align = "hv"
)

final_figure <- cowplot::ggdraw() +
  cowplot::draw_plot(
    combined_maps,
    x = 0,
    y = 0,
    width = 0.84,
    height = 1
  ) +
  cowplot::draw_plot(
    shared_legend,
    x = 0.84,
    y = 0.32,
    width = 0.16,
    height = 0.36
  )

final_figure


# Outbreak proportion map
p_outbreak_prop

# Updated bivariate maps using regression data date range
plot_bivariate_map_reg("inunda", title_label = "flood")
plot_bivariate_map_reg("seca",   title_label = "drought")
plot_bivariate_map_reg("alaga",  title_label = "urban flood")


# Save
ggsave("Submission/map_outbreak_proportion_by_state.png",
       p_outbreak_prop, width = 8, height = 7, dpi = 150)

ggsave("Submission/bivariate_map_combined_reg.png",
       final_figure,
       width = 12, height = 6, dpi = 150)




####---combined updated disaster maps----#####

# Function to get disaster totals by state for a specific typology
get_disaster_map_data <- function(search_term) {
  get_disaster_totals_reg(search_term) |>
    right_join(
      states_sf |> select(abbrev_state, geometry),
      by = c("sigla_uf" = "abbrev_state")
    ) |>
    mutate(n_events = replace_na(n_events, 0))
}

# Build map data for each typology
flood_data      <- get_disaster_map_data("inunda")
urban_flood_data <- get_disaster_map_data("alaga")
drought_data    <- get_disaster_map_data("seca")

#single disaster map
plot_disaster_map_single <- function(map_data,
                                     title_label,
                                     fill_high = "#08306b") {
  
  ggplot(map_data) +
    geom_sf(aes(fill = n_events), colour = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low      = "#eff3ff",
      high     = fill_high,
      na.value = "grey80",
      name     = "Events",
      labels   = scales::comma
    ) +
    geom_sf_text(
      data = states_sf,
      aes(label = abbrev_state),
      size   = 2,
      colour = "grey20"
    ) +
    annotation_scale(
      location   = "bl",
      width_hint = 0.25,
      pad_x      = unit(0.3, "cm"),
      pad_y      = unit(0.3, "cm"),
      text_cex   = 0.7,
      line_width = 0.5
    ) +
    annotation_north_arrow(
      location    = "bl",
      which_north = "true",
      style       = north_arrow_fancy_orienteering,
      pad_x       = unit(0.4, "cm"),
      pad_y       = unit(1.8, "cm"),
      height      = unit(1, "cm"),
      width       = unit(1, "cm")
    ) +
    labs(title = title_label) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 11),
      legend.position = "right",
      plot.margin     = margin(5, 5, 5, 5)
    )
}

##build panes for combined map
p_flood_single <- plot_disaster_map_single(
  st_as_sf(flood_data),
  title_label = "Flood events (Inundações)",
  fill_high   = "#08306b"
)

p_urban_flood_single <- plot_disaster_map_single(
  st_as_sf(urban_flood_data),
  title_label = "Urban flood events (Alagamentos)",
  fill_high   = "#08306b"
)

p_drought_single <- plot_disaster_map_single(
  st_as_sf(drought_data),
  title_label = "Drought events (Estiagem e Seca)",
  fill_high   = "#800026"   # different colour to distinguish drought
)

#combine panes with labels
panel_label <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label,
             fontface = "bold", size = 5) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

panel_a <- cowplot::plot_grid(
  panel_label("A"), p_flood_single,
  ncol = 1, rel_heights = c(0.06, 1)
)

panel_b <- cowplot::plot_grid(
  panel_label("B"), p_urban_flood_single,
  ncol = 1, rel_heights = c(0.06, 1)
)

panel_c <- cowplot::plot_grid(
  panel_label("C"), p_drought_single,
  ncol = 1, rel_heights = c(0.06, 1)
)

combined_disaster_maps <- cowplot::plot_grid(
  panel_a, panel_b, panel_c,
  ncol  = 3,
  align = "hv"
)

combined_disaster_maps

# SAVE

ggsave(
  "Submission/map_disaster_types_combined.png",
  combined_disaster_maps,
  width  = 16,
  height = 6,
  dpi    = 150
)

cat("Saved: outputs/map_disaster_types_combined.png\n")


#####DISASTER INCIDENCE BY YEAR: floods, urban floods, drought####

# Build annual counts for each typology from disasters_plot
disaster_annual <- bind_rows(
  disasters_plot |>
    filter(
      str_detect(tolower(descricao_tipologia), "inunda"),
      date >= disaster_date_min,
      date <= disaster_date_max
    ) |>
    mutate(year = year(date), type = "Floods (Inundações)"),
  
  disasters_plot |>
    filter(
      str_detect(tolower(descricao_tipologia), "alaga"),
      date >= disaster_date_min,
      date <= disaster_date_max
    ) |>
    mutate(year = year(date), type = "Urban Floods (Alagamentos)"),
  
  disasters_plot |>
    filter(
      str_detect(tolower(descricao_tipologia), "seca"),
      date >= disaster_date_min,
      date <= disaster_date_max
    ) |>
    mutate(year = year(date), type = "Drought (Estiagem e Seca)")
) |>
  group_by(year, type) |>
  summarise(n_events = n(), .groups = "drop") |>
  mutate(type = factor(type, levels = c(
    "Floods (Inundações)",
    "Urban Floods (Alagamentos)",
    "Drought (Estiagem e Seca)"
  )))

####combined graph####
p_disaster_annual <- ggplot(disaster_annual,
                            aes(x = year, y = n_events,
                                colour = type, group = type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c(
      "Floods (Inundações)"         = "#2166ac",
      "Urban Floods (Alagamentos)"  = "#74add1",
      "Drought (Estiagem e Seca)"   = "#d73027"
    ),
    name = "Disaster type"
  ) +
  scale_x_continuous(breaks = 2006:2023) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Annual disaster event frequency by type, Brazil (2006–2023)",
    subtitle = "Each point represents the total number of registered events in that year",
    x        = NULL,
    y        = "Number of disaster events"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold")
  )

p_disaster_annual

###other version
p_disaster_facet <- ggplot(disaster_annual,
                           aes(x = year, y = n_events, fill = type)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Floods (Inundações)"         = "#2166ac",
    "Urban Floods (Alagamentos)"  = "#74add1",
    "Drought (Estiagem e Seca)"   = "#d73027"
  )) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  labs(
    title    = "Annual disaster event frequency by type, Brazil (2006–2023)",
    x        = NULL,
    y        = "Number of disaster events"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

p_disaster_facet

##combined map and graph
combined_full <- cowplot::plot_grid(
  combined_disaster_maps,
  p_disaster_annual,
  ncol        = 1,
  rel_heights = c(1, 0.6),
  labels      = c("", "D"),
  label_size  = 13,
  label_fontface = "bold"
)

combined_full

# =============================================================================
# SAVE
# =============================================================================

ggsave("outputs/plot_disaster_annual.png",
       p_disaster_annual, width = 10, height = 5, dpi = 150)

ggsave("outputs/plot_disaster_facet.png",
       p_disaster_facet, width = 8, height = 8, dpi = 150)

ggsave("outputs/figure_disaster_maps_timeseries.png",
       combined_full, width = 16, height = 10, dpi = 150)

cat("Saved all disaster figures to outputs/\n")
