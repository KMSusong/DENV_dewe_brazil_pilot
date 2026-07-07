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
#' 

install.packages("geobr")
install.packages("patchwork")
install.packages("patchwork")
install.packages("biscale")
install.packages("cowplot")


library(tidyverse)
library(geobr)     # Brazil shapefiles - install.packages("geobr")
library(sf)
library(ggplot2)
library(patchwork)
library(biscale)
library(cowplot)


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


#load brazil shapefile
states_sf <- read_state(year = 2020, showProgress = FALSE)

#outbreak totals by state
outbreak_totals <- outbreak_5 |>
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


# --- 5a. Outbreak map ---
plot_outbreak_map <- function(variable  = "n_outbreak_months",
                              label    = "Outbreak months") {
  
  ggplot(map_outbreaks) +
    geom_sf(aes(fill = .data[[variable]]), colour = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low      = "#ffffcc",
      high     = "#800026",
      na.value = "grey80",
      name     = label,
      labels   = scales::comma
    ) +
    geom_sf_text(aes(label = abbrev_state), size = 2, colour = "grey20") +
    labs(
      title    = paste0("Dengue outbreak months by state 2001 - 2024"),
      subtitle = paste0("Outbreak defined as dengue > mean + 1.25 SD ",
                        "of previous 5-year monthly mean"),
      
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = "right"
    )
}

# --- 5b. Disaster map ---
plot_disaster_map <- function(search_term, title_label = NULL) {
  
  # Use search_term as title if no label provided
  display_label <- if (!is.null(title_label)) title_label else search_term
  
  map_data <- join_disasters_to_map(search_term)
  
  ggplot(map_data) +
    geom_sf(aes(fill = n_events), colour = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low      = "#eff3ff",
      high     = "#08306b",
      na.value = "grey80",
      name     = "Events",
      labels   = scales::comma
    ) +
    geom_sf_text(aes(label = abbrev_state), size = 2, colour = "grey20") +
    labs(
      title    = paste0("Brazil natural disaster events by state 2001 - 2023: ", display_label),
      
    
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
}



# Individual maps
plot_outbreak_map()
plot_outbreak_map("pct_outbreak", "% months as outbreak")

plot_disaster_map("inunda", title_label = "Floods")
plot_disaster_map("seca",   title_label = "Drought")
plot_disaster_map("alaga",  title_label = "Urban Floods")
plot_disaster_map("massa",  title_label = "Mass Movement")


# Save
ggsave("Dev_Figures/map_pct_outbreaks.png",
       plot_outbreak_map("pct_outbreak", "% months as outbreak"), width = 8, height = 7, dpi = 150)
ggsave("Dev_Figures/map_outbreaks.png",
       plot_outbreak_map(), width = 8, height = 7, dpi = 150)





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
                               pal         = "DkBlue",
                               style       = "quantile") {
  
  display_label <- if (!is.null(title_label)) title_label else search_term
  
  bivar_data <- states_sf |>
    left_join(outbreak_totals, by = c("abbrev_state" = "sigla_uf")) |>
    left_join(
      get_disaster_totals(search_term),
      by = c("abbrev_state" = "sigla_uf")
    ) |>
    rename(n_disasters = n_events) |>
    mutate(n_disasters       = replace_na(n_disasters, 0),
           n_outbreak_months = replace_na(n_outbreak_months, 0)) |>
    bi_class(x = n_disasters, y = n_outbreak_months,
             style = style, dim = 3)
  
  map <- ggplot(bivar_data) +
    geom_sf(aes(fill = bi_class), colour = "white", linewidth = 0.3,
            show.legend = FALSE) +
    bi_scale_fill(pal = pal, dim = 3) +
    geom_sf_text(aes(label = abbrev_state), size = 2, colour = "grey20") +
    labs(
      title    = paste0("Brazil dengue outbreak months vs ", display_label, " events by state 2001 - 2023"),
      subtitle = "Colour shows combination of disaster frequency and outbreak frequency",
      
    ) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  
  legend <- bi_legend(
    pal  = pal,
    dim  = 3,
    xlab = paste0("More ", display_label),
    ylab = "More outbreaks",
    size = 8
  )
  
  ggdraw() +
    draw_plot(map,    x = 0,    y = 0,    width = 0.8,  height = 1) +
    draw_plot(legend, x = 0.75, y = 0.05, width = 0.25, height = 0.25)
}

# Usage:
plot_bivariate_map("inunda", title_label = "flood", pal = "DkCyan")
plot_bivariate_map("seca",   title_label = "drought", pal = "DkCyan")
plot_bivariate_map("massa",  title_label = "mass movement", pal = "DkCyan")
plot_bivariate_map("alaga",  title_label = "urban Flood", pal = "DkCyan")


ggsave("Dev_Figures/bivariate_map_urb_flood.png",
       plot_bivariate_map("alaga",  title_label = "urban flood", pal = "DkCyan"), 
       width = 10, height = 8, dpi = 150)

ggsave("Dev_Figures/bivariate_map_flood.png",
       plot_bivariate_map("inunda",  title_label = "flood", pal = "DkCyan"), 
       width = 10, height = 8, dpi = 150)

ggsave("Dev_Figures/bivariate_map_drought.png",
       plot_bivariate_map("seca",  title_label = "drought", pal = "DkCyan"), 
       width = 10, height = 8, dpi = 150)

ggsave("Dev_Figures/bivariate_map_mass.png",
       plot_bivariate_map("mass",  title_label = "mass movement", pal = "DkCyan"), 
       width = 10, height = 8, dpi = 150)
