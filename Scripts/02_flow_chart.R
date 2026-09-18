# data filtering flow chart

# install.packages("DiagrammeR")
install.packages("DiagrammeR")
install.packages("DiagrammeRsvg")
install.packages("rsvg")
library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


# Raw files
dengue_raw    <- read_csv("00_Data/Spatial_extract_V1_3.csv")
disasters_raw <- read_csv("00_Data/nat_dis_bra.csv")

# Processed files (already saved by earlier scripts)
merged          <- read_csv("00_Data/merged_week_month.csv")
regression_data <- read_csv("00_Data/regression_data_v4.csv")
disasters_plot <- read_csv("00_Data/disasters_plot_data.csv")
adm2_outbreak <-read_csv("00_Data/adm2_outbreak_data.csv")

# Weekly aggregated rows (needed for one count in the flowchart)
# Rebuild quickly from dengue_raw
dengue_week_agg <- dengue_raw |>
  filter(ISO_A0 == "BRA" | adm_0_name == "BRAZIL") |>
  filter(S_res == "Admin2", !is.na(IBGE_code), !is.na(adm_2_name),
         T_res == "Week") |>
  mutate(IBGE_code = as.character(IBGE_code),
         join_year = year(as.Date(calendar_start_date))) |>
  anti_join(
    dengue_raw |>
      filter(ISO_A0 == "BRA", S_res == "Admin2",
             !is.na(IBGE_code), T_res == "Month") |>
      mutate(IBGE_code = as.character(IBGE_code),
             join_year = year(as.Date(calendar_start_date))) |>
      distinct(IBGE_code, join_year),
    by = c("IBGE_code", "join_year")
  )

# Numbers for updated flowchart
n_raw_dengue      <- nrow(dengue_raw)
n_monthly         <- dengue_raw |> filter(ISO_A0 == "BRA", S_res == "Admin2",
                                          !is.na(IBGE_code), T_res == "Month") |> nrow()
n_weekly_agg      <- nrow(dengue_week_agg)
n_dengue_final    <- nrow(merged)

n_raw_disaster    <- nrow(disasters_raw)
n_disaster_final  <- nrow(disasters_plot)

n_merged          <- nrow(merged)

# Rows with and without sufficient 5yr history
n_with_threshold  <- adm2_outbreak |> filter(!is.na(outbreak)) |> nrow()
n_without_threshold <- adm2_outbreak |> filter(is.na(outbreak)) |> nrow()
n_regression      <- nrow(regression_data)

#rows with 2006-2023 data
n_final_merged <- nrow(regression_data) 

# Southeast only
southeast_states <- c("SAO PAULO", "RIO DE JANEIRO", "MINAS GERAIS", "ESPIRITO SANTO")

n_southeast       <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  nrow()


n_southeast_mun <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  pull(IBGE_code) |>
  n_distinct()


# Southeast with climate data
n_southeast_climate <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  drop_na(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  nrow()

n_southeast_climate_mun <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  drop_na(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  pull(IBGE_code) |>
  n_distinct()

n_model_data      <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  drop_na(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  nrow()
# Southeast model data
n_southeast_model <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         n_inunda_lag1, n_inunda_lag2, n_inunda_lag3, n_inunda_lag4, n_inunda_lag5,
         n_seca_lag1, n_seca_lag2, n_seca_lag3, n_seca_lag4, n_seca_lag5,
         n_alaga_lag1, n_alaga_lag2, n_alaga_lag3, n_alaga_lag4, n_alaga_lag5,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         priv_total_lag1, priv_total_lag2, priv_total_lag3,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmin_lag2, tmin_lag3,
         tmax_lag1, tmax_lag2, tmax_lag3,
         pr_lag1, pr_lag2, pr_lag3) |>
  drop_na() |>
  nrow()

n_southeast_model_mun <- regression_data |>
  filter(adm_1_name %in% southeast_states) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         n_inunda_lag1, n_inunda_lag2, n_inunda_lag3, n_inunda_lag4, n_inunda_lag5,
         n_seca_lag1, n_seca_lag2, n_seca_lag3, n_seca_lag4, n_seca_lag5,
         n_alaga_lag1, n_alaga_lag2, n_alaga_lag3, n_alaga_lag4, n_alaga_lag5,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         priv_total_lag1, priv_total_lag2, priv_total_lag3,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmin_lag2, tmin_lag3,
         tmax_lag1, tmax_lag2, tmax_lag3,
         pr_lag1, pr_lag2, pr_lag3) |>
  drop_na() |>
  pull(IBGE_code) |>
  n_distinct()

cat("Southeast model rows:         ", n_southeast_model, "\n")
cat("Southeast model municipalities:", n_southeast_model_mun, "\n")
# National with climate data
n_national_climate <- regression_data |>
  drop_na(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  nrow()

n_national_climate_mun <- regression_data |>
  drop_na(sd_anomaly, tmin_lag1, tmax_lag2, pr_lag1) |>
  pull(IBGE_code) |>
  n_distinct()

n_national_mun <- regression_data |>
  pull(IBGE_code) |>
  n_distinct()
# National model data: complete cases across all model variables
n_national_model <- model_data |> nrow()
n_national_model_mun <- model_data |> pull(IBGE_code) |> n_distinct()

cat("National model rows:         ", n_national_model, "\n")
cat("National model municipalities:", n_national_model_mun, "\n")

cat("National rows:              ", n_regression, "\n")
cat("National municipalities:    ", n_national_mun, "\n")
cat("National + climate rows:    ", n_national_climate, "\n")
cat("National + climate mun:     ", n_national_climate_mun, "\n")
cat("Southeast rows:             ", n_southeast, "\n")
cat("Southeast municipalities:   ", n_southeast_mun, "\n")
cat("Southeast + climate rows:   ", n_southeast_climate, "\n")
cat("Southeast + climate mun:    ", n_southeast_climate_mun, "\n")

cat("Raw dengue:              ", n_raw_dengue, "\n")
cat("Monthly rows:            ", n_monthly, "\n")
cat("Weekly aggregated:       ", n_weekly_agg, "\n")
cat("Final dengue:            ", n_dengue_final, "\n")
cat("Raw disaster:            ", n_raw_disaster, "\n")
cat("Final disaster:          ", n_disaster_final, "\n")
cat("Merged:                  ", n_merged, "\n")
cat("With threshold:          ", n_with_threshold, "\n")
cat("Without threshold:       ", n_without_threshold, "\n")
cat("Regression data:         ", n_regression, "\n")
cat("Southeast rows:          ", n_southeast, "\n")
cat("Southeast municipalities:", n_southeast_mun, "\n")
cat("Southeast + climate:     ", n_southeast_climate, "\n")
cat("Southeast + climate mun: ", n_southeast_climate_mun, "\n")

# =============================================================================
# OUTBREAK MONTH FILTERING NUMBERS
# =============================================================================

# National: rows in outbreak months only
n_national_outbreak <- regression_data |>
  filter(outbreak == TRUE) |>
  nrow()

n_national_outbreak_mun <- regression_data |>
  filter(outbreak == TRUE) |>
  pull(IBGE_code) |>
  n_distinct()

n_national_non_outbreak <- n_national_model - n_national_outbreak_model

# National outbreak months + complete cases
n_national_outbreak_model <- regression_data |>
  filter(outbreak == TRUE) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmax_lag2, pr_lag1) |>
  drop_na() |>
  nrow()

n_national_outbreak_model_mun <- regression_data |>
  filter(outbreak == TRUE) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmax_lag2, pr_lag1) |>
  drop_na() |>
  pull(IBGE_code) |>
  n_distinct()

# Southeast: rows in outbreak months only
n_southeast_outbreak <- regression_data |>
  filter(adm_1_name %in% southeast_states,
         outbreak == TRUE) |>
  nrow()

n_southeast_outbreak_mun <- regression_data |>
  filter(adm_1_name %in% southeast_states,
         outbreak == TRUE) |>
  pull(IBGE_code) |>
  n_distinct()

n_southeast_non_outbreak <- n_southeast_model - n_southeast_outbreak_model

# Southeast outbreak months + complete cases
n_southeast_outbreak_model <- regression_data |>
  filter(adm_1_name %in% southeast_states,
         outbreak == TRUE) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmax_lag2, pr_lag1) |>
  drop_na() |>
  nrow()

n_southeast_outbreak_model_mun <- regression_data |>
  filter(adm_1_name %in% southeast_states,
         outbreak == TRUE) |>
  select(outbreak, join_year, join_month, adm_1_name, IBGE_code,
         priv_pub_lag1, priv_pub_lag2, priv_pub_lag3,
         pub_total_lag1, pub_total_lag2, pub_total_lag3,
         dh_homeless_lag1, dh_homeless_lag2, dh_homeless_lag3,
         sd_anomaly, sd_prev_5yr, season_month,
         tmin_lag1, tmax_lag2, pr_lag1) |>
  drop_na() |>
  pull(IBGE_code) |>
  n_distinct()

cat("National outbreak rows:            ", n_national_outbreak, "\n")
cat("National non-outbreak excluded:    ", n_national_non_outbreak, "\n")
cat("National outbreak model rows:      ", n_national_outbreak_model, "\n")
cat("National outbreak model mun:       ", n_national_outbreak_model_mun, "\n")
cat("Southeast outbreak rows:           ", n_southeast_outbreak, "\n")
cat("Southeast non-outbreak excluded:   ", n_southeast_non_outbreak, "\n")
cat("Southeast outbreak model rows:     ", n_southeast_outbreak_model, "\n")
cat("Southeast outbreak model mun:      ", n_southeast_outbreak_model_mun, "\n")

flowchart <- grViz(paste0('
digraph flowchart {

  graph [layout = dot, rankdir = TB, fontname = "Arial", fontsize = 12,
         bgcolor = white, nodesep = 0.6, ranksep = 0.7]

  node [fontname = "Arial", fontsize = 11, style = filled, shape = box,
        fillcolor = "#EBF5FB", color = "#2166ac", penwidth = 1.5,
        width = 3.5]

  edge [fontname = "Arial", fontsize = 10, color = "#2166ac", penwidth = 1.5]

  # === DENGUE BRANCH ===

  D0 [label = "Dengue surveillance data\\n(Spatial_extract_V1_3.csv)\\nn = ', n_raw_dengue, ' rows (multi-country)"]

  D1 [label = "Monthly municipality-month rows\\nretained (Brazil, Admin2,\\nvalid IBGE code)\\nn = ', n_monthly, ' rows"]

  D2 [label = "Weekly rows aggregated\\nto month (municipality-years\\nwithout monthly data)\\nn = ', n_weekly_agg, ' rows added"]

  D3 [label = "Final dengue dataset\\nn = ', n_dengue_final, ' municipality-months\\n2001\u20132024",
      fillcolor = "#D5F5E3", color = "#1a9850"]

  # === DISASTER BRANCH ===

  N0 [label = "S2ID Atlas Digital de Desastres\\n(desastres_naturais_no_brasil)\\nn = ', n_raw_disaster, ' events (1991\u20132023)"]

  N1 [label = "Final disaster dataset\\n(filtered to dengue date range,\\nvalid typology and event date)\\nn = ', n_disaster_final, ' events",
      fillcolor = "#D5F5E3", color = "#1a9850"]

  # === MERGE ===

  M1 [label = "Left join on IBGE code +\\nyear + month\\n(disasters aggregated to\\nmunicipality-month,\\nrestricted to 2006\u20132023)\\nn = ', n_merged, ' municipality-months",
      fillcolor = "#FEF9E7", color = "#d4ac0d"]

  # === OUTBREAK DEFINITION ===

  O1 [label = "Apply outbreak threshold\\n(mean + 1.25 SD of preceding\\n5-year monthly mean,\\nminimum 10 cases)"]

  O2 [label = "Full regression dataset\\n(all Brazil, 2006\u20132023)\\nn = ', n_with_threshold, ' municipality-months\\n', n_national_mun, ' municipalities",
      fillcolor = "#FEF9E7", color = "#d4ac0d"]

  # === NATIONAL BRANCH ===

  NAT1 [label = "National analysis\\n(all Brazil)\\nn = ', n_with_threshold, ' municipality-months\\n', n_national_mun, ' municipalities",
        fillcolor = "#D5F5E3", color = "#1a9850"]

  NAT2 [label = "National model dataset\\n(complete cases across\\nall model variables)\\nn = ', n_national_model, ' municipality-months\\n', n_national_model_mun, ' municipalities",
      fillcolor = "#D5F5E3", color = "#1a9850"]

  # === SOUTHEAST BRANCH ===

  SE1 [label = "Southeast Brazil subset\\n(São Paulo, Rio de Janeiro,\\nMinas Gerais, Espírito Santo)\\nn = ', n_southeast, ' municipality-months\\n', n_southeast_mun, ' municipalities",
       fillcolor = "#D5F5E3", color = "#1a9850"]

  SE2 [label = "Southeast model dataset\\n(complete cases across\\nall model variables)\\nn = ', n_southeast_model, ' municipality-months\\n', n_southeast_model_mun, ' municipalities",
       fillcolor = "#D5F5E3", color = "#1a9850"]

  # === EXCLUSION NODES ===

  node [shape = box, fillcolor = "#FADBD8", color = "#c0392b",
        style = filled, width = 2.8]

  E1 [label = "Excluded: insufficient\\n5-year history\\n(outbreak = NA)\\nn = ', n_without_threshold, ' municipality-months"]

  E2 [label = "Excluded: missing\\ncomplete data across\\nall model variables\\nn = ', n_with_threshold - n_national_model, ' municipality-months"]

  E3 [label = "Excluded: missing\\ncomplete data across\\nall model variables\\nn = ', n_southeast - n_southeast_model, ' municipality-months"]

  # === EDGES: DENGUE ===

  D0 -> D1
  D1 -> D2
  D1 -> D3
  D2 -> D3

  # === EDGES: DISASTER ===

  N0 -> N1

  # === EDGES: MERGE ===

  D3 -> M1
  N1 -> M1

  # === EDGES: OUTBREAK ===

  M1 -> O1
  O1 -> E1 [style = dashed, color = "#c0392b"]
  O1 -> O2

  # === EDGES: NATIONAL BRANCH ===

  O2 -> NAT1
  NAT1 -> NAT2
  NAT1 -> E2 [style = dashed, color = "#c0392b"]

  # === EDGES: SOUTHEAST BRANCH ===

  O2 -> SE1
  SE1 -> SE2
  SE1 -> E3 [style = dashed, color = "#c0392b"]

  # === LAYOUT: force branches side by side ===

  { rank = same; D0; N0 }
  { rank = same; D3; N1 }
  { rank = same; NAT1; SE1 }
  { rank = same; NAT2; SE2 }
  { rank = same; E2; E3 }
}
'))

flowchart

# Save
flowchart |>
  export_svg() |>
  chartr("\n", " ", x = _) |>
  rsvg_png("outputs/data_flowchart.png", width = 2800)

cat("Saved: outputs/data_flowchart.png\n")




flowchart <- grViz(paste0('
digraph flowchart {

  graph [layout = dot, rankdir = TB, fontname = "Arial", fontsize = 12,
         bgcolor = white, nodesep = 0.6, ranksep = 0.7]

  node [fontname = "Arial", fontsize = 11, style = filled, shape = box,
        fillcolor = "#EBF5FB", color = "#2166ac", penwidth = 1.5,
        width = 3.5]

  edge [fontname = "Arial", fontsize = 10, color = "#2166ac", penwidth = 1.5]

  # === DENGUE BRANCH ===

  D0 [label = "Dengue surveillance data\\n(Spatial_extract_V1_3.csv)\\nn = ', n_raw_dengue, ' rows (multi-country)"]

  D1 [label = "Monthly municipality-month rows\\nretained (Brazil, Admin2,\\nvalid IBGE code)\\nn = ', n_monthly, ' rows"]

  D2 [label = "Weekly rows aggregated\\nto month (municipality-years\\nwithout monthly data)\\nn = ', n_weekly_agg, ' rows added"]

  D3 [label = "Final dengue dataset\\nn = ', n_dengue_final, ' municipality-months\\n2001\u20132024",
      fillcolor = "#D5F5E3", color = "#1a9850"]

  # === DISASTER BRANCH ===

  N0 [label = "S2ID Atlas Digital de Desastres\\n(desastres_naturais_no_brasil)\\nn = ', n_raw_disaster, ' events (1991\u20132023)"]

  N1 [label = "Final disaster dataset\\n(filtered to dengue date range,\\nvalid typology and event date)\\nn = ', n_disaster_final, ' events",
      fillcolor = "#D5F5E3", color = "#1a9850"]

  # === MERGE ===

  M1 [label = "Left join on IBGE code +\\nyear + month\\n(disasters aggregated to\\nmunicipality-month,\\nrestricted to 2006\u20132023)\\nn = ', n_merged, ' municipality-months",
      fillcolor = "#FEF9E7", color = "#d4ac0d"]

  # === OUTBREAK DEFINITION ===

  O1 [label = "Apply outbreak threshold\\n(mean + 1.25 SD of preceding\\n5-year monthly mean,\\nminimum 10 cases)"]

  O2 [label = "Full regression dataset\\n(all Brazil, 2006\u20132023)\\nn = ', n_with_threshold, ' municipality-months\\n', n_national_mun, ' municipalities",
      fillcolor = "#FEF9E7", color = "#d4ac0d"]

  # ============================================================
  # === NATIONAL BRANCH ===
  # ============================================================

  # Disaster typology analysis (all months)
  NAT1 [label = "National: disaster typology\\nanalysis (all months)\\nn = ', n_national_model, ' municipality-months\\n', n_national_model_mun, ' municipalities",
        fillcolor = "#D5F5E3", color = "#1a9850"]

  # Climate data join
  NAT_CLI [label = "National + climate data\\n(BR-DWGD via brclimr,\\n2006\u20132023, complete cases)\\nn = ', n_national_climate, ' municipality-months\\n', n_national_climate_mun, ' municipalities",
           fillcolor = "#D5F5E3", color = "#1a9850"]

  # Filter to outbreak months for impact variables
  NAT_OB [label = "National: disaster impact\\nanalysis (outbreak months only)\\nn = ', n_national_outbreak, ' municipality-months\\n', n_national_outbreak_mun, ' municipalities",
          fillcolor = "#D5F5E3", color = "#1a9850"]

  # Impact analysis complete cases
  NAT_OB2 [label = "National impact model dataset\\n(complete cases)\\nn = ', n_national_outbreak_model, ' municipality-months\\n', n_national_outbreak_model_mun, ' municipalities",
           fillcolor = "#D5F5E3", color = "#1a9850"]

  # ============================================================
  # === SOUTHEAST BRANCH ===
  # ============================================================

  # Southeast subset
  SE0 [label = "Southeast Brazil subset\\n(São Paulo, Rio de Janeiro,\\nMinas Gerais, Espírito Santo)\\nn = ', n_southeast, ' municipality-months\\n', n_southeast_mun, ' municipalities",
       fillcolor = "#FEF9E7", color = "#d4ac0d"]

  # Disaster typology analysis (all months)
  SE1 [label = "Southeast: disaster typology\\nanalysis (all months)\\nn = ', n_southeast_model, ' municipality-months\\n', n_southeast_model_mun, ' municipalities",
       fillcolor = "#D5F5E3", color = "#1a9850"]

  # Climate data join
  SE_CLI [label = "Southeast + climate data\\n(BR-DWGD via brclimr,\\n2006\u20132023, complete cases)\\nn = ', n_southeast_climate, ' municipality-months\\n', n_southeast_climate_mun, ' municipalities",
          fillcolor = "#D5F5E3", color = "#1a9850"]

  # Filter to outbreak months for impact variables
  SE_OB [label = "Southeast: disaster impact\\nanalysis (outbreak months only)\\nn = ', n_southeast_outbreak, ' municipality-months\\n', n_southeast_outbreak_mun, ' municipalities",
         fillcolor = "#D5F5E3", color = "#1a9850"]

  # Impact analysis complete cases
  SE_OB2 [label = "Southeast impact model dataset\\n(complete cases)\\nn = ', n_southeast_outbreak_model, ' municipality-months\\n', n_southeast_outbreak_model_mun, ' municipalities",
          fillcolor = "#D5F5E3", color = "#1a9850"]

  # === EXCLUSION NODES ===

  node [shape = box, fillcolor = "#FADBD8", color = "#c0392b",
        style = filled, width = 2.8]

  E1 [label = "Excluded: insufficient\\n5-year history\\n(outbreak = NA)\\nn = ', n_without_threshold, ' municipality-months"]

  E_NAT_CLI  [label = "Excluded: missing\\nclimate data\\nn = ', n_with_threshold - n_national_climate, ' municipality-months"]

  E_NAT_OB   [label = "Excluded: non-outbreak\\nmonths\\nn = ', n_national_non_outbreak, ' municipality-months"]

  E_SE_CLI   [label = "Excluded: missing\\nclimate data\\nn = ', n_southeast - n_southeast_climate, ' municipality-months"]

  E_SE_OB    [label = "Excluded: non-outbreak\\nmonths\\nn = ', n_southeast_non_outbreak, ' municipality-months"]

  # === EDGES: DENGUE ===

  D0 -> D1
  D1 -> D2
  D1 -> D3
  D2 -> D3

  # === EDGES: DISASTER ===

  N0 -> N1

  # === EDGES: MERGE ===

  D3 -> M1
  N1 -> M1

  # === EDGES: OUTBREAK ===

  M1 -> O1
  O1 -> E1 [style = dashed, color = "#c0392b"]
  O1 -> O2

  # === EDGES: BIFURCATION ===

  O2 -> NAT1
  O2 -> SE0

  # === EDGES: NATIONAL BRANCH ===

  NAT1 -> NAT_CLI
  NAT_CLI -> E_NAT_CLI [style = dashed, color = "#c0392b"]
  NAT1 -> NAT_OB
  NAT_OB -> E_NAT_OB  [style = dashed, color = "#c0392b"]
  NAT_OB -> NAT_OB2

  # === EDGES: SOUTHEAST BRANCH ===

  SE0  -> SE1
  SE1  -> SE_CLI
  SE_CLI -> E_SE_CLI  [style = dashed, color = "#c0392b"]
  SE1  -> SE_OB
  SE_OB -> E_SE_OB    [style = dashed, color = "#c0392b"]
  SE_OB -> SE_OB2

  # === LAYOUT ===

  { rank = same; D0; N0 }
  { rank = same; D3; N1 }
  { rank = same; NAT1; SE0 }
  { rank = same; NAT_CLI; SE_CLI }
  { rank = same; NAT_OB; SE_OB }
  { rank = same; NAT_OB2; SE_OB2 }
  { rank = same; E_NAT_CLI; E_SE_CLI }
  { rank = same; E_NAT_OB; E_SE_OB }
}
'))

flowchart

# Save
flowchart |>
  export_svg() |>
  chartr("\n", " ", x = _) |>
  rsvg_png("outputs/data_flowchart.png", width = 3200)

cat("Saved: outputs/data_flowchart.png\n")