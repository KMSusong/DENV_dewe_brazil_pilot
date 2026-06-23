#' --- 
#' title: "02 Merged dengue and Natural Disaster Datasets with month and week data" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-18" 
#' --- 

#' Overview: 
#'   Merge dengue and natural distaster datasets by municipality, aggregating weekly data where months not
#'   available; save to Data/ 

#' Timeline: 
#'   2025-06-18:

library(tidyverse)
library(lubridate)
dengue_raw <- read_csv("00_Data/Spatial_extract_V1_3.csv")
disasters_raw <- read_csv("00_Data/nat_dis_bra.csv")

#check dates for disasters
disasters_raw |>
  summarise(
    min_date = min(as.Date(data_evento), na.rm = TRUE),
    max_date = max(as.Date(data_evento), na.rm = TRUE),
    n_years  = n_distinct(`__ano_evento`),
    years    = paste(sort(unique(`__ano_evento`)), collapse = ", ")
  ) |>
  print()
#make reference table for admin 2 to admin 1
adm1_lookup <- dengue_raw |>
  filter(ISO_A0 == "BRA" | adm_0_name == "BRAZIL") |>
  filter(!is.na(IBGE_code), !is.na(adm_1_name)) |>
  mutate(IBGE_code = as.character(IBGE_code)) |>
  distinct(IBGE_code, adm_1_name, RNE_iso_code) |>
  # In the rare case a municipality maps to >1 state name, keep first
  group_by(IBGE_code) |>
  slice(1) |>
  ungroup()

#2b. build the state-code crosswalk 
#     allows to attach sigla_uf (used in disaster data) to dengue rows.
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

# Enrich lookup with sigla_uf
adm1_lookup <- adm1_lookup |>
  left_join(state_crosswalk |> select(RNE_iso_code, sigla_uf),
            by = "RNE_iso_code")

# 2c. Prepare Admin2 dengue rows - prefer monthly, aggregate weekly where
#     monthly is absent for that municipality-year combination

dengue_month <- dengue_raw |>
  filter(ISO_A0 == "BRA" | adm_0_name == "BRAZIL") |>
  filter(S_res == "Admin2", !is.na(IBGE_code), !is.na(adm_2_name)) |>
  filter(T_res == "Month") |>
  mutate(
    calendar_start_date = as.Date(calendar_start_date),
    join_year           = year(calendar_start_date),
    join_month          = month(calendar_start_date),
    IBGE_code           = as.character(IBGE_code)
  )

dengue_week <- dengue_raw |>
  filter(ISO_A0 == "BRA" | adm_0_name == "BRAZIL") |>
  filter(S_res == "Admin2", !is.na(IBGE_code), !is.na(adm_2_name)) |>
  filter(T_res == "Week") |>
  mutate(
    calendar_start_date = as.Date(calendar_start_date),
    join_year           = year(calendar_start_date),
    join_month          = month(calendar_start_date),
    IBGE_code           = as.character(IBGE_code)
  )

# Find municipality-years already covered by monthly data
covered <- dengue_month |>
  distinct(IBGE_code, join_year)

# Aggregate weekly rows to month, only for municipality-years NOT in monthly data
dengue_week_agg <- dengue_week |>
  anti_join(covered, by = c("IBGE_code", "join_year")) |>
  group_by(IBGE_code, adm_2_name, adm_0_name, ISO_A0,
           join_year, join_month, case_definition_standardised) |>
  summarise(
    dengue_total        = sum(dengue_total, na.rm = TRUE),
    calendar_start_date = min(calendar_start_date),   # first week of that month
    S_res               = "Admin2",
    T_res               = "Week->Month",              # flag the source
    .groups = "drop"
  )

# Combine
dengue <- bind_rows(dengue_month, dengue_week_agg) |>
  select(-any_of(c("adm_1_name", "RNE_iso_code"))) |>
  left_join(adm1_lookup, by = "IBGE_code")
#view dates
dengue |>
  summarise(
    min_date = min(calendar_start_date, na.rm = TRUE),
    max_date = max(calendar_start_date, na.rm = TRUE),
    n_years  = n_distinct(join_year),
    years    = paste(sort(unique(join_year)), collapse = ", ")
  ) |>
  print()

# 3a. Parse dates and ensure join-key types match
disasters <- disasters_raw |>
  mutate(
    cod_ibge_mun  = as.character(cod_ibge_mun),
    `__ano_evento` = as.integer(`__ano_evento`),
    `__mes_evento` = as.integer(`__mes_evento`)
  )

# 3b. Aggregate disasters to municipality-month level
#     Multiple events in the same municipality-month are collapsed to:
#       - n_events        : total number of disaster events
#       - n_by_group      : counts by broad disaster group
#       - human/material/environmental/public/private impact totals
#       - flags for any environmental damage sub-fields

disasters_agg <- disasters |>
  group_by(cod_ibge_mun, `__ano_evento`, `__mes_evento`) |>
  summarise(
    # --- event counts ---
    n_events = n(),
    
    # --- counts by disaster group (Obj 1) ---
    n_climatological  = sum(grupo_de_desastre == "Climatológico",  na.rm = TRUE),
    n_hidrological    = sum(grupo_de_desastre == "Hidrológico",    na.rm = TRUE),
    n_meteorological  = sum(grupo_de_desastre == "Meteorológico",  na.rm = TRUE),
    n_geological      = sum(grupo_de_desastre == "Geológico",      na.rm = TRUE),
    
    # --- human impacts ---
    dh_deaths_sum              = sum(dh_mortos,              na.rm = TRUE),
    dh_injured_sum             = sum(dh_feridos,             na.rm = TRUE),
    dh_ill_sum            = sum(dh_enfermos,            na.rm = TRUE),
    dh_homeless_sum        = sum(dh_desabrigados,        na.rm = TRUE),
    dh_displaced_sum         = sum(dh_desalojados,         na.rm = TRUE),
    dh_missing_sum       = sum(dh_desaparecidos,       na.rm = TRUE),
    dh_total_dam_human_sum = sum(dh_total_danos_humanos, na.rm = TRUE),
    
    # --- material / infrastructure damages (Obj 3) ---
    dm_unit_house_damaged_sum  = sum(dm_uni_habita_danificadas,  na.rm = TRUE),
    dm_unit_house_destroy_sum   = sum(dm_uni_habita_destruidas,   na.rm = TRUE),
    dm_health_inst_damaged_sum  = sum(dm_inst_saude_danificadas,  na.rm = TRUE),
    dm_health_inst_destroy_sum   = sum(dm_inst_saude_destruidas,   na.rm = TRUE),
    dm_pub_infra_damaged_sum = sum(dm_obras_de_infra_danificadas, na.rm = TRUE),
    dm_pub_infra_destroyed_sum  = sum(dm_obras_de_infra_destruidas,  na.rm = TRUE),
    dm_total_damage_material_sum   = sum(dm_total_danos_materiais,   na.rm = TRUE),
    
    # --- environmental damages (Obj 2) ---
    #  flag any non-missing/non-zero entry
    flag_water_contam  = any(!is.na(da_poluicont_da_agua)  & da_poluicont_da_agua  != "" & da_poluicont_da_agua  != 0),
    flag_air_contam    = any(!is.na(da_poluicont_do_ar)    & da_poluicont_do_ar    != "" & da_poluicont_do_ar    != 0),
    flag_soil_contam  = sum(!is.na(da_poluicont_do_solo)  & da_poluicont_do_solo  != "" & da_poluicont_do_solo  != 0),
    flag_water_deplete   = any(!is.na(da_dimiexauri_hidrico) & da_dimiexauri_hidrico != "" & da_dimiexauri_hidrico != 0),
    
    
    # --- public-sector losses (Obj 3) ---
    pub_health_sum         = sum(pepl_assis_med_e_emergenr,  na.rm = TRUE),
    pub_pot_water_supply_sum          = sum(pepl_abast_de_agua_potr,    na.rm = TRUE),
    pub_sewage_sum    = sum(pepl_sist_de_esgotos_sanitr, na.rm = TRUE),
    pub_waste_sum      = sum(pepl_sis_limp_e_rec_lixo_r, na.rm = TRUE),
    pub_vector_control_sum       = sum(pepl_sis_cont_pragas_r,     na.rm = TRUE),
    pub_total_sum = sum(pepl_total_publico,         na.rm = TRUE),
    
    # --- private losses and total ---
    priv_total_sum = sum(pepr_total_privado, na.rm = TRUE),
    priv_pub_sum           = sum(pe_plepr,           na.rm = TRUE),
    
    # --- retain typology list for reference ---
    tipologias = paste(sort(unique(descricao_tipologia)), collapse = "; "),
    
    .groups = "drop"
  ) |>
  rename(
    join_year  = `__ano_evento`,
    join_month = `__mes_evento`
  )


# =============================================================================
# 4. JOIN: dengue (left) <- disasters_agg (right)
# =============================================================================
# Left join so every dengue municipality-month row is retained.
# Disaster columns will be NA where no disaster was recorded.

merged <- dengue |>
  left_join(
    disasters_agg,
    by = c("IBGE_code" = "cod_ibge_mun", "join_year", "join_month")
  ) |>
  # Convenience: replace NA disaster counts with 0 (no event = 0)
  mutate(
    across(
      c(n_events, starts_with("n_clima"), starts_with("n_hidro"),
        starts_with("n_meteor"), starts_with("n_geolog"),
        ends_with("_sum")),
      ~ replace_na(.x, 0)
    ),
    across(starts_with("flag_"), ~ replace_na(.x, FALSE))
  )


# =============================================================================
# 7. ADMIN1-LEVEL SUMMARY
# =============================================================================
# Aggregate the municipality-month merged data up to state × year × month.
# Dengue cases are summed; disaster counts/impacts are summed across
# municipalities; flags become TRUE if any municipality was flagged.

merged_adm1 <- merged |>
  group_by(adm_1_name, RNE_iso_code, sigla_uf, join_year, join_month) |>
  summarise(
    n_municipalities          = n_distinct(IBGE_code),
    
    # --- dengue ---
    dengue_total              = sum(dengue_total, na.rm = TRUE),
    
    # --- disaster event counts ---
    n_events                  = sum(n_events,          na.rm = TRUE),
    n_climatologico           = sum(n_climatologico,   na.rm = TRUE),
    n_hidrologico             = sum(n_hidrologico,     na.rm = TRUE),
    n_meteorologico           = sum(n_meteorologico,   na.rm = TRUE),
    n_geologico               = sum(n_geologico,       na.rm = TRUE),
    
    # --- human impacts ---
    dh_mortos_sum             = sum(dh_mortos_sum,              na.rm = TRUE),
    dh_desabrigados_sum       = sum(dh_desabrigados_sum,        na.rm = TRUE),
    dh_desalojados_sum        = sum(dh_desalojados_sum,         na.rm = TRUE),
    dh_total_danos_humanos_sum= sum(dh_total_danos_humanos_sum, na.rm = TRUE),
    
    # --- material damages ---
    dm_total_danos_materiais_sum = sum(dm_total_danos_materiais_sum, na.rm = TRUE),
    
    # --- environmental flags (TRUE if any municipality flagged) ---
    flag_agua_contaminada     = any(flag_agua_contaminada,  na.rm = TRUE),
    flag_ar_contaminado       = any(flag_ar_contaminado,    na.rm = TRUE),
    flag_solo_contaminado     = any(flag_solo_contaminado > 0, na.rm = TRUE),
    flag_deficit_hidrico      = any(flag_deficit_hidrico,   na.rm = TRUE),
    flag_incendio_proteg      = any(flag_incendio_proteg,   na.rm = TRUE),
    
    # --- public-sector losses ---
    pepl_saude_sum            = sum(pepl_saude_sum,         na.rm = TRUE),
    pepl_agua_sum             = sum(pepl_agua_sum,          na.rm = TRUE),
    pepl_saneamento_sum       = sum(pepl_saneamento_sum,    na.rm = TRUE),
    pepl_vetores_sum          = sum(pepl_vetores_sum,       na.rm = TRUE),
    pepl_total_publico_sum    = sum(pepl_total_publico_sum, na.rm = TRUE),
    
    # --- total losses ---
    pe_plepr_sum              = sum(pe_plepr_sum,           na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  arrange(adm_1_name, join_year, join_month)

cat("=== Admin1 summary dimensions ===\n")
cat(sprintf("Rows: %d  |  Columns: %d\n\n", nrow(merged_adm1), ncol(merged_adm1)))

cat("=== States present in Admin1 summary ===\n")
print(sort(unique(merged_adm1$adm_1_name)))

write_csv(merged,"00_Data/merged_week_month.csv")
write_csv(merged_adm1,"00_Data/merged_adm1_week_month.csv")
