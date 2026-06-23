#' --- 
#' title: "02 Merged dengue and Natural Disaster Datasets" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-18" 
#' --- 

#' Overview: 
#'   Merge dengue and natural distaster datasets by municipality only using
#'   monthly data; save to Data/ 

#' Timeline: 
#'   2025-06-18:

library(tidyverse)
library(lubridate)
dengue_raw <- read_csv("00_Data/Spatial_extract_V1_3.csv")
disasters_raw <- read_csv("00_Data/nat_dis_bra.csv")

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

#2b. Also build the state-code crosswalk from the guide (Section 6.2).
#     This lets us attach sigla_uf (used in disaster data) to dengue rows.
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

#2c. Prepare Admin2 dengue rows for analysis
dengue <- dengue_raw |>
  # Filter to Brazil only
  filter(ISO_A0 == "BRA" | adm_0_name == "BRAZIL") |>
  # Keep municipality-level rows only (Admin2 with a valid IBGE code)
  filter(S_res == "Admin2", !is.na(IBGE_code), !is.na(adm_2_name)) |>
  # Keep month-resolution rows (avoids double-counting weekly rows)
  filter(T_res == "Month") |>
  # Extract join keys: year and month from calendar_start_date
  mutate(
    calendar_start_date = as.Date(calendar_start_date),
    join_year  = year(calendar_start_date),
    join_month = month(calendar_start_date),
    IBGE_code  = as.character(IBGE_code)
  ) |>
  # Attach reliable Admin1 columns from the lookup
  select(-any_of(c("adm_1_name", "RNE_iso_code"))) |>   # drop possibly-NA originals
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
    n_climatologico  = sum(grupo_de_desastre == "Climatológico",  na.rm = TRUE),
    n_hidrologico    = sum(grupo_de_desastre == "Hidrológico",    na.rm = TRUE),
    n_meteorologico  = sum(grupo_de_desastre == "Meteorológico",  na.rm = TRUE),
    n_geologico      = sum(grupo_de_desastre == "Geológico",      na.rm = TRUE),
    
    # --- human impacts ---
    dh_mortos_sum              = sum(dh_mortos,              na.rm = TRUE),
    dh_feridos_sum             = sum(dh_feridos,             na.rm = TRUE),
    dh_enfermos_sum            = sum(dh_enfermos,            na.rm = TRUE),
    dh_desabrigados_sum        = sum(dh_desabrigados,        na.rm = TRUE),
    dh_desalojados_sum         = sum(dh_desalojados,         na.rm = TRUE),
    dh_desaparecidos_sum       = sum(dh_desaparecidos,       na.rm = TRUE),
    dh_total_danos_humanos_sum = sum(dh_total_danos_humanos, na.rm = TRUE),
    
    # --- material / infrastructure damages (Obj 3) ---
    dm_uni_habita_danificadas_sum  = sum(dm_uni_habita_danificadas,  na.rm = TRUE),
    dm_uni_habita_destruidas_sum   = sum(dm_uni_habita_destruidas,   na.rm = TRUE),
    dm_inst_saude_danificadas_sum  = sum(dm_inst_saude_danificadas,  na.rm = TRUE),
    dm_inst_saude_destruidas_sum   = sum(dm_inst_saude_destruidas,   na.rm = TRUE),
    dm_obras_de_infra_danificadas_sum = sum(dm_obras_de_infra_danificadas, na.rm = TRUE),
    dm_obras_de_infra_destruidas_sum  = sum(dm_obras_de_infra_destruidas,  na.rm = TRUE),
    dm_total_danos_materiais_sum   = sum(dm_total_danos_materiais,   na.rm = TRUE),
    
    # --- environmental damages (Obj 2) ---
    # These fields are often free-text or blank; flag any non-missing/non-zero entry
    flag_agua_contaminada  = any(!is.na(da_poluicont_da_agua)  & da_poluicont_da_agua  != "" & da_poluicont_da_agua  != 0),
    flag_ar_contaminado    = any(!is.na(da_poluicont_do_ar)    & da_poluicont_do_ar    != "" & da_poluicont_do_ar    != 0),
    flag_solo_contaminado  = sum(!is.na(da_poluicont_do_solo)  & da_poluicont_do_solo  != "" & da_poluicont_do_solo  != 0),
    flag_deficit_hidrico   = any(!is.na(da_dimiexauri_hidrico) & da_dimiexauri_hidrico != "" & da_dimiexauri_hidrico != 0),
    flag_incendio_proteg   = any(!is.na(da_incendi_parquesapasapps) & da_incendi_parquesapasapps != "" & da_incendi_parquesapasapps != 0),
    
    # --- public-sector losses (Obj 3) ---
    pepl_saude_sum         = sum(pepl_assis_med_e_emergenr,  na.rm = TRUE),
    pepl_agua_sum          = sum(pepl_abast_de_agua_potr,    na.rm = TRUE),
    pepl_saneamento_sum    = sum(pepl_sist_de_esgotos_sanitr, na.rm = TRUE),
    pepl_residuos_sum      = sum(pepl_sis_limp_e_rec_lixo_r, na.rm = TRUE),
    pepl_vetores_sum       = sum(pepl_sis_cont_pragas_r,     na.rm = TRUE),
    pepl_total_publico_sum = sum(pepl_total_publico,         na.rm = TRUE),
    
    # --- private losses and total ---
    pepr_total_privado_sum = sum(pepr_total_privado, na.rm = TRUE),
    pe_plepr_sum           = sum(pe_plepr,           na.rm = TRUE),
    
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

#viewing merged data
cat("=== Merged dataset dimensions ===\n")
cat(sprintf("Rows: %d  |  Columns: %d\n\n", nrow(merged), ncol(merged)))

cat("=== Dengue rows matched to >=1 disaster event ===\n")
print(table(has_disaster = merged$n_events > 0))

cat("\n=== Dengue case summary (dengue_total) ===\n")
print(summary(merged$dengue_total))

cat("\n=== Disaster events per municipality-month (where >0) ===\n")
merged |>
  filter(n_events > 0) |>
  pull(n_events) |>
  summary() |>
  print()

cat("\n=== Year range covered ===\n")
cat(sprintf("Dengue:    %d – %d\n", min(merged$join_year), max(merged$join_year)))

cat("\n=== Rows with any environmental damage flag ===\n")
merged |>
  filter(flag_agua_contaminada | flag_deficit_hidrico |
           flag_solo_contaminado | flag_incendio_proteg) |>
  nrow() |>
  cat("\n")

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

# =============================================================================
# 8. TABULATE DENGUE OBSERVATIONS BY ADMIN1 × YEAR
# =============================================================================
# n_obs  = number of municipality-month rows (data coverage)
# n_mun  = number of distinct municipalities contributing
# dengue_total = sum of dengue cases

obs_by_adm1_year <- merged |>
  group_by(adm_1_name, sigla_uf, join_year) |>
  summarise(
    n_obs        = n(),
    n_mun        = n_distinct(IBGE_code),
    dengue_total = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(adm_1_name, join_year)

# Wide version: rows = states, one column per year
obs_wide <- obs_by_adm1_year |>
  select(adm_1_name, sigla_uf, join_year, n_obs) |>
  pivot_wider(names_from  = join_year,
              values_from = n_obs,
              names_prefix = "y",
              values_fill  = 0)

cat("\n=== Dengue observations (municipality-month rows) by Admin1 and year ===\n")
print(obs_wide, n = Inf)

write_csv(obs_by_adm1_year,  "Results/02_obs_by_adm1_year_long.csv")
write_csv(obs_wide,          "Results/02_obs_by_adm1_year_wide.csv")


write_csv(merged,            "00_Data/merged_only_month.csv")
write_csv(merged_adm1,       "00_Data/merged_adm1_only_month.csv")
