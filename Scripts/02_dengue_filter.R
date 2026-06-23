dengue_bra <- dengue_raw %>%
  filter(ISO_A0 == "BRA") %>%
# Keep municipality-level rows only (Admin2 with a valid IBGE code)
filter(S_res == "Admin2", !is.na(IBGE_code), !is.na(adm_2_name))
 
# Keep month-resolution rows (avoids double-counting weekly rows)
filter(T_res == "Month") 

dengue_bra |>
  summarise(
    min_date = min(calendar_start_date, na.rm = TRUE),
    max_date = max(calendar_start_date, na.rm = TRUE)
    ) |>
  print()

dengue_bra$T_res
