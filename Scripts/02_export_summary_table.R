##' --- 
#' title: "02 Export Summary Table" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-19" 
#' --- 

#' Overview: 
#'   export table of dengue data by district and year to docx; save to Results/;


#' Timeline: 
#'   2026-06-19 initial
library(tidyverse)
library(flextable)
library(officer)


#load data
obs_long <- read_csv("Results/02_case_by_adm1_year_long_week_month.csv")


table_wide <- obs_long |>
  select(adm_1_name, sigla_uf, join_year, dengue_total) |>
  mutate(
   
    State = paste0(str_to_title(adm_1_name), " (", sigla_uf, ")"),
  
    join_year = paste0("Y", join_year)
  ) |>
  pivot_wider(
    id_cols     = State,
    names_from  = join_year,
    values_from = dengue_total,
    values_fill = 0
  ) |>

  select(State, sort(tidyselect::peek_vars())) |>
  arrange(State)


year_cols <- setdiff(names(table_wide), "State")

table_wide <- table_wide |>
  mutate(Total = rowSums(across(all_of(year_cols)), na.rm = TRUE))


totals_row <- table_wide |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) |>
  mutate(State = "Total")

table_final <- bind_rows(table_wide, totals_row)


n_cols      <- ncol(table_final)
n_rows      <- nrow(table_final)
n_data_rows <- n_rows - 1   # exclude totals row

ft <- flextable(table_final, col_keys = names(table_final)) |>
  
  # --- column header labels: strip the Y prefix from year columns ---
  set_header_labels(State = "State") |>
  set_header_labels(values = setNames(
    gsub("^Y", "", year_cols), year_cols
  )) |>
  
  # --- number formatting: comma thousands, no decimals ---
  colformat_num(
    j      = year_cols,
    big.mark = ",",
    digits = 0
  ) |>
  colformat_num(
    j      = "Total",
    big.mark = ",",
    digits = 0
  ) |>
  
  # --- add a spanning header row ---
  add_header_row(
    values  = c("", "Dengue cases by year", ""),
    colwidths = c(1, length(year_cols), 1),
    top     = TRUE
  ) |>
  
  # --- alignment ---
  align(align = "left",   part = "header", j = 1) |>
  align(align = "center", part = "header", j = seq(2, n_cols)) |>
  align(align = "left",   part = "body",   j = 1) |>
  align(align = "right",  part = "body",   j = seq(2, n_cols)) |>
  
  # --- bold header and totals row ---
  bold(part = "header") |>
  bold(i = n_rows, part = "body") |>
  
  # --- shade the totals row ---
  bg(i = n_rows, bg = "#D9E1F2", part = "body") |>
  
  # --- shade alternate data rows for readability ---
  bg(
    i   = seq(2, n_data_rows, by = 2),
    bg  = "#F2F2F2",
    part = "body"
  ) |>
  
  # --- shade header ---
  bg(bg = "#2E5F8A", part = "header") |>
  color(color = "white", part = "header") |>
  
  # --- borders ---
  border_remove() |>
  hline_top(border = fp_border(color = "#2E5F8A", width = 2), part = "header") |>
  hline(
    i      = 1,
    border = fp_border(color = "white", width = 1),
    part   = "header"
  ) |>
  hline_bottom(border = fp_border(color = "#2E5F8A", width = 2), part = "header") |>
  hline_bottom(border = fp_border(color = "#2E5F8A", width = 2), part = "body") |>
  hline(
    i      = n_data_rows,
    border = fp_border(color = "#2E5F8A", width = 1),
    part   = "body"
  ) |>
  vline(
    j      = 1,
    border = fp_border(color = "#CCCCCC", width = 0.5),
    part   = "body"
  ) |>
  
  # --- font ---
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 9,  part = "body") |>
  fontsize(size = 9,  part = "header") |>
  
  # --- column widths: state col wider, year cols narrower ---
  width(j = 1,               width = 1.8) |>
  width(j = seq(2, n_cols),  width = 0.65) |>
  
  # --- row heights ---
  height_all(height = 0.25) |>
  
  # --- caption ---
  set_caption(
    caption = "Table 1. Dengue case counts by Brazilian state and year.",
    autonum = run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "tab_dengue_adm1")
  )

#export to docx
doc <- read_docx() |>
  body_add_par(
    "Dengue Cases by Administrative Region (Admin1), Brazil",
    style = "heading 1"
  ) |>
  body_add_par(
    paste0(
      "Dengue case counts aggregated to state level by year. ",
      "Rows represent Brazilian states; columns represent calendar years. ",
      "Values are summed from municipality-month observations."
    ),
    style = "Normal"
  ) |>
  body_add_par("", style = "Normal") |>   # blank line before table
  body_add_flextable(ft) |>
  body_add_par("", style = "Normal") |>
  body_add_par(
    paste0("Source: Dengue data filtered to Brazil (Admin2, monthly resolution). ",
           "Generated: ", Sys.Date(), "."),
    style = "Normal"
  ) |>
  # Landscape orientation 
  body_end_section_landscape()

output_path <- "Results/dengue_by_adm1_table.docx"
print(doc, target = output_path)
cat("Saved:", output_path, "\n")


