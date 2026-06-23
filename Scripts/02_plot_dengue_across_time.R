##' --- 
#' title: "02 Tabulations and summary plots" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-18" 
#' --- 

#' Overview: 
#'   tabulate dengue data by district and year; save to Results/;
#'   plot dengue cases across time by admin 1 level, including natural disasters;
#'    save to  Dev_figures
#'    make correlation plot for variables by dengue cases and disaster types

#' Timeline: 
#'   2026-06-18 initial
#'   2026-06-19 add correlation plots
#'   2026-06-20 add disaster summary table

library(tidyverse)
install.packages("corrplot")
library(corrplot)

merged <- read_csv("00_Data/merged_week_month.csv")
disasters  <- read_csv("00_Data/nat_dis_bra.csv")
adm1_lookup <- merged |>
  distinct(IBGE_code, adm_1_name, sigla_uf) |>
  filter(!is.na(adm_1_name))
adm1_lookup <- adm1_lookup |> mutate(IBGE_code = as.character(IBGE_code))
#------------tabulate dengue cases by admin 1 district and year:
case_by_adm1_year <- merged |>
  group_by(adm_1_name, sigla_uf, join_year) |>
  summarise(
    n_obs        = n(),
    dengue_total = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(adm_1_name, join_year)

# Wide version: rows = states, one column per year
case_wide <- case_by_adm1_year |>
  select(adm_1_name, sigla_uf, join_year, dengue_total) |>
  pivot_wider(names_from  = join_year,
              values_from = dengue_total,
              names_prefix = "y",
              values_fill  = 0)

#print table:
cat("\n=== Dengue observations (municipality-month rows) by Admin1 and year ===\n")
print(case_wide, n = Inf)

#save tables to results
write_csv(case_by_adm1_year,  "Results/02_case_by_adm1_year_long_week_month.csv")
write_csv(case_wide,          "Results/02_case_by_adm1_year_wide_week_month.csv")

#----------plot
#aggregate to admin 1 month
dengue_adm1 <- merged |>
  group_by(adm_1_name, sigla_uf, join_year, join_month) |>
  summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
  mutate(date = as.Date(paste(join_year, join_month, "01", sep = "-"))) |>
  filter(!is.na(adm_1_name))


#filter disasters for plotting

disasters_plot <- disasters |>
  mutate(
    cod_ibge_mun        = as.character(cod_ibge_mun),
    date                = as.Date(data_evento),
    descricao_tipologia = as.character(descricao_tipologia)
  ) |>
  filter(!is.na(date), !is.na(descricao_tipologia),
         descricao_tipologia != "") |>
  left_join(adm1_lookup, by = c("cod_ibge_mun" = "IBGE_code")) |>
  filter(!is.na(adm_1_name)) |>
  # Keep only dates within the dengue date range
  semi_join(dengue_adm1 |> distinct(adm_1_name),
            by = "adm_1_name") |>
  filter(date >= min(dengue_adm1$date),
         date <= max(dengue_adm1$date))


# print disaster groups
disaster_groups <- sort(unique(disasters_plot$descricao_tipologia))

cat("=== Available descricao_tipologia values ===\n")
print(disaster_groups)
#"Alagamentos"                     "Chuvas Intensas"                
#[3] "Doenças infecciosas"             "Enxurradas"                     
#[5] "Erosão"                          "Estiagem e Seca"                
#[7] "Granizo"                         "Incêndio Florestal"             
#[9] "Inundações"                      "Movimento de Massa"             
#[11] "Onda de Calor e Baixa Umidade"   "Onda de Frio"                   
#[13] "Outros"                          "Rompimento/Colapso de barragens"
#[15] "Tornado"                         "Vendavais e Ciclones" 


# bar colour
group_colour <- "#2166ac"

# --- plot function
plot_state <- function(state_name, dengue_df, disaster_df, group) {
  
  d_deng <- dengue_df |> filter(adm_1_name == state_name)
  d_dis  <- disaster_df |> filter(adm_1_name == state_name,
                                  descricao_tipologia == group)
  
  y_max      <- max(d_deng$dengue_total, na.rm = TRUE) * 1.15
  bar_height <- y_max
  
  p <- ggplot() +
    geom_line(
      data = d_deng,
      aes(x = date, y = dengue_total),
      colour = "black", linewidth = 0.7
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = state_name, subtitle = group, x = "Year", y = "Dengue cases") +
    theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  # add bars if there are events for this group in this state
  if (nrow(d_dis) > 0) {
    p <- p +
      geom_segment(
        data = d_dis,
        aes(x = date, xend = date, y = 0, yend = bar_height),
        colour = group_colour, linewidth = 0.35, alpha = 0.6
      )
  }
  
  p
}

#single state (sao paulo)
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Alagamentos")
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Enxurradas")
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Estiagem e Seca")
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Chuvas Intensas")
  #chuvias intensas only since 2013
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Inundações")
plot_state("SAO PAULO", dengue_adm1, disasters_plot, "Movimento de Massa")
#rio de janeiro
plot_state("RIO DE JANEIRO", dengue_adm1, disasters_plot, "Movimento de Massa")
plot_state("RIO DE JANEIRO", dengue_adm1, disasters_plot, "Estiagem e Seca")
plot_state("RIO DE JANEIRO", dengue_adm1, disasters_plot, "Alagamentos")
plot_state("RIO DE JANEIRO", dengue_adm1, disasters_plot, "Inundações")








#---------plot dengue over time (no disasters)
plot_dengue_adm1 <- function(state, data = merged) {
  
  d <- data |>
    filter(adm_1_name == state) |>
    group_by(calendar_start_date) |>
    summarise(dengue_total = sum(dengue_total, na.rm = TRUE), .groups = "drop") |>
    arrange(calendar_start_date)
  
  plot(d$calendar_start_date, d$dengue_total,
       type = "o",
       pch  = 19,
       ylab = "Number of dengue cases per month",
       xlab = "Time",
       main = paste("Dengue cases —", str_to_title(state)))
}

# plot
plot_dengue_adm1("SAO PAULO")
plot_dengue_adm1("RIO DE JANEIRO")
plot_dengue_adm1("BAHIA") 
plot_dengue_adm1("MINAS GERAIS")











#-------pair correlation plots
view(merged)
#correlation plot with side variables
cor_vars <- merged |>
  select(dengue_total,
         dh_total_dam_human_sum,
         dm_pub_infra_damaged_sum,
         dh_homeless_sum,
         priv_pub_sum,
         pub_pot_water_supply_sum,
         pub_sewage_sum,
         pub_vector_control_sum,
         flag_water_deplete,
         flag_water_contam) |>
  drop_na()

cor_matrix <- cor(cor_vars, method = "spearman")  # spearman better for skewed count data

corrplot(cor_matrix,
         method  = "color",
         type    = "upper",
         addCoef.col = "black",
         tl.col  = "black",
         tl.srt  = 45,
         diag    = FALSE)

#correlation plot with disaster types
cor_dis_group <- merged |>
  select(dengue_total,
         n_climatological,
         n_hidrological,
         n_meteorological) |>
  drop_na()

cor_matrix_dis_group <- cor(cor_dis_group, method = "spearman")  # spearman better for skewed count data

corrplot(cor_matrix_dis_group,
         method  = "color",
         type    = "upper",
         addCoef.col = "black",
         tl.col  = "black",
         tl.srt  = 45,
         diag    = FALSE)


#tabulate disasters by year by state and copy to clipboard

summarise_disasters("inunda")
tbl <- summarise_disasters("inunda")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

summarise_disasters <- function(search_term, data = disasters_plot) {
  
  wide <- data |>
    filter(!is.na(adm_1_name),
           str_detect(tolower(descricao_tipologia), tolower(search_term))) |>
    mutate(year = as.character(year(date))) |>
    group_by(adm_1_name, year) |>
    summarise(n_events = n(), .groups = "drop") |>
    pivot_wider(
      id_cols     = adm_1_name,
      names_from  = year,
      values_from = n_events,
      values_fill = 0
    ) |>
    arrange(adm_1_name)
  
  year_cols <- sort(setdiff(names(wide), "adm_1_name"))
  
  wide |>
    select(adm_1_name, all_of(year_cols)) |>
    mutate(Total = rowSums(across(all_of(year_cols)), na.rm = TRUE))
}

#give disaster names
unique(disasters_plot$descricao_tipologia)
#summarize by disaster type:
summarise_disasters("inunda")
#copy to clipboard
tbl <- summarise_disasters("inunda")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

tbl <- summarise_disasters("enxurradas")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

tbl <- summarise_disasters("alagamentos")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

tbl <- summarise_disasters("estiagem")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)

tbl <- summarise_disasters("movimento")
tbl |> write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)
