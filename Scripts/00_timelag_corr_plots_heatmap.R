#funtion to make correlation heatmaps for all states by time lag; function
#to 
#' --- 
#' title: "00 functions for disaster timelag correlation plots" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-25" 
#' --- 

#' Overview: 
#'  #funtion to make correlation heatmaps for all states by time lag and 
#'  disaster type; function to plot the correlation by one state only

#' Timeline: 
#'   2026-06-25 
#' 

#heatmap function
plot_lag_heatmap <- function(data = lag_cors, title_suffix = "") {
  
  # Order states by their peak absolute correlation across lags
  state_order <- data |>
    group_by(adm_1_name) |>
    summarise(max_abs_cor = max(abs(correlation), na.rm = TRUE)) |>
    arrange(desc(max_abs_cor)) |>
    pull(adm_1_name)
  
  data |>
    mutate(adm_1_name = factor(adm_1_name, levels = rev(state_order))) |>
    ggplot(aes(x = lag, y = adm_1_name, fill = correlation)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(is.na(correlation), "NA",
                                 sprintf("%.2f", correlation))),
              size = 2.5) +
    scale_fill_gradient2(
      low      = "#d73027",
      mid      = "white",
      high     = "#1a9850",
      midpoint = 0,
      limits   = c(-1, 1),
      na.value = "grey80",
      name     = "Spearman r"
    ) +
    scale_x_continuous(breaks = unique(data$lag),
                       labels = paste0("Lag ", unique(data$lag))) +
    labs(
      title    = paste0("Spearman correlation: disaster events vs dengue by state and lag",
                        if (title_suffix != "") paste0("\n", title_suffix) else ""),
      subtitle = "States ordered by peak absolute correlation. Grey = insufficient data.",
      x        = "Lag (months)",
      y        = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid  = element_blank(),
      axis.text.y = element_text(size = 8)
    )
}


#usage:
plot_lag_heatmap(lag_cors_floods, "Floods")
plot_lag_heatmap(lag_cors_outbreak_floods, "Floods vs outbreak months")

#individual state correlation graph
plot_lag_cor <- function(state, data = lag_cors, title_suffix = "") {
  
  d <- data |> filter(adm_1_name == state)
  
  if (nrow(d) == 0) stop("State not found: ", state)
  
  ggplot(d, aes(x = lag, y = correlation)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = 0, ymax = correlation),
                fill = "#2166ac", alpha = 0.15) +
    geom_line(colour = "#2166ac", linewidth = 0.8) +
    geom_point(size = 3, colour = "#2166ac") +
    geom_label(aes(label = round(correlation, 2)),
               nudge_y = 0.04, size = 3) +
    scale_x_continuous(breaks = 0:max(data$lag),
                       labels = paste0("Lag ", 0:max(data$lag))) +
    ylim(-1, 1) +
    labs(
      title    = paste0("Spearman correlation: disaster events vs dengue — ",
                        str_to_title(state),
                        if (title_suffix != "") paste0(" (", title_suffix, ")") else ""),
      subtitle = "Lag 0 = concurrent month; Lag 1 = disasters 1 month prior, etc.",
      x        = "Lag (months)",
      y        = "Spearman correlation"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

#usage:
plot_lag_cor("SAO PAULO", data = lag_cors_floods, title_suffix = "Floods")
plot_lag_cor("AMAZONAS",  data = lag_cors_outbreak_floods, title_suffix = "Floods vs outbreak")