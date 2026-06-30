#' --- 
#' title: "02 Check distribution type of dengue count data" 
#' author: "Natalia Cuartas" 
#' date: "2026-06-29" 
#' --- 

#' Overview: 
#'  visualize the dengue count data and check whether the distribution is 
#'  poisson or negative binomial

#' Timeline: 
#'   2026-06-29 
#' 


library(tidyverse)
merged <- read_csv("00_Data/merged_week_month.csv")


# Histogram
ggplot(merged, aes(x = dengue_total)) +
  geom_histogram(bins = 50, fill = "#2166ac") +
  theme_bw() +
  labs(title = "Distribution of dengue_total")

# Histogram, log scale (helps see the skew clearly)
ggplot(merged, aes(x = dengue_total)) +
  geom_histogram(bins = 50, fill = "#2166ac") +
  scale_x_log10() +
  theme_bw() +
  labs(title = "Distribution of dengue_total (log scale)")

#check mean vs variance
mean(merged$dengue_total, na.rm = TRUE)
var(merged$dengue_total,  na.rm = TRUE)

#check proportion of zeros
mean(merged$dengue_total == 0, na.rm = TRUE)


