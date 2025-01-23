#Load packages
library(tidyverse)

temp_rep_df <- read_csv("./00_data/temp_rep_data.csv")

num_df <- temp_rep_df %>% 
  filter(type == "num_data_points")

completeness_df <- temp_rep_df %>% 
  filter(type == "completeness")

length_df <- temp_rep_df %>% 
  filter(type == "period")

zero_option_data <- read_csv("./00_data/zero_option_data.csv") %>% 
  select(year, zero_option, LPI_final:CI_high)
# 
zero_option_data %>%
  ggplot(data = ., aes(x = year, y = LPI_final, color = zero_option)) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "black") +
  geom_line(aes(color = zero_option)) +
  geom_ribbon(aes(
    ymin = CI_low,
    ymax = CI_high,
    fill = zero_option
  ), alpha = 0.5) +
  scale_colour_manual(
    values = c(
      "min_value" = "#CC3D24FF",
      "NAs" = "#F3C558FF",
      "onepercent_mean" = "#6DAE90FF",
      "plusone" = "#30B4CCFF",
      "small_value" = "#004F7AFF"
    ),   
    labels = c("minimum value", "NA", "1% of the mean", "+ 1", "+ 0.0000001"),
    guide  = "none"
  ) +
  scale_fill_manual(
    values = c(
      "min_value" = "#CC3D24FF",
      "NAs" = "#F3C558FF",
      "onepercent_mean" = "#6DAE90FF",
      "plusone" = "#30B4CCFF",
      "small_value" = "#004F7AFF"
    ),
    labels = c("minimum value", "NA", "1% of the mean", "+ 1", "+ 0.0000001")
  ) +
  labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Replacement of Zeroes") +
  # ylim(0.5, 1.55) +
  theme_classic()
#   