#Load packages
library(tidyverse)
library(paletteer)

temp_rep_df <- read_csv("./00_data/temp_rep_data.csv")

num_df <- temp_rep_df %>% 
  filter(type == "num_data_points")

completeness_df <- temp_rep_df %>% 
  filter(type == "completeness")

length_df <- temp_rep_df %>% 
  filter(type == "period")

zeros_df <- read_csv("./01_outdata/zeros/zeros_df.csv")

modelling_df <- read_csv("./01_outdata/modelling/modelling_df.csv")

credible_df <- read_csv("./01_outdata/credible_intervals/credible_df.csv")

outlier_df <- read_csv("./01_outdata/outliers/upr_lwr_outliers_CIspp.csv")

base_df <- read_csv("./01_outdata/baseline_years/CLPI_baseline-years_sppCIs.csv")

weight_df <- read_csv("./01_outdata/weighted/weight_df.csv")
