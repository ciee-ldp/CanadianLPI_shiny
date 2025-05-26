#Load packages
library(tidyverse)

temp_rep_df <- read_csv("./00_data/temp_rep_data.csv")

num_df <- temp_rep_df %>% 
  filter(type == "num_data_points")

completeness_df <- temp_rep_df %>% 
  filter(type == "completeness")

length_df <- temp_rep_df %>% 
  filter(type == "period")

zeros_df <- read_csv("./01_outdata/zeros/zeros_df.csv")

modelling_df <- read_csv("./01_outdata/zeros/modelling_df.csv")

credible_df <- read_csv("./01_outdata/zeros/credible_df.csv")