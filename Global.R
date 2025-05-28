### global.R ###

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(paletteer)

# Load pre-processed data objects
# load("01_outdata/shiny/num_df.Rda")
# load("data/length_df.rda")
# load("data/completeness_df.rda")
load("01_outdata/shiny/zeros_df.rda")
load("01_outdata/shiny/base_df.rda")
load("01_outdata/shiny/credible_df.rda")
load("01_outdata/shiny/modelling_df.rda")
load("01_outdata/shiny/outlier_df.rda")
load("01_outdata/shiny/weight_df.rda")
load("01_outdata/shiny/length_df.rda")
load("01_outdata/shiny/completeness_df.rda")
