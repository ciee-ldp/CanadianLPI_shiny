### global.R ###

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(paletteer)

# Load pre-processed data objects
load("01_outdata/shiny/zeros_df.Rda")
load("01_outdata/shiny/base_df.Rda")
load("01_outdata/shiny/credible_df.Rda")
load("01_outdata/shiny/modelling_df.Rda")
load("01_outdata/shiny/outlier_df.Rda")
load("01_outdata/shiny/weight_df.Rda")
load("01_outdata/shiny/completeness_df.Rda")
