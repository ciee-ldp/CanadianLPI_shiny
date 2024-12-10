#### sensitivity of the canadian LPI
#### date created: 2024-07-05
#### date last modified: 2024-12-09

#### table of contents 
# 0: setup 
## 0.1: load packages 
## 0.2: load & tidy data

# 1: run LPI 
## 1.1: run unweighted C-LPI
## 1.2: run weighted C-LPI

# 2: confidence intervals
## 2.1: calculate confidence intervals (3 methods)
## 2.2: width of confidence intervals (3 methods)

# 3: shifting baselines
## 3.1: compare shifting baseline trends
## 3.2: compare avg rates of change per baseline
## 3.3: compare avg lambda values per baseline

# 4: modelling decisions (linear vs log linear vs GAM)
## 4.1: process data for modelling decisions
## 4.2: compare modelling decisions

# 5: short/sparse time series 
## 5.1: process data for short/sparse analysis
## 5.2:


# 6: excluding outliers

# 4: treatment of zeroes 

# 7: weighted vs unweighted trend comparison

# 8: manuscript figures 


#### 0.1: load packages ----
# install.packages("collapse")
# install.packages("scico")
library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(rlpi)
library(here)
library(tibble)
library(scico)
library(ggpubr)
library(RColorBrewer)

#### 0.2: load & tidy data ----
# set source of bootstrap_by_rows() function
source(here("02_scripts","function_bootstrap_rows.R"))

# set source of calculate_index_lambdas() function
source(here("02_scripts","calculate_index_lambdas.R"))

# load data
# original canadian data
cad <- read.csv(here("00_data", "CAD_Paper_withzeroes.csv"), na.strings="NULL", header=TRUE)  %>%
  mutate(X2009 = as.character(X2009)) %>%             # set from numeric to character so following line works
  mutate(across(X1970:X2022, ~na_if(., "Null"))) %>%  # replace any "Null" to NA 
  rename(original_id = ID) %>%      # set "ID" to original_id, and make a new ID corresponding to rownumber to avoid issues
  mutate(ID = row_number()) %>% 
  rename(Taxa = Taxonomic_group) %>%                      # rename this column to Taxa
  mutate(Taxa = case_when(Taxa=="Mammalia" ~ "Mammals",   # rename these redundant labels
                          Taxa=="Reptilia" ~ "Herps", 
                          TRUE ~ Taxa))

# save (original) data with zeros 
cad_z <- cad 

# change all zeros to NA in population abundance columns (ie. X1970-X2022 values)
cad <- cad %>% 
  mutate(case_when(if_any(X1970:X2022, ~. ==0) ~ NA, 
                   TRUE ~ .))  # set all zeros to NA

# join binomial spp name to pop_lambdas
cad.names <- cad %>% 
  dplyr::select(ID, Binomial) %>% 
  rename(population_id = ID)


# unweighted LPI output 
u_cad <- read.csv(here("01_outdata", "unweighted-LPI.csv"), header=TRUE) %>% 
  column_to_rownames(var="X")

# population-level lambdas
pop_lambdas <- read.csv(here("01_outdata", "unweighted_pops_PopLambda.txt"), header=TRUE)
pop_lambdas <- left_join(pop_lambdas, cad.names) # add spp name to popn lambda file

# species-level lambdas
spp_lambdas <- read.csv(here("01_outdata", "unweighted_pops_lambda.csv"), header=TRUE) 


##### DELETE/MODIFY????

# 
# # load LPIMain output data
# lpi_out <- read.csv(here("02_outdata", "Group3_Confidence-Intervals", "LPI-sensitivity_conf-int_CAD-unweighted-year-CIs.csv"), header=TRUE) 
# 
# # load df with information on zeros
# cad_zeros <- read.csv(here("02_outdata", "Group3_Confidence-Intervals", "CAD_data_zeros.csv"), header=TRUE) 


#### 1.1: run unweighted C-LPI ----
# NOTE: This code is commented out after running the first time. Only re-run if necessary to make modifications, otherwise use 
# LPI outputs (saved & loaded in 0.1: load packages).

# # create infile
# infile_u <- create_infile(cad,
#                           start_col_name = "X1970",  # data start year
#                           end_col_name = "X2022",    # data end year
#                           CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
#                           name = "./01_outdata/unweighted") # name the infile/outputs
# 
# # run LPIMain
# u_cad <- LPIMain(infile_u,
#                  REF_YEAR = 1970,
#                  PLOT_MAX=2022,
#                  LINEAR_MODEL_SHORT_FLAG = TRUE,
#                  BOOT_STRAP_SIZE = 10000, 
#                  DATA_LENGTH_MIN = 3,
#                  VERBOSE=TRUE, 
#                  SHOW_PROGRESS =FALSE, 
#                  force_recalculation = 1)
# 
# # make rownames (year) to separate col
# u_cad <- u_cad %>%
#   mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
#   filter(!year==2023)                          # remove this--we only want till 2022
# rownames(u_cad) <- u_cad$year
# 
# # save file
# write.csv(u_cad,file.path("01_outdata","unweighted-LPI.csv"))

#### 1.2: run weighted C-LPI ----
# IPR, see rob's code

#### 2.1: calculate confidence intervals (3 methods) ----

# The purpose of this script is to run three methods of bootstrapping confidence intervals on the canadian LPI subset. Bootstrapping occurs on 
# unweighted data by population (ie. bootstrapping of population-level lambda values by row), species (ie. bootstrapping of species-level lambda 
# values by row), and within years (ie. bootstrapping of species-level lambda values for a given year). LPIs are resampled *with replacement* 
# 10,000 times on each unit (population, species, year) in the dataset.

# By population:
boot_pop <- bootstrap_by_rows(pop_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_pop_CI <- as.data.frame(boot_pop$interval_data) # save CI intervals in a separate object
boot_pop_CI$year <- as.numeric(boot_pop_CI$year) # change year from character to numeric

# save data to csv--only needs to be saved the first time this code is run (or after revisions)!
# write.csv(boot_pop_CI, file.path("01_outdata", "unweighted-pop-CIs.csv"))

# what was CI range in 2022? 
boot_pop_CI %>% 
  filter(year==2022)
# range = Upper_CI - Lower_CI = 1.10317 - 0.9873199 = 0.1158501

# tidy
boot_pop_df <- boot_pop_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., u_cad %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(boot_pop_df, col="#c1e6db", line_col = "#66C2A5")
#### end of population method

# By species:
boot_spp <- bootstrap_by_rows(spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_spp_CI <- as.data.frame(boot_spp$interval_data) # save CI intervals in a separate object
boot_spp_CI$year <- as.numeric(boot_spp_CI$year ) # change year from character to numeric

# save data to csv--only needs to be saved the first time this code is run (or after revisions)!
# write.csv(boot_spp_CI, file.path("01_outdata", "unweighted-species-CIs.csv"))

# what was CI range in 2022? 
boot_spp_CI %>% 
  filter(year==2022)
# range = Upper_CI - Lower_CI = 1.138598 - 0.8974167 = 0.2411813

# tidy
boot_spp_df <- boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., u_cad %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(boot_spp_df, col="#fdd1c0", line_col = "#FC8D62")
#### end of species method

# By year:
# NOTE: boostrapping by year is done automatically in LPIMain(), so just need to call the output.
# plot
ggplot_lpi(u_cad, col="#d1d9ea", line_col = "#8DA0CB")

# what was CI range in 2022? 
u_cad %>% 
  filter(year==2022)
# range = CI_high - CI_low = 1.069979 - 0.9449429 = 0.1250361
#### end of year method

# Compare all 3 methods:
# create plot
cad_boot_CIs <- ggplot_multi_lpi(list(boot_pop_df, boot_spp_df, u_cad), 
                                 names=c("Population", "Species", "Year"), 
                                 col="Set2", 
                                 facet=TRUE) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=10)); cad_boot_CIs

#### 2.2: width of confidence intervals (3 methods) ----

data_folder_path <- "./01_outdata"

# create method labels
bootstrap_method_label <- c("year bootstrap",
                            "species bootstrap",
                            "population bootstrap")

# assign column labels
col_labs <- list(c("year","CI_low","CI_high"),
                 c("year","Lower_CI","Upper_CI"),
                 c("year","Lower_CI","Upper_CI"))

# assign paths for each method file
bootstrap_method_output_path <- c(file.path(data_folder_path,"unweighted-LPI.csv"),
                                  file.path(data_folder_path,"unweighted-pop-CIs.csv"),
                                  file.path(data_folder_path,"unweighted-species-CIs.csv"))

# read bootstrap data and assign method name
l_bootstrap_data <- map2(bootstrap_method_output_path, col_labs, 
                         ~read_csv(.x) |> 
                           select(contains(.y)) |> 
                           setNames(c("Year","Lower_CI","Upper_CI"))) |> 
  setNames(bootstrap_method_label)

# convert to dataframe
df_bootstrap_data <- l_bootstrap_data |> bind_rows(.id="method")
df_bootstrap_data_range <- df_bootstrap_data %>% 
  mutate(range=Upper_CI-Lower_CI) %>% 
  mutate(method = str_remove_all(method, " bootstrap"), 
         method = str_to_sentence(method))

# create boxplot
img_bootstrap_methods_boxplot <- ggplot(df_bootstrap_data_range, aes(x=method,y=range)) + 
  geom_boxplot() + 
  theme_classic() + 
  ylab("Confidence Interval Range") + 
  xlab("Bootstrap Method") + 
  theme(text = element_text(size=15)) +
  #scale_fill_manual(values = c("#c1e6db", "#fdd1c0", "#d1d9ea")) + 
  guides(fill="none");img_bootstrap_methods_boxplot

#### 3.1: compare shifting baseline trends ----
# Set up directories and infiles:
# Set year range
ini_year <- 1970
fin_year <- 2022
years <- seq(ini_year,fin_year-5,5) # create options for base years until 2015 because 2020 left with only 2 years of data
folders <- c(file.path("01_outdata/baseline_years", years, "linear"), file.path("01_outdata/baseline_years", years, "loglinear"))

# Create directories for pops_lambda files for each year groups
map(folders, \(x) ifelse(!dir.exists(x),dir.create(x, recursive=TRUE),print("Dir exists")))

# save the files: data_infile and data_pops from create_infile to folders by years to run LPIMain on each one
# created the infile for the same year range but changed the REF_YEAR on LPIMain
system.time(map(folders, \(x) create_infile(cad,
                                            start_col_name="X1970",
                                            end_col_name = "X2022",
                                            CUT_OFF_YEAR = ini_year,
                                            name=paste0(x, "/C-LPI"))))

# Next, compute LPI where short time series are modelled using linear regression:
# calculate LPI for each baseline (1970-2015 at 5-yr intervals)
# short time series are modelled using linear regression
baselines_linear <- map(years, \(x) LPIMain(file.path("01_outdata/baseline_years",x,"linear/C-LPI_infile.txt"),
                                            REF_YEAR = x,
                                            PLOT_MAX=2022,
                                            BOOT_STRAP_SIZE = 10000, 
                                            DATA_LENGTH_MIN = 3, 
                                            VERBOSE=TRUE, 
                                            SHOW_PROGRESS =FALSE, 
                                            force_recalculation = 1, 
                                            LINEAR_MODEL_SHORT_FLAG = TRUE))

# read in lambda files
linear_folders <- file.path("01_outdata/baseline_years", years, "linear")
baselines_linear_lambdas <- map(linear_folders, \(x) read.csv(file.path(x,"C-LPI_pops_lambda.csv"))) |>
  setNames(years)

# bootstrap each LPI baseline output by species to generate CIs lists to write bootstrap results to
linear_bootstrap_result <- list()
linear_bootstrap_list <- list()

# iterate the species bootstrapping over each baseline
for(i in 1:length(years)){
  linear_bootstrap_result <- bootstrap_by_rows(
    baselines_linear_lambdas[[i]],
    species_column_name = "Binomial", 
    start_col_name = paste0("X", years[i]),
    end_col_name = "X2022", 
    iter = TRUE, 
    N = 10000
  )
  linear_bootstrap_list[[i]] <- as.data.frame(linear_bootstrap_result$interval_data)
  names(linear_bootstrap_list)[i] <- paste0("spp_bootstrap_", years[i])
}

# load in lpi data
baselines_linear_df <- map(baselines_linear, \(x)  x |> rownames_to_column(var="Year")) |> setNames(years) |> bind_rows(.id="initial_year")

# Plot LPI:
# join lpi index data with species bootstrapped CIs
baselines_linear_plot <- map(linear_bootstrap_list, \(x)  x |> rownames_to_column(var="Year")) |> 
  setNames(years) |> 
  bind_rows(.id="initial_year") |>
  dplyr::select(initial_year, year, Lower_CI, Upper_CI) |>
  rename(Year = year) |>
  left_join(baselines_linear_df, ., by=c("initial_year", "Year")) |>
  dplyr::select(-c(CI_low, CI_high)) |> 
  mutate(Year=as.numeric(Year)) |> 
  ggplot() + 
  geom_ribbon(aes(x = Year, y = LPI_final, ymax = Upper_CI , ymin = Lower_CI,fill=as.factor(initial_year)), alpha = 0.2) +
  geom_line(aes(x = Year, y = LPI_final, colour = as.factor(initial_year)), linewidth = 0.5) + 
  ylab("Index") + 
  #coord_cartesian(ylim = c(0, 2)) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(ini_year, fin_year, 5)) +
  scale_y_continuous(limits=c(0.8,1.2), breaks=seq(0.5,1.5,0.1)) + 
  scale_fill_scico_d(palette = "roma",name="Reference\n year") +
  scale_color_scico_d(palette = "roma",name="Reference\n year")+ 
  theme(axis.text.x = element_text(angle = 90), 
        text=element_text(size=15), 
        legend.position = "top", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10)); baselines_linear_plot


#### 3.2: compare avg rates of change per baseline ----

# join info from all baseline years into one dataframe
baselines_linear_df <- map(baselines_linear, \(x)  x |> rownames_to_column(var="Year")) |> setNames(years) |> bind_rows(.id="initial_year")

# get mean lpi values
baselines_linear_mean_lpi <- baselines_linear_df  |> 
  group_by(initial_year) |> 
  summarise(min_lpi = min(LPI_final),
            mean_lpi = mean(LPI_final),
            max_lpi = max(LPI_final),
            sd_lpi = sd(LPI_final))

# plot mean values 
baselines_linear_mean_lpi_boxplot <- baselines_linear_df |> 
  ggplot()  + 
  geom_boxplot(aes(x = as.factor(initial_year), y = LPI_final)) + 
  ylab("Index") + 
  xlab("Reference year") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), 
        text=element_text(size=15)); baselines_linear_mean_lpi_boxplot

#### 3.3: compare avg lambda values per baseline ----
# load lambda files
baselines_linear_lambdas <- map(linear_folders, \(x) read.csv(file.path(x,"C-LPI_pops_lambda.csv")))

# create dataframe with all lambda files and convert to tidy format
baselines_linear_lambdas_df <- baselines_linear_lambdas |>
  setNames(years) |> 
  bind_rows(.id="initial_year") |> 
  dplyr::select(-X) |> 
  pivot_longer(starts_with("X"),names_to="Year",values_to="lambdas") |> 
  mutate(Year=as.numeric(gsub("X","",Year)))

# plot all lambda values 
baselines_linear_lambdas_boxplot <- ggplot(baselines_linear_lambdas_df) + 
  geom_boxplot(aes(x = as.factor(initial_year), y = lambdas)) + 
  ylab("Index") + xlab("Year") +
  theme_bw() + scale_color_discrete(name="Initial year");baselines_linear_lambdas_boxplot
# ggsave(filename = "03_figures/baselines_linear_lambdas_boxplots.png",baselines_linear_lambdas_boxplot,width=7,height=5)

# get mean values
baselines_linear_mean_lambdas <- baselines_linear_lambdas_df |> 
  group_by(initial_year,Year) |> 
  summarise(mean_lambda=mean(lambdas,na.rm=T)) |> 
  na.omit()

# plot mean lambda for each year (regardless of baseline)
baselines_linear_mean_lambdas_boxplot <- ggplot(baselines_linear_mean_lambdas |> filter(mean_lambda<1)) + # to remove initial years with 1 values
  geom_boxplot(aes(x = as.factor(initial_year), y = mean_lambda)) + 
  ylab("Mean lambda") + 
  xlab("Reference year") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), 
        text=element_text(size=15));baselines_linear_mean_lambdas_boxplot

#### 3.4: summary statistics per baseline ----
# how many time series are included in each baseline?

# specify empty output objects
num_popns <- c()
baseline_df <- data.frame()

# count number of time series without exclusively NAs for each baseline subset 
# NOTE: reference year is not included in determining whether there are only NAs, since reference year may be set to 1 even if there is no data past the reference year.
for(i in 1:length(years)){
  num_popns <- baselines_linear_lambdas[[i]] %>%
    mutate(non_na_count = rowSums(across(paste0("X", years[i]+1):X2022, ~ !is.na(.)))) %>% 
    filter(non_na_count>0) %>% 
    nrow()
  
  baseline_df[i,1] <- years[i]
  baseline_df[i,2] <- num_popns
}

baseline_df

# what was the minimum final LPI value? baseline==1995
baselines_linear_df %>% 
  filter(initial_year==1995 & Year==2022) # 0.970485

# what was the maximum final LPI value? baseline==1970
baselines_linear_df %>% 
  filter(initial_year==1970 & Year==2022) # 1.005511


#### 4.1: process data for modelling decisions ----
# Here we investigate how various modelling decisions affect the LPI trend line. In the first case, short time series (<6 data points) are 
# modelled in a log linear regression and longer time series (≥6 data points) are modelled using a general additive model (GAM). In the 
# second case, short time series are modelled using a linear regression and longer time series are modelled using a GAM. We also compared 
# this to a case where all time series, regardless of length, were modelled using a GAM. Note that time series with only 2 data points are 
# excluded from all analyses. Confidence intervals use the species bootstrapping method as in section 1.1.

# short time series modelled as log-linear & long time series as GAM:
# create infile 
infile_loglin <- create_infile(cad,
                               start_col_name = "X1970",  # data start year
                               end_col_name = "X2022",    # data end year
                               CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
                               name = "./01_outdata/loglin") # name the infile/outputs

# run LPI 
lpi_lm_false <- LPIMain(infile_loglin, 
                        REF_YEAR = 1970,
                        PLOT_MAX=2022,
                        BOOT_STRAP_SIZE = 10000, 
                        DATA_LENGTH_MIN = 3, 
                        VERBOSE=TRUE, 
                        SHOW_PROGRESS =FALSE, 
                        force_recalculation = 1,
                        LINEAR_MODEL_SHORT_FLAG = FALSE)

# read in species lambda file 
spp_lambdas_loglin <- read.csv(here("01_outdata", "loglin_pops_lambda.csv"), header=TRUE) 

# bootstrap by species to get CIs for lpi_lm_false
boot_spp_loglin <- bootstrap_by_rows(spp_lambdas_loglin, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_spp_loglin <- as.data.frame(boot_spp_loglin$interval_data) # save CI intervals in a separate object
boot_spp_loglin$year <- as.numeric(boot_spp_loglin$year) # change year from character to numeric

boot_spp_loglin_df <- boot_spp_loglin %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., 
            lpi_lm_false %>% 
              rownames_to_column(var="year") %>% 
              dplyr::select(year, LPI_final) %>% 
              filter(!year==2023) %>% 
              mutate(year = as.numeric(year)), 
            by="year") %>% 
  column_to_rownames(var="year")
#### end of section where short time series = log-linear

# short time series modelled as linear & long time series as GAM:
# create infile 
infile_lin <- create_infile(cad,
                            start_col_name = "X1970",  # data start year
                            end_col_name = "X2022",    # data end year
                            CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
                            name = "./01_outdata/lin") # name the infile/outputs

# run LPI 
lpi_lm_true <- LPIMain(infile_lin, 
                       REF_YEAR = 1970,
                       PLOT_MAX=2022,
                       BOOT_STRAP_SIZE = 10000, 
                       DATA_LENGTH_MIN = 3, 
                       VERBOSE=TRUE, 
                       SHOW_PROGRESS =FALSE, 
                       force_recalculation = 1,
                       LINEAR_MODEL_SHORT_FLAG = TRUE)

# read in species lambda file 
spp_lambdas_lin <- read.csv(here("01_outdata", "lin_pops_lambda.csv"), header=TRUE) 

# bootstrap by species to get CIs for lpi_lm_false
boot_spp_lin <- bootstrap_by_rows(spp_lambdas_lin, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_spp_lin <- as.data.frame(boot_spp_lin$interval_data) # save CI intervals in a separate object
boot_spp_lin$year <- as.numeric(boot_spp_lin$year) # change year from character to numeric

boot_spp_lin_df <- boot_spp_lin %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi_lm_true %>% 
              rownames_to_column(var="year") %>% 
              dplyr::select(year, LPI_final) %>% 
              filter(!year==2023) %>% 
              mutate(year = as.numeric(year)),
            by="year") %>% 
  column_to_rownames(var="year")
#### end of section where short time series = linear

# all series, regardless of length, are modelled as GAM:
# create infile 
infile_gam <- create_infile(cad,
                            start_col_name = "X1970",  # data start year
                            end_col_name = "X2022",    # data end year
                            CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
                            name = "./01_outdata/gam") # name the infile/outputs

# run LPI where all time series (regardless of length) are modelled as GAM
lpi_gam <- LPIMain(infile_gam, 
                   REF_YEAR = 1970,
                   PLOT_MAX=2022,
                   BOOT_STRAP_SIZE = 10000, 
                   DATA_LENGTH_MIN = 3, 
                   VERBOSE=TRUE, 
                   SHOW_PROGRESS =FALSE, 
                   force_recalculation = 1,
                   GLOBAL_GAM_FLAG_SHORT_DATA_FLAG = TRUE)

# read in species lambda file 
spp_lambdas_gam <- read.csv(here("01_outdata", "gam_pops_lambda.csv"), header=TRUE) 

# bootstrap by species to get CIs for lpi_lm_false
boot_spp_gam <- bootstrap_by_rows(spp_lambdas_gam, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_spp_gam <- as.data.frame(boot_spp_gam$interval_data) # save CI intervals in a separate object
boot_spp_gam$year <- as.numeric(boot_spp_gam$year) # change year from character to numeric

boot_spp_gam_df <- boot_spp_gam %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi_gam %>% 
              rownames_to_column(var="year") %>% 
              dplyr::select(year, LPI_final) %>% 
              filter(!year==2023) %>% 
              mutate(year = as.numeric(year)),
            by="year") %>% 
  column_to_rownames(var="year")
#### end of section where short time series = GAM

#### 4.2: compare modelling decisions ----
# plot 
lpi_model_versions <- ggplot_multi_lpi(list(boot_spp_loglin_df, boot_spp_lin_df, boot_spp_gam_df), 
                                       names=c("log linear (<6 points)", "linear (<6 points)", "GAM"), 
                                       col="Set1", 
                                       facet=TRUE) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); lpi_model_versions

# summary statistics 
# How many populations are modelled using a GAM (≥6 data points in time series), and how many using linear/log linear regression (<6 data points)?
pt_counts <- cad %>% 
  mutate(num.datapoints = rowSums(!is.na(select(., X1970:X2022))))

length(which(pt_counts$num.datapoints<6)) # 1833 time series have less than 6 data points
length(which(pt_counts$num.datapoints>=6)) # 2660 time series have 6 or more data points


#### 5.1: process data for short/sparse analysis ----
# First calculate number of data points, period, and completeness for each time series. 
#calculate number of data points (number of years for each time series that have population abundances)
cad2 <- cad %>% 
  mutate(num.datapoints = rowSums(!is.na(select(., X1970:X2022)))) 

# For time series with ≥15 points: 
greaterthan15points.lpidata <- subset(cad2, num.datapoints >= 15)
nrow(greaterthan15points.lpidata) # 1288

#this calculates the start year, end year, and period (length) of each time series
period_greaterthan15points <- matrix(NA, nrow = nrow(greaterthan15points.lpidata), ncol = 3)

for (i in 1:nrow(period_greaterthan15points)){
  period_greaterthan15points[i,1] <-min(which(!is.na(greaterthan15points.lpidata[i,13:65])))
  period_greaterthan15points[i,2] <-max(which(!is.na(greaterthan15points.lpidata[i,13:65])))
  period_greaterthan15points[i,3] <- period_greaterthan15points[i,2]-period_greaterthan15points[i,1]+1
}

#name and merge them into the dataset
colnames(period_greaterthan15points) <- c("start.year", "end.year", "period")

greaterthan15points.lpidata <- cbind(greaterthan15points.lpidata, period_greaterthan15points)

#calculate a completeness proportion (number of years sampled per length)
greaterthan15points.lpidata$completeness <- greaterthan15points.lpidata$num.datapoints / greaterthan15points.lpidata$period

# #create a random subsample for testing
# random_sample_size_10 <- round(nrow(greaterthan15points.lpidata)*subsample.prop)

# For time series with ≥6 points: 
greaterthan6points.lpidata <- subset(cad2, num.datapoints >= 6)
nrow(greaterthan6points.lpidata) # 2660

#this calculates the start year, end year, and period (length) of each time series
period_greaterthan6points <- matrix(NA, nrow = nrow(greaterthan6points.lpidata), ncol = 3)

for (i in 1:nrow(period_greaterthan6points)){
  period_greaterthan6points[i,1] <-min(which(!is.na(greaterthan6points.lpidata[i,13:65])))
  period_greaterthan6points[i,2] <-max(which(!is.na(greaterthan6points.lpidata[i,13:65])))
  period_greaterthan6points[i,3] <- period_greaterthan6points[i,2]-period_greaterthan6points[i,1]+1
}

#name and merge them into the dataset
colnames(period_greaterthan6points) <- c("start.year", "end.year", "period")

greaterthan6points.lpidata <- cbind(greaterthan6points.lpidata, period_greaterthan6points)

#calculate a completeness proportion (number of years sampled per length)
greaterthan6points.lpidata$completeness <- greaterthan6points.lpidata$num.datapoints / greaterthan6points.lpidata$period


# For time series with ≥3 points: 
greaterthan3points.lpidata <- subset(cad2, num.datapoints >= 3)
nrow(greaterthan3points.lpidata) # 3473

#this calculates the start year, end year, and period (length) of each time series
period_greaterthan3points <- matrix(NA, nrow = nrow(greaterthan3points.lpidata), ncol = 3)

for (i in 1:nrow(period_greaterthan3points)){
  period_greaterthan3points[i,1] <-min(which(!is.na(greaterthan3points.lpidata[i,13:65])))
  period_greaterthan3points[i,2] <-max(which(!is.na(greaterthan3points.lpidata[i,13:65])))
  period_greaterthan3points[i,3] <- period_greaterthan3points[i,2]-period_greaterthan3points[i,1]+1
}

#name and merge them into the dataset
colnames(period_greaterthan3points) <- c("start.year", "end.year", "period")

greaterthan3points.lpidata <- cbind(greaterthan3points.lpidata, period_greaterthan3points)

#calculate a completeness proportion (number of years sampled per length)
greaterthan3points.lpidata$completeness <- greaterthan3points.lpidata$num.datapoints / greaterthan3points.lpidata$period


# For time series with ≥2 points: 
greaterthan2points.lpidata <- subset(cad2, num.datapoints >= 2)
nrow(greaterthan2points.lpidata) # 4145

#this calculates the start year, end year, and period (length) of each time series
period_greaterthan2points <- matrix(NA, nrow = nrow(greaterthan2points.lpidata), ncol = 3)

for (i in 1:nrow(period_greaterthan2points)){
  period_greaterthan2points[i,1] <-min(which(!is.na(greaterthan2points.lpidata[i,13:65])))
  period_greaterthan2points[i,2] <-max(which(!is.na(greaterthan2points.lpidata[i,13:65])))
  period_greaterthan2points[i,3] <- period_greaterthan2points[i,2]-period_greaterthan2points[i,1]+1
}

#name and merge them into the dataset
colnames(period_greaterthan2points) <- c("start.year", "end.year", "period")

greaterthan2points.lpidata <- cbind(greaterthan2points.lpidata, period_greaterthan2points)

#calculate a completeness proportion (number of years sampled per length)
greaterthan2points.lpidata$completeness <- greaterthan2points.lpidata$num.datapoints / greaterthan2points.lpidata$period

#### 5.2: impact of number of data points in time series ----
# Decision 1: How does the number of data points (times in which population abundance was assessed) impact the LPI? Options: at least 2, 
# at least 3 (to match what WWF-Canada currently does), at least 6 (to include only the populations modelled via GAMs), and at least 15 
# (to see what a much larger number does). 

# Calculate the LPI for each category, i.e. time series with >=2, >=3, >=6, >=15 datapoints:
# calculate LPI for time series with at least 15 datapoints
subset_lpi_15 <- LPIMain(create_infile(greaterthan15points.lpidata,
                                       name="01_outdata/15pts+_data",
                                       start_col_name = "X1970",
                                       end_col_name = "X2022",
                                       CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
BOOT_STRAP_SIZE = 10000,
VERBOSE=FALSE, 
PLOT_MAX=2022,
LINEAR_MODEL_SHORT_FLAG = 1,
#DATA_LENGTH_MIN = 3,
force_recalculation = 1
)

# calculate LPI for time series with at least 6 datapoints
subset_lpi_6 <- LPIMain(create_infile(greaterthan6points.lpidata,
                                      name="01_outdata/6pts+_data",
                                      start_col_name = "X1970",
                                      end_col_name = "X2022",
                                      CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
BOOT_STRAP_SIZE = 10000,
VERBOSE=FALSE, 
PLOT_MAX=2022,
LINEAR_MODEL_SHORT_FLAG = 1,
#DATA_LENGTH_MIN = 3,
force_recalculation = 1
)

# calculate LPI for time series with at least 3 datapoints
subset_lpi_3 <- LPIMain(create_infile(greaterthan3points.lpidata,
                                      name="01_outdata/3pts+_data",
                                      start_col_name = "X1970",
                                      end_col_name = "X2022",
                                      CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
BOOT_STRAP_SIZE = 10000,
VERBOSE=FALSE, 
PLOT_MAX=2022,
LINEAR_MODEL_SHORT_FLAG = 1,
# DATA_LENGTH_MIN = 3,
force_recalculation = 1
)

# calculate LPI for time series with at least 2 datapoints
subset_lpi_2 <- LPIMain(create_infile(greaterthan2points.lpidata,
                                      name="01_outdata/2pts+_data",
                                      start_col_name = "X1970",
                                      end_col_name = "X2022",
                                      CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
BOOT_STRAP_SIZE = 10000,
VERBOSE=FALSE,
PLOT_MAX=2022,
LINEAR_MODEL_SHORT_FLAG = 1,
# DATA_LENGTH_MIN = 3,
force_recalculation = 1
)

# remove 2023
subset_lpi_15 <- subset_lpi_15[1:53,]
subset_lpi_6 <- subset_lpi_6[1:53,]
subset_lpi_3 <- subset_lpi_3[1:53,]
subset_lpi_2 <- subset_lpi_2[1:53,]

# Plot all four scenarios (>=15, >=6, >=3, or >=2 datapoints) together:
# order names so it plots in logical order
namesvec <- c("≥2 data points", "≥3 data points", "≥6 data points", "≥15 data points")
namesvec <- factor(namesvec, levels=c("≥2 data points", "≥3 data points", "≥6 data points", "≥15 data points"))

# code plot
num_datapts_plot <- ggplot_multi_lpi(list(subset_lpi_2, subset_lpi_3, subset_lpi_6, subset_lpi_15), 
                                     names = namesvec, 
                                     facet=TRUE) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(size=12));num_datapts_plot


#### 5.3: impact of completeness of time series ----
# subset data
greaterthan75complete.2pts <- subset(greaterthan2points.lpidata, completeness > 0.75)
greaterthan75complete.3pts <- subset(greaterthan3points.lpidata, completeness > 0.75)
greaterthan75complete.6pts <- subset(greaterthan6points.lpidata, completeness > 0.75)
greaterthan75complete.15pts <- subset(greaterthan15points.lpidata, completeness > 0.75)

greaterthan50complete.2pts <- subset(greaterthan2points.lpidata, completeness > 0.5)
greaterthan50complete.3pts <- subset(greaterthan3points.lpidata, completeness > 0.5)
greaterthan50complete.6pts <- subset(greaterthan6points.lpidata, completeness > 0.5)
greaterthan50complete.15pts <- subset(greaterthan15points.lpidata, completeness > 0.5)

greaterthan25complete.2pts <- subset(greaterthan2points.lpidata, completeness > 0.25)
greaterthan25complete.3pts <- subset(greaterthan3points.lpidata, completeness > 0.25)
greaterthan25complete.6pts <- subset(greaterthan6points.lpidata, completeness > 0.25)
greaterthan25complete.15pts <- subset(greaterthan2points.lpidata, completeness > 0.25)

greaterthan0complete.2pts <- subset(greaterthan2points.lpidata, completeness > 0)
greaterthan0complete.3pts <- subset(greaterthan3points.lpidata, completeness > 0)
greaterthan0complete.6pts <- subset(greaterthan6points.lpidata, completeness > 0)
greaterthan0complete.15pts <- subset(greaterthan15points.lpidata, completeness > 0)

# how many time series are included in each subset?
nrow(greaterthan75complete.2pts) # 2186
nrow(greaterthan50complete.2pts) # 3031
nrow(greaterthan25complete.2pts) # 3948
nrow(greaterthan0complete.2pts) # 4145

# run LPI for each subset
completeness75.2pts.lpi <- LPIMain(create_infile(greaterthan75complete.2pts,
                                            name="01_outdata/75+complete_data",
                                            start_col_name = "X1970",
                                            end_col_name = "X2022",
                                            CUT_OFF_YEAR = 1970),
REF_YEAR = 1970,
PLOT_MAX = 2022,
BOOT_STRAP_SIZE = 10000,
# DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
VERBOSE=FALSE, 
LINEAR_MODEL_SHORT_FLAG = 1,
force_recalculation = 1
)

completeness75.3pts.lpi <- LPIMain(create_infile(greaterthan75complete.3pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
REF_YEAR = 1970,
PLOT_MAX = 2022,
BOOT_STRAP_SIZE = 10000,
# DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
VERBOSE=FALSE, 
LINEAR_MODEL_SHORT_FLAG = 1,
force_recalculation = 1
)

completeness75.6pts.lpi <- LPIMain(create_infile(greaterthan75complete.6pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness75.15pts.lpi <- LPIMain(create_infile(greaterthan75complete.15pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness50.2pts.lpi <- LPIMain(create_infile(greaterthan50complete.2pts,
                                                  name="01_outdata/75+complete_data",
                                                  start_col_name = "X1970",
                                                  end_col_name = "X2022",
                                                  CUT_OFF_YEAR = 1970),
                                    REF_YEAR = 1970,
                                    PLOT_MAX = 2022,
                                    BOOT_STRAP_SIZE = 10000,
                                    # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                    VERBOSE=FALSE, 
                                    LINEAR_MODEL_SHORT_FLAG = 1,
                                    force_recalculation = 1
)

completeness50.3pts.lpi <- LPIMain(create_infile(greaterthan50complete.3pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness50.6pts.lpi <- LPIMain(create_infile(greaterthan50complete.6pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness50.15pts.lpi <- LPIMain(create_infile(greaterthan50complete.15pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness25.2pts.lpi <- LPIMain(create_infile(greaterthan25complete.2pts,
                                                  name="01_outdata/75+complete_data",
                                                  start_col_name = "X1970",
                                                  end_col_name = "X2022",
                                                  CUT_OFF_YEAR = 1970),
                                    REF_YEAR = 1970,
                                    PLOT_MAX = 2022,
                                    BOOT_STRAP_SIZE = 10000,
                                    # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                    VERBOSE=FALSE, 
                                    LINEAR_MODEL_SHORT_FLAG = 1,
                                    force_recalculation = 1
)

completeness25.3pts.lpi <- LPIMain(create_infile(greaterthan25complete.3pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness25.6pts.lpi <- LPIMain(create_infile(greaterthan25complete.6pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness25.15pts.lpi <- LPIMain(create_infile(greaterthan25complete.15pts,
                                                 name="01_outdata/75+complete_data",
                                                 start_col_name = "X1970",
                                                 end_col_name = "X2022",
                                                 CUT_OFF_YEAR = 1970),
                                   REF_YEAR = 1970,
                                   PLOT_MAX = 2022,
                                   BOOT_STRAP_SIZE = 10000,
                                   # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                   VERBOSE=FALSE, 
                                   LINEAR_MODEL_SHORT_FLAG = 1,
                                   force_recalculation = 1
)

completeness0.2pts.lpi <- LPIMain(create_infile(greaterthan0complete.2pts,
                                                  name="01_outdata/75+complete_data",
                                                  start_col_name = "X1970",
                                                  end_col_name = "X2022",
                                                  CUT_OFF_YEAR = 1970),
                                    REF_YEAR = 1970,
                                    PLOT_MAX = 2022,
                                    BOOT_STRAP_SIZE = 10000,
                                    # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                    VERBOSE=FALSE, 
                                    LINEAR_MODEL_SHORT_FLAG = 1,
                                    force_recalculation = 1
)

completeness0.3pts.lpi <- LPIMain(create_infile(greaterthan0complete.3pts,
                                                name="01_outdata/75+complete_data",
                                                start_col_name = "X1970",
                                                end_col_name = "X2022",
                                                CUT_OFF_YEAR = 1970),
                                  REF_YEAR = 1970,
                                  PLOT_MAX = 2022,
                                  BOOT_STRAP_SIZE = 10000,
                                  # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                  VERBOSE=FALSE, 
                                  LINEAR_MODEL_SHORT_FLAG = 1,
                                  force_recalculation = 1
)

completeness0.6pts.lpi <- LPIMain(create_infile(greaterthan0complete.6pts,
                                                name="01_outdata/75+complete_data",
                                                start_col_name = "X1970",
                                                end_col_name = "X2022",
                                                CUT_OFF_YEAR = 1970),
                                  REF_YEAR = 1970,
                                  PLOT_MAX = 2022,
                                  BOOT_STRAP_SIZE = 10000,
                                  # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                  VERBOSE=FALSE, 
                                  LINEAR_MODEL_SHORT_FLAG = 1,
                                  force_recalculation = 1
)

completeness0.15pts.lpi <- LPIMain(create_infile(greaterthan0complete.15pts,
                                                name="01_outdata/75+complete_data",
                                                start_col_name = "X1970",
                                                end_col_name = "X2022",
                                                CUT_OFF_YEAR = 1970),
                                  REF_YEAR = 1970,
                                  PLOT_MAX = 2022,
                                  BOOT_STRAP_SIZE = 10000,
                                  # DATA_LENGTH_MIN = 3,     # commented out, since subsetted manually
                                  VERBOSE=FALSE, 
                                  LINEAR_MODEL_SHORT_FLAG = 1,
                                  force_recalculation = 1
)

# remove 2023
completeness75.2pts.lpi <- completeness75.2pts.lpi[1:53,]
completeness75.3pts.lpi <- completeness75.3pts.lpi[1:53,]
completeness75.6pts.lpi <- completeness75.6pts.lpi[1:53,]
completeness75.15pts.lpi <- completeness75.15pts.lpi[1:53,]

completeness50.2pts.lpi <- completeness50.2pts.lpi[1:53,]
completeness50.3pts.lpi <- completeness50.3pts.lpi[1:53,]
completeness50.6pts.lpi <- completeness50.6pts.lpi[1:53,]
completeness50.15pts.lpi <- completeness50.15pts.lpi[1:53,]

completeness25.2pts.lpi <- completeness25.2pts.lpi[1:53,]
completeness25.3pts.lpi <- completeness25.3pts.lpi[1:53,]
completeness25.6pts.lpi <- completeness25.6pts.lpi[1:53,]
completeness25.15pts.lpi <- completeness25.15pts.lpi[1:53,]

completeness0.2pts.lpi <- completeness0.2pts.lpi[1:53,]
completeness0.3pts.lpi <- completeness0.3pts.lpi[1:53,]
completeness0.6pts.lpi <- completeness0.6pts.lpi[1:53,]
completeness0.15pts.lpi <- completeness0.15pts.lpi[1:53,]


# Plot all scenarios
completeness75_plot <- ggplot_multi_lpi(list(completeness75.2pts.lpi, completeness75.3pts.lpi, completeness75.6pts.lpi, completeness75.15pts.lpi),
                                      names = c("≥2 points", "≥3 points", "≥6 points", "≥15 points"), 
                                      facet=FALSE) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); completeness75_plot

completeness50_plot <- ggplot_multi_lpi(list(completeness50.2pts.lpi, completeness50.3pts.lpi, completeness50.6pts.lpi, completeness50.15pts.lpi),
                                        names = c("≥2 points", "≥3 points", "≥6 points", "≥15 points"), 
                                        facet=FALSE) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); completeness50_plot

completeness25_plot <- ggplot_multi_lpi(list(completeness25.2pts.lpi, completeness25.3pts.lpi, completeness25.6pts.lpi, completeness25.15pts.lpi),
                                        names = c("≥2 points", "≥3 points", "≥6 points", "≥15 points"), 
                                        facet=FALSE) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); completeness25_plot

completeness0_plot <- ggplot_multi_lpi(list(completeness0.2pts.lpi, completeness0.3pts.lpi, completeness0.6pts.lpi, completeness0.15pts.lpi),
                                        names = c("≥2 points", "≥3 points", "≥6 points", "≥15 points"), 
                                        facet=FALSE) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); completeness0_plot

 

ggarrange(completeness0_plot, 
          completeness25_plot, 
          completeness50_plot, 
          completeness75_plot, 
          labels="auto", 
          font.label = list(size = 15))


### OLD
# code plot
completeness_plot <- ggplot_multi_lpi(list(completeness75.lpi, completeness50.lpi, completeness25.lpi, completeness0.lpi),
                                      names = c(">75% complete", ">50% complete", ">25% complete", ">0% complete"), 
                                      facet=TRUE) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(size=12))

# plot
completeness_plot

# save plot
ggsave(here("03_figures", "completeness.png"), completeness_plot, width=8,height=5)


#### 5.3: impact of period of time series ----


#### 8: manuscript figures ----

## figure 2: confidence intervals (3 methods)
fig2_CIplot <- ggarrange(cad_boot_CIs, img_bootstrap_methods_boxplot, labels="auto", font.label = list(size = 15))
ggsave(here("03_figures", "fig2_CIplot.png"), fig2_CIplot, width=10,height=5)

## figure 4: modelling decisions 
fig4_modelling <- ggplot_multi_lpi(list(boot_spp_loglin_df, boot_spp_lin_df, boot_spp_gam_df), 
                                   names=c("log linear (<6 points)", "linear (<6 points)", "GAM"), 
                                   col="Set1", 
                                   facet=TRUE) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); fig4_modelling
ggsave(here("03_figures", "fig4_modelling.png"), fig4_modelling, width=8, height=5)

## figure 6: shifting baselines
fig6_baselinesplot <- ggarrange(baselines_linear_plot, 
                                baselines_linear_mean_lpi_boxplot, 
                                baselines_linear_mean_lambdas_boxplot, 
                                labels="auto", font.label = list(size = 15), ncol=3); fig6_baselinesplot
ggsave(filename = "03_figures/fig6_baselinesplot.png",fig6_baselinesplot,width=15,height=5)


## short/sparse
num_datapts_plot 


