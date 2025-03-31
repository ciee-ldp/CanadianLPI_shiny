#### sensitivity of the canadian LPI to analysis decisions
#### date created: 2024-07-05
#### date last modified: 2025-03-31

#### table of contents 
# 0: setup 
## 0.1: load packages 
## 0.2: load & tidy data

# 1: weighted v unweighted C-LPI
## 1.1: run unweighted C-LPI
## 1.2: run weighted C-LPI

# 2: confidence intervals
## 2.1: calculate confidence intervals (3 methods)
## 2.2: width of confidence intervals (3 methods)

# 3: shifting baseline year
## 3.1: compare shifting baseline year trends
## 3.2: compare avg rates of change per baseline year
## 3.3: compare avg lambda values per baseline year

# 4: modelling decisions (linear vs log linear vs GAM)
## 4.1: process data for modelling decisions
## 4.2: compare modelling decisions

# 5: length/fullness of time series 
## 5.1: process data for length/fullness analysis
## 5.2: impact of time series fullness
## 5.3: impact of completeness of time series
## 5.4: impact of time series length

# 6: treatment of zeroes 
## 6.1: process data for treatment of zeros
## 6.2: explore characteristics of zeros 
## 6.3: compare options for treatment of zeros

# 7: excluding outliers
## 7.1: process data for outlier analysis
## 7.2: compare trends removing extreme low outliers
## 7.3: compare trends removing extreme high outliers
## 7.4: compare trends removing extreme high & low outliers

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
library(data.table)

#### 0.2: load & tidy data ----

## set source of bootstrap_by_rows() function
source(here("02_scripts","function_bootstrap_rows.R"))

# set source of calculate_index_lambdas() function
source(here("02_scripts","calculate_index_lambdas.R"))

## load data

# canadian dataset
cad <- read.csv(here("00_data", "CAD_Paper_withzeroes.csv"), na.strings="NULL", header=TRUE)  %>%
  mutate(X2009 = as.character(X2009)) %>%             # set from numeric to character so following line works
  mutate(across(X1970:X2022, ~na_if(., "Null"))) %>%  # replace any "Null" to NA 
  rename(original_id = ID) %>%      # set "ID" to original_id, and make a new ID corresponding to rownumber to avoid issues
  mutate(ID = row_number()) %>% 
  rename(Taxa = Taxonomic_group) %>%                      # rename this column to Taxa
  mutate(Taxa = case_when(Taxa=="Mammalia" ~ "Mammals",   # rename these redundant labels
                          Taxa=="Reptilia" ~ "Herps", 
                          TRUE ~ Taxa))

## Data to exclude: Finding the time series that are only NA
nullids <- cad %>%
  pivot_longer(X1970:X2022, names_to = "Year") %>%   # Changing the data frame to long format; i.e. all values in X1950:X2020 will go into a column "value", and all column names from X1950 to X2020 will go into a column named "Year".
  group_by(ID) %>%  # group_by to ensure the summarise function seperates groups
  summarise(n_datapoints = n()) %>% # count the number of rows
  left_join(., cad %>%  # add another dataset with matching ID column
              pivot_longer(X1970:X2022, names_to = "Year") %>%  #long form
              filter(is.na(value)) %>%  #keep only values that are NA
              group_by(ID) %>%
              summarise(n_null = n()), by = "ID") %>% # Count number of rows (which are only NA in this data set)
  filter(n_datapoints == n_null) %>%  #Select the populations where the number of Nulls == number of data points
  pull(ID)  # create a vector with population ids that are only NA

## Data to exclude: Finding the time series that only have zero values
zeroids <- cad %>%
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  filter(!is.na(value)) %>%   # Remove values that are null
  group_by(ID) %>%
  summarise(n_datapoints = n()) %>%
  left_join(., cad %>%
              pivot_longer(X1970:X2022, names_to = "Year") %>%
              filter(!is.na(value)) %>% # Remove values that are null
              filter(value == 0) %>%
              group_by(ID) %>%
              summarise(n0 = n()), by = "ID")%>%
  filter(n_datapoints == n0) %>%   #Select the populations where the number of Nulls == number of data points
  pull(ID)   #create a vector of IDs corresponding to populations with only zeros

## clean data--remove time series with only zeroes or nulls
cad <- cad %>% 
  filter(!ID %in% zeroids) %>% # remove populations that are only zeros
  filter(!ID %in% nullids)  # remove populations that are only NULL

# save (original) data with zeros 
cad_z <- cad 

# change all zeros to NA in population abundance columns (ie. X1970-X2022 values)
cad <- cad %>% 
  mutate(across(X1970:X2022, ~ case_when(. == 0 ~ NA, TRUE ~ .)))


# join binomial spp name to pop_lambdas
cad.names <- cad %>% 
  dplyr::select(ID, Binomial) %>% 
  rename(population_id = ID)


## load unweighted LPI output 
u_cad <- read.csv(here("01_outdata", "credible_intervals", "CLPI_CIs-by-year.csv"), header=TRUE) %>% 
  column_to_rownames(var="X")

## population-level lambdas
pop_lambdas <- read.csv(here("01_outdata", "credible_intervals", "unweighted_pops_PopLambda.txt"), header=TRUE)
pop_lambdas <- left_join(pop_lambdas, cad.names) # add spp name to popn lambda file

## species-level lambdas
spp_lambdas <- read.csv(here("01_outdata", "credible_intervals", "unweighted_pops_lambda.csv"), header=TRUE) 



#### 1.1: run unweighted C-LPI ----

# NOTE: This code is commented out after running the first time. Only re-run if necessary to make modifications, otherwise use
# LPI outputs (saved & loaded in 0.2).
# 
# # create infile
# infile_u <- create_infile(cad,
#                           start_col_name = "X1970",  # data start year
#                           end_col_name = "X2022",    # data end year
#                           CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
#                           name = "01_outdata/credible_intervals/unweighted") # name the infile/outputs
# 
# 
# # run LPIMain
# set.seed(2383)
# u_cad <- LPIMain(infile_u,
#                  REF_YEAR = 1970,
#                  PLOT_MAX=2022,
#                  LINEAR_MODEL_SHORT_FLAG = 1,
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
# 
# # save file
# write.csv(u_cad,file.path("01_outdata","credible_intervals", "CLPI_CIs-by-year.csv"))



#### 1.2: run weighted C-LPI ----

## NOTE: this code is commented out to prevent accidentally re-running! it is VERY COMPUTATIONALLY INTENSIVE (>100 hours)!

# this analysis is run in 2 steps:
## i. to generate species CIs, sample species with replacement & run the weighted analysis 10,000 times. the CIs for each year represent the 0.025 & 0.975 quartiles.
## ii. to generate the trendline, use the full dataset and take the final index value per year. do not use the CIs generated by this process--these are bootstrapped by year.

##################### i. run 10,000 bootstraps of weighted analysis to get CIs by species 
# 
# # <><><><><> step 1: read in data and tidy <><><><><>
# cad <- read.csv(file=here("00_data", "CAD_Paper_withzeroes.csv"), na.strings="NULL", header=TRUE)  %>%
#   mutate(X2009 = as.character(X2009)) %>%             # set from numeric to character so following line works
#   mutate(across(X1970:X2022, ~na_if(., "Null"))) %>%  # replace any "Null" to NA 
#   rename(original_id = ID) %>%      # set "ID" to original_id, and make a new ID corresponding to rownumber to avoid issues
#   mutate(ID = row_number()) %>% 
#   rename(Taxa = Taxonomic_group) %>%                      # rename this column to Taxa
#   mutate(Taxa = case_when(Taxa=="Mammalia" ~ "Mammals",   # rename these redundant labels
#                           Taxa=="Reptilia" ~ "Herps", 
#                           TRUE ~ Taxa))
# 
# # Data to exclude: Finding the time series that are only NA
# nullids <- cad %>%
#   pivot_longer(X1970:X2022, names_to = "Year") %>%   # Changing the data frame to long format; i.e. all values in X1950:X2020 will go into a column "value", and all column names from X1950 to X2020 will go into a column named "Year".
#   group_by(ID) %>%  # group_by to ensure the summarise function seperates groups
#   summarise(n_datapoints = n()) %>% # count the number of rows
#   left_join(., cad %>%  # add another dataset with matching ID column
#               pivot_longer(X1970:X2022, names_to = "Year") %>%  #long form
#               filter(is.na(value)) %>%  #keep only values that are NA
#               group_by(ID) %>%
#               summarise(n_null = n()), by = "ID") %>% # Count number of rows (which are only NA in this data set)
#   filter(n_datapoints == n_null) %>%  #Select the populations where the number of Nulls == number of data points
#   pull(ID)  # create a vector with population ids that are only NA
# 
# # Data to exclude: Finding the time series that only have zero values
# zeroids <- cad %>%
#   pivot_longer(X1970:X2022, names_to = "Year") %>%
#   filter(!is.na(value)) %>%   # Remove values that are null
#   group_by(ID) %>%
#   summarise(n_datapoints = n()) %>%
#   left_join(., cad %>%
#               pivot_longer(X1970:X2022, names_to = "Year") %>%
#               filter(!is.na(value)) %>% # Remove values that are null
#               filter(value == 0) %>%
#               group_by(ID) %>%
#               summarise(n0 = n()), by = "ID")%>%
#   filter(n_datapoints == n0) %>%   #Select the populations where the number of Nulls == number of data points
#   pull(ID)   #create a vector of IDs corresponding to populations with only zeros
# 
# # clean data--remove time series with only zeroes or nulls
# nrow(cad) # 4493
# cad <- cad %>% 
#   filter(!ID %in% zeroids) %>% # remove populations that are only zeros
#   filter(!ID %in% nullids)  # remove populations that are only NULL
# nrow(cad) # 4490--check--should only have removed 3 popns
# 
# # change all zeros to NA in population abundance columns (ie. X1970-X2022 values)
# cad <- cad %>% 
#   mutate(case_when(if_any(X1970:X2022, ~. ==0) ~ NA, 
#                    TRUE ~ .))  # set all zeros to NA
# nrow(cad) # 4490--check
# 
# # join binomial spp name to pop_lambdas
# cad.names <- cad %>% 
#   dplyr::select(ID, Binomial) %>% 
#   rename(population_id = ID)
# 
# 
# # <><><><><> step 2: run bootstrap as a for loop <><><><><> # 
# 
# # ran this in chunks of 1,000 iterations & stitched each of the 10 chunks together
# # 1,000 itrs takes about 11 hours
# 
# sp = unique(cad$Binomial)
# boot_indices = list()
# N_BOOT = 1000
# 
# for(i in 1:N_BOOT) {
#   tryCatch({
#     print(sprintf("Processing sample %d", i))
#     
#     # Sample speceis names from the species list with replacement
#     sampled_species <- sample(sp, length(sp), replace = TRUE)
#     
#     # Count how many of each species we've sampled
#     sampled_counts <- tibble(Binomial = sampled_species) %>%
#       count(Binomial, name = "count")
#     
#     # Using this count to get the data for each species from the data frame
#     # The right number of times
#     # But also rename pop IDs appropriately
#     sampled_data <- cad %>%
#       inner_join(., sampled_counts, by = "Binomial") %>%   # Filter for species in sample_counts list
#       group_by(Binomial) %>%                            # And for each species
#       slice(rep(row_number(), times = count)) %>%       # Get each species data 'count' times
#       mutate(ID = paste0(ID, "_", row_number())) %>%    # Added to make 'new' duplicated populations unique
#       ungroup() %>%
#       select(-count)                                    # Remove the count column
#     
#     ###
#     ## subset original LPI data into each weightings group
#     ###
#     # subset by taxa
#     Taxa = sampled_data$Taxa
#     Aves <- Taxa == 'Aves'
#     Mammalia <- Taxa == 'Mammals'
#     Fishes <- Taxa == 'Fish'
#     Herps <- Taxa == 'Herps'
#     
#     ###
#     ## create infiles for each system & taxa subset
#     # RF: I've needed to append Sys.getpid() to the infile name to stop the parallel processes from trying to access the same files
#     ###
#     aves_index <- create_infile(sampled_data, index_vector=Aves, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted", "aves"))
#     mammalia_index <- create_infile(sampled_data, index_vector=Mammalia, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","mammalia"))
#     herps_index <- create_infile(sampled_data, index_vector=Herps, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","herps"))
#     fishes_index <- create_infile(sampled_data, index_vector=Fishes, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","fishes"))
#     
#     
#     
#     note: if BOOT_STRAP_SIZE is 1 and CI_FLAG is true, we get CI2 errors
# 
#     # this input works: "./01_outdata/weighted/global-weightings-taxa-local.txt"
#     # this DOESN'T work: "~/01_outdata/weighted/global-weightings-taxa-local.txt"
#     # this DOESN'T work: here("01_outdata", "weighted", "global-weightings-taxa-local.txt")
#     w_boot_lpi <- LPIMain("./01_outdata/weighted/global-weightings-taxa-local.txt",
#                           BOOT_STRAP_SIZE = 1,  # we only want to sample once per spp pool subset--the real resampling occurs via for loop
#                           use_weightings = 1,              # to generate weighted LPI
#                           #use_weightings_B = 0,
#                           LINEAR_MODEL_SHORT_FLAG = 1,     # flag for CAD process
#                           DATA_LENGTH_MIN = 3,             # include only time-series with >2 data points
#                           VERBOSE=TRUE,
#                           PLOT_MAX = 2022,
#                           SHOW_PROGRESS = FALSE,
#                           CI_FLAG = FALSE,     # don't calculate CIs bootstrapped by year
#                           save_plots = 0,      # don't save plots
#                           plot_lpi = 0,        # don't plot LPI trend
#                           basedir = ".", 
#                           force_recalculation = 1) 
#     
#     # save run to list
#     boot_indices[[i]] = w_boot_lpi
#     
#   }, error = function(e) {
#     print("Error while processing sample")
#     print(e)
#     boot_indices[[i]] = NULL
#   })
# }
# 
# # <><><><><> step 3: save outputs <><><><><> # 
# 
# # for each itr chunk, save the output (make sure to rename w_boot_1 depending on itr chunk)
# # itr 1-1000
# boot_indices # check
# w_boot_1 <- do.call(cbind, boot_indices) # bind so rows are year & cols are itr
# colnames(w_boot_1) <- paste0("itr_", 1:1000) # name by boot iteration
# write.csv(w_boot_1, file=here("01_outdata/w_boot_1.csv")) # save somewhere



################## ii. run weighted analysis to get trendline 
# this creates the trendline (CIs generated by separate process of species boostrapping above)
# 
# Taxa = cad$Taxa
# Aves <- Taxa == 'Aves'
# Mammalia <- Taxa == 'Mammals'
# Fishes <- Taxa == 'Fish'
# Herps <- Taxa == 'Herps'
# 
# ## create infiles for each system & taxa subset
# 
# aves_index <- create_infile(cad, index_vector=Aves, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted", "aves"))
# mammalia_index <- create_infile(cad, index_vector=Mammalia, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","mammalia"))
# herps_index <- create_infile(cad, index_vector=Herps, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","herps"))
# fishes_index <- create_infile(cad, index_vector=Fishes, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","fishes"))
# 
# w_boot_lpi <- LPIMain("./01_outdata/weighted/global-weightings-taxa-local.txt",
#                       BOOT_STRAP_SIZE = 10,  # doesn't really matter what number i give it, since we want trendline not bootstrapped CIs by year
#                       use_weightings = 1,              # to generate weighted LPI
#                       #use_weightings_B = 0,
#                       LINEAR_MODEL_SHORT_FLAG = 1,     # flag for CAD process
#                       DATA_LENGTH_MIN = 3,             # include only time-series with >2 data points
#                       VERBOSE=TRUE,
#                       PLOT_MAX = 2022,
#                       SHOW_PROGRESS = FALSE,
#                       CI_FLAG = TRUE,      # to get bootstrapped CIs by year
#                       save_plots = 0,      # don't save plots
#                       plot_lpi = 1,        # don't plot LPI trend
#                       basedir = ".", 
#                       force_recalculation = 1)
# 
# w_boot_lpi <- w_boot_lpi[-54,] # remove 2023

################## iii. tidying: put trendline & CIs in same dataframe
# put together each of the itr chunks
w_dat <- cbind(w_boot_1, w_boot_2, w_boot_3, w_boot_4, w_boot_5, w_boot_6, w_boot_7, w_boot_8, w_boot_9, w_boot_10) %>% 
  as.data.frame() %>%
  rownames_to_column(var="year") %>% # name rownames as year
  pivot_longer(itr_1:itr_10000, names_to = "itr", values_to = "index")  # put in long format

# calculate CIs per year 
intervals_boot_indices_nested <- w_dat |> nest_by(year) |>  
  mutate(quantiles = list(
    quantile(data$index, probs = c(0.025, 0.975)) |> setNames(c("Lower_CI","Upper_CI")) |> bind_rows()), # get quantiles and set names
    mean_lpi = list(mean(data$index))) # measure mean within years

# tidying 
intervals_boot_indices_df <- intervals_boot_indices_nested |> 
  unnest(c(quantiles,mean_lpi)) |> select(-data) |> data.frame() |> ungroup()

# join CI df to trendline df 
w_dat2 <- cbind(intervals_boot_indices_df[-54,-4], w_boot_lpi) %>%   # remove 2023 and mean_lpi from the CI df
  dplyr::select(-c(CI_low, CI_high)) %>%  # remove CIs by year
  rename(CI_high = Upper_CI, # rename CIs by spp so it plots properly
         CI_low = Lower_CI) %>% 
  mutate(year = as.numeric(year)) %>%  # make this var numeric
  column_to_rownames(var="year") %>%  # and label rownames as year
  as.data.frame()

# save weighted info
write.csv(w_dat2, file=here("01_outdata/weighted/weighted_lpi_sppCI_data.csv"))


#### 2.1: calculate confidence intervals (3 methods) ----

# The purpose of this script is to run three methods of bootstrapping confidence intervals on the canadian LPI subset. Bootstrapping occurs on 
# unweighted data by population (ie. bootstrapping of population-level lambda values by row), species (ie. bootstrapping of species-level lambda 
# values by row), and within years (ie. bootstrapping of species-level lambda values for a given year). LPIs are resampled *with replacement* 
# 10,000 times on each unit (population, species, year) in the dataset.

# By population:
set.seed(2941)
boot_pop <- bootstrap_by_rows(pop_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_pop_CI <- as.data.frame(boot_pop$interval_data) # save CI intervals in a separate object
boot_pop_CI$year <- as.numeric(boot_pop_CI$year) # change year from character to numeric

boot_pop_CI <- boot_pop_CI %>% 
  dplyr::select(-mean_lpi)  # remove this col so not confused with index value

# save data to csv--only needs to be saved the first time this code is run (or after revisions)!
write.csv(boot_pop_CI, file.path("01_outdata", "credible_intervals", "CLPI_CIs-by-population.csv"), row.names = FALSE)

# what was CI range in 2022? 
boot_pop_CI %>% 
  filter(year==2022)
# range = Upper_CI - Lower_CI = 1.126707 - 0.8962958 = 0.2304112

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
set.seed(2903)
boot_spp <- bootstrap_by_rows(spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
boot_spp_CI <- as.data.frame(boot_spp$interval_data) # save CI intervals in a separate object
boot_spp_CI$year <- as.numeric(boot_spp_CI$year ) # change year from character to numeric

boot_spp_CI <- boot_spp_CI %>% 
  dplyr::select(-mean_lpi)  # remove this col so not confused with index value

# save data to csv--only needs to be saved the first time this code is run (or after revisions)!
write.csv(boot_spp_CI, file.path("01_outdata", "credible_intervals", "CLPI_CIs-by-species.csv"), row.names = FALSE)

# what was CI range in 2022? 
boot_spp_CI %>% 
  filter(year==2022)
# range = Upper_CI - Lower_CI = 1.140173 - 0.8977526 = 0.2424204

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
# range = CI_high - CI_low = 1.077115 - 0.9506417 = 0.1264733
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

data_folder_path <- "./01_outdata/credible_intervals"

# create method labels
bootstrap_method_label <- c("year bootstrap",
                            "species bootstrap",
                            "population bootstrap")

# assign column labels
col_labs <- list(c("year","CI_low","CI_high"),
                 c("year","Lower_CI","Upper_CI"),
                 c("year","Lower_CI","Upper_CI"))

# assign paths for each method file
bootstrap_method_output_path <- c(file.path(data_folder_path,"CLPI_CIs-by-year.csv"),
                                  file.path(data_folder_path,"CLPI_CIs-by-population.csv"),
                                  file.path(data_folder_path,"CLPI_CIs-by-species.csv"))

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


#### 3.1: compare shifting baseline years ----
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
set.seed(1048)
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
set.seed(12093)
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

# save for shiny plotting
write.csv(baselines_linear_df, file=here("01_outdata", "baseline_years", "CLPI_baseline-years_sppCIs.csv"), row.names = FALSE)

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
        legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10)); baselines_linear_plot


#### 3.2: compare avg rates of change per baseline year ----

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

#### 3.3: compare avg lambda values per baseline year ----
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

#### 3.4: summary statistics per baseline year ----
## how many population time series & species are included in each baseline calculation?
# empty output object
columns <-  c("year","n_pops","n_spp") 
baseline_counts_df <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(baseline_counts_df) <-  columns

# count number of popn time series & species without exclusively NAs for each baseline 
# NOTE: reference year is not included in determining whether there are only NAs, since reference year may be set to 1 even if there is no data past the reference year.
for(i in 1:length(years)){
  
  # count number of population time series
  n_pops <- cad %>% 
    dplyr::select(Binomial, paste0("X", years[i]+1):X2022) %>%    # select ref_year+1 to 2022
    mutate(non_na_count = rowSums(across(starts_with("X"), ~ !is.na(.)))) %>%    # count the number of non-NA values per time series
    filter(non_na_count>2) %>%   # filter for time series that have more than 2 non-NA values (only time series with ≥3 data points included as per C-LPI inclusion criteria)
    nrow() # count number of rows aka time series 
  
  # count number of species 
  n_spp1 <- cad %>% 
    dplyr::select(Binomial, paste0("X", years[i]+1):X2022) %>%  # select ref_year+1 to 2022
    mutate(non_na_count = rowSums(across(starts_with("X"), ~ !is.na(.)))) %>%   # count the number of non-NA values per time series
    filter(non_na_count>2) %>%    # filter for time series that have more than 2 non-NA values (only time series with ≥3 data points included as per C-LPI inclusion criteria)
    dplyr::select(Binomial) # select the species col
  
  n_spp2 <- length(unique(n_spp1$Binomial)) # get the number of unique species names 
  
  # write results to output df 
  baseline_counts_df[i,1] <- years[i]
  baseline_counts_df[i,2] <- n_pops
  baseline_counts_df[i,3] <- n_spp2
  
}

# inspect output
baseline_counts_df

## what were final index values?
baselines_linear_df %>%  
  filter(Year==2022)

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
                               name = "./01_outdata/modelling/loglin") # name the infile/outputs

# run LPI 
set.seed(20485)
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
set.seed(8204)
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

# save for shiny
write.csv(boot_spp_loglin_df, file=here("01_outdata", "modelling", "short-loglinear-sppCIs.csv"))

# what is final (2022) LPI value for this method?
boot_spp_loglin_df[53,] 
#### end of section where short time series = log-linear

# short time series modelled as linear & long time series as GAM:
# create infile 
infile_lin <- create_infile(cad,
                            start_col_name = "X1970",  # data start year
                            end_col_name = "X2022",    # data end year
                            CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
                            name = "./01_outdata/modelling/lin") # name the infile/outputs

# run LPI 
set.seed(9284)
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
set.seed(10948)
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

# save for shiny
write.csv(boot_spp_lin_df, file=here("01_outdata", "modelling", "short-linear-sppCIs.csv"))

# what is final (2022) LPI value for this method?
boot_spp_lin_df[53,] 
#### end of section where short time series = linear

# all series, regardless of length, are modelled as GAM:
# create infile 
infile_gam <- create_infile(cad,
                            start_col_name = "X1970",  # data start year
                            end_col_name = "X2022",    # data end year
                            CUT_OFF_YEAR = 1970,       # filters all data out existing before this year
                            name = "./01_outdata/modelling/gam") # name the infile/outputs

# run LPI where all time series (regardless of length) are modelled as GAM
set.seed(9284)
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
spp_lambdas_gam <- read.csv(here("01_outdata", "modelling", "gam_pops_lambda.csv"), header=TRUE) 

# bootstrap by species to get CIs for lpi_lm_false
set.seed(928457)
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

# save for shiny
write.csv(boot_spp_gam_df, file=here("01_outdata", "modelling", "all-gam-sppCIs.csv"))

# what is final (2022) LPI value for this method?
boot_spp_gam_df[53,] 
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

length(which(pt_counts$num.datapoints<6)) # 1719 time series have less than 6 data points
length(which(pt_counts$num.datapoints>=6)) # 2771 time series have 6 or more data points
#


#### 5.1: process data for length/fullness analysis ----
# First calculate number of data points, period, and completeness for each time series. 
#calculate number of data points (number of years for each time series that have population abundances)
cad2 <- cad %>% 
  mutate(num.datapoints = rowSums(!is.na(select(., X1970:X2022)))) 

# For time series with ≥15 points: 
greaterthan15points.lpidata <- subset(cad2, num.datapoints >= 15)
nrow(greaterthan15points.lpidata) # 1324

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
nrow(greaterthan6points.lpidata) # 2771

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
nrow(greaterthan3points.lpidata) # 3664

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
nrow(greaterthan2points.lpidata) # 4373

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

#### 5.2: impact of number of data points ----
# Decision 1: How does the number of data points (times in which population abundance was assessed) impact the LPI? Options: at least 2, 
# at least 3 (to match what WWF-Canada currently does), at least 6 (to include only the populations modelled via GAMs), and at least 15 
# (to see what a much larger number does). 

# Calculate the LPI for each category, i.e. time series with >=2, >=3, >=6, >=15 datapoints:
## calculate LPI for time series with at least 15 datapoints
set.seed(18475)
subset_lpi_15 <- LPIMain(create_infile(greaterthan15points.lpidata,
                                       name="01_outdata/number_data_points/15pts+_data",
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

# bootstrap CIs by species lambdas
subset_15pts_spp_lambdas <- read.csv(here("01_outdata", "number_data_points", "15pts+_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(92847)
subset_15pts_boot <- bootstrap_by_rows(subset_lpi15_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
subset_15pts_boot_CI <- as.data.frame(subset_15pts_boot$interval_data) # save CI intervals in a separate object
subset_15pts_boot_CI$year <- as.numeric(subset_15pts_boot_CI$year) # change year from character to numeric

# tidy
subset_15pts_boot_df <- subset_15pts_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., subset_lpi_15 %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# save for shiny
write.csv(subset_15pts_boot_df, file=here("01_outdata", "number_data_points", "15pts+_CIspp.csv"))

# plot
ggplot_lpi(subset_15pts_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
subset_15pts_boot_df[53,] # final index in 2022 = 1.039023
nrow(greaterthan15points.lpidata) # 1324 popns included in this subset
length(unique(greaterthan15points.lpidata$Binomial)) # 593 species included in this subset

## calculate LPI for time series with at least 6 datapoints
set.seed(92747)
subset_lpi_6 <- LPIMain(create_infile(greaterthan6points.lpidata,
                                      name="01_outdata/number_data_points/6pts+_data",
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


# bootstrap CIs by species lambdas
subset_6pts_spp_lambdas <- read.csv(here("01_outdata", "number_data_points", "6pts+_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(25743)
subset_6pts_boot <- bootstrap_by_rows(subset_6pts_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
subset_6pts_boot_CI <- as.data.frame(subset_6pts_boot$interval_data) # save CI intervals in a separate object
subset_6pts_boot_CI$year <- as.numeric(subset_6pts_boot_CI$year) # change year from character to numeric

# tidy
subset_6pts_boot_df <- subset_6pts_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., subset_lpi_6 %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# save for shiny
write.csv(subset_6pts_boot_df, file=here("01_outdata", "number_data_points", "6pts+_CIspp.csv"))

# plot
ggplot_lpi(subset_6pts_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
subset_6pts_boot_df[53,] # final index in 2022 = 1.143245
nrow(greaterthan6points.lpidata) # 2771 popns included in this subset
length(unique(greaterthan6points.lpidata$Binomial)) # 787 species included in this subset


## calculate LPI for time series with at least 3 datapoints
set.seed(49502)
subset_lpi_3 <- LPIMain(create_infile(greaterthan3points.lpidata,
                                      name="01_outdata/number_data_points/3pts+_data",
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

# bootstrap CIs by species lambdas
subset_3pts_spp_lambdas <- read.csv(here("01_outdata", "number_data_points", "3pts+_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(84902)
subset_3pts_boot <- bootstrap_by_rows(subset_3pts_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
subset_3pts_boot_CI <- as.data.frame(subset_3pts_boot$interval_data) # save CI intervals in a separate object
subset_3pts_boot_CI$year <- as.numeric(subset_3pts_boot_CI$year) # change year from character to numeric

# tidy
subset_3pts_boot_df <- subset_3pts_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., subset_lpi_3 %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# save for shiny
write.csv(subset_3pts_boot_df, file=here("01_outdata", "number_data_points", "3pts+_CIspp.csv"))

# plot
ggplot_lpi(subset_3pts_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
subset_3pts_boot_df[53,] # final index in 2022 = 1.01151
nrow(greaterthan3points.lpidata) # 3664 popns included in this subset
length(unique(greaterthan3points.lpidata$Binomial)) # 889 species included in this subset

## calculate LPI for time series with at least 2 datapoints
set.seed(28475)
subset_lpi_2 <- LPIMain(create_infile(greaterthan2points.lpidata,
                                      name="01_outdata/number_data_points/2pts+_data",
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

# bootstrap CIs by species lambdas
subset_2pts_spp_lambdas <- read.csv(here("01_outdata", "number_data_points", "2pts+_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(294785)
subset_2pts_boot <- bootstrap_by_rows(subset_2pts_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
subset_2pts_boot_CI <- as.data.frame(subset_2pts_boot$interval_data) # save CI intervals in a separate object
subset_2pts_boot_CI$year <- as.numeric(subset_2pts_boot_CI$year) # change year from character to numeric

# tidy
subset_2pts_boot_df <- subset_2pts_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., subset_lpi_2 %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# save for shiny
write.csv(subset_2pts_boot_df, file=here("01_outdata", "number_data_points", "2pts+_CIspp.csv"))

# plot
ggplot_lpi(subset_2pts_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
subset_2pts_boot_df[53,] # final index in 2022 = 0.9447457
nrow(greaterthan2points.lpidata) # 4373 popns included in this subset
length(unique(greaterthan2points.lpidata$Binomial)) # 931 species included in this subset


## Plot all four scenarios (>=15, >=6, >=3, or >=2 datapoints) together:
# order names so it plots in logical order
namesvec <- c("≥2 points", "≥3 points", "≥6 points", "≥15 points")
namesvec <- factor(namesvec, levels=c("≥2 points", "≥3 points", "≥6 points", "≥15 points"))

# code plot
num_datapts_plot <- ggplot_multi_lpi(list(subset_2pts_boot_df, subset_3pts_boot_df, subset_6pts_boot_df, subset_15pts_boot_df), 
                                     names = namesvec, 
                                     facet=TRUE) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(size=12));num_datapts_plot


#### 5.3: impact of completeness of time series ----
## subset data
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
nrow(greaterthan75complete.2pts) # 2326
nrow(greaterthan50complete.2pts) # 3226
nrow(greaterthan25complete.2pts) # 4175
nrow(greaterthan0complete.2pts) # 4373

## run LPI for each subset
# function automating LPIMain & bootstrapping species CIs based on completeness & number of data points:
automate_completeness <- function(indata=indata, complete_num=complete_num, num_pts=num_pts){
  
  # calculate LPIMain
  lpi_out <- LPIMain(create_infile(indata,
                                   name=paste0("01_outdata/completeness/", complete_num, "complete_", num_pts, "pts"),
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
  
  # create CIs by bootstrapping species lambdas
  lambda_file <- paste0(complete_num, "complete_", num_pts, "pts_pops_lambda.csv") # specify lambda file to read in 
  boot_spp_lambdas <- read.csv(here("01_outdata", "completeness", lambda_file)) # read in spp lambdas
  boot_out <- bootstrap_by_rows(boot_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000) # run bootstrap
  boot_out_CI <- as.data.frame(boot_out$interval_data) # save CI intervals in a separate object
  boot_out_CI$year <- as.numeric(boot_out_CI$year) # change year from character to numeric
  
  # tidy
  boot_out_df <- boot_out_CI %>% 
    dplyr::select(year, Upper_CI, Lower_CI) %>% 
    rename(CI_high = Upper_CI,
           CI_low = Lower_CI) %>% 
    left_join(., lpi_out %>% 
                rownames_to_column(var="year") %>% 
                mutate(year = as.numeric(year)) %>% 
                dplyr::select(year, LPI_final), 
              by="year") %>% 
    column_to_rownames(var="year")
  
  # plot
  out_plot <- ggplot_lpi(boot_out_df, col="#c1e6db", line_col = "#66C2A5")
  
  return(list(boot_out_df = boot_out_df, out_plot = out_plot))  # specify outputs to return
}
 
# apply function to all completeness & num data point subsets:
set.seed(57383)
complete75_2pts_out <- automate_completeness(indata=greaterthan75complete.2pts, complete_num=75, num_pts=2) # complete=75% & ≥2 data points
complete75_2pts_out$out_plot # inspect output
complete75_2pts_out[[1]]

set.seed(299438)
complete75_3pts_out <- automate_completeness(indata=greaterthan75complete.3pts, complete_num=75, num_pts=3) # complete=75% & ≥3 data points
complete75_3pts_out$out_plot # inspect output

set.seed(859329)
complete75_6pts_out <- automate_completeness(indata=greaterthan75complete.6pts, complete_num=75, num_pts=6) # complete=75% & ≥6 data points
complete75_6pts_out$out_plot # inspect output

set.seed(27483)
complete75_15pts_out <- automate_completeness(indata=greaterthan75complete.15pts, complete_num=75, num_pts=15) # complete=75% & ≥15 data points
complete75_15pts_out$out_plot # inspect output

set.seed(57483)
complete50_2pts_out <- automate_completeness(indata=greaterthan50complete.2pts, complete_num=50, num_pts=2) # complete=50% & ≥2 data points
complete50_2pts_out$out_plot # inspect output

set.seed(39475)
complete50_3pts_out <- automate_completeness(indata=greaterthan50complete.3pts, complete_num=50, num_pts=3) # complete=50% & ≥3 data points
complete50_3pts_out$out_plot # inspect output

set.seed(82943)
complete50_6pts_out <- automate_completeness(indata=greaterthan50complete.6pts, complete_num=50, num_pts=6) # complete=50% & ≥6 data points
complete50_6pts_out$out_plot # inspect output

set.seed(87392)
complete50_15pts_out <- automate_completeness(indata=greaterthan50complete.15pts, complete_num=50, num_pts=15) # complete=50% & ≥15 data points
complete50_15pts_out$out_plot # inspect output

set.seed(458373)
complete25_2pts_out <- automate_completeness(indata=greaterthan25complete.2pts, complete_num=25, num_pts=2) # complete=25% & ≥2 data points
complete25_2pts_out$out_plot # inspect output

set.seed(72829)
complete25_3pts_out <- automate_completeness(indata=greaterthan25complete.3pts, complete_num=25, num_pts=3) # complete=25% & ≥3 data points
complete25_3pts_out$out_plot # inspect output

set.seed(484393)
complete25_6pts_out <- automate_completeness(indata=greaterthan25complete.6pts, complete_num=25, num_pts=6) # complete=25% & ≥6 data points
complete25_6pts_out$out_plot # inspect output

set.seed(72849)
complete25_15pts_out <- automate_completeness(indata=greaterthan25complete.15pts, complete_num=25, num_pts=15) # complete=25% & ≥15 data points
complete25_15pts_out$out_plot # inspect output

set.seed(74393)
complete0_2pts_out <- automate_completeness(indata=greaterthan0complete.2pts, complete_num=0, num_pts=2) # complete=0% & ≥2 data points
complete0_2pts_out$out_plot # inspect output

set.seed(573892)
complete0_3pts_out <- automate_completeness(indata=greaterthan0complete.3pts, complete_num=0, num_pts=3) # complete=0% & ≥3 data points
complete0_3pts_out$out_plot # inspect output

set.seed(39583)
complete0_6pts_out <- automate_completeness(indata=greaterthan0complete.6pts, complete_num=0, num_pts=6) # complete=0% & ≥6 data points
complete0_6pts_out$out_plot # inspect output

set.seed(82982)
complete0_15pts_out <- automate_completeness(indata=greaterthan0complete.15pts, complete_num=0, num_pts=15) # complete=0% & ≥15 data points
complete0_15pts_out$out_plot # inspect output

## compare final (2022) LPI values for each permutation
complete75_2pts_out$boot_out_df[53,3] 
complete75_3pts_out$boot_out_df[53,3] 
complete75_6pts_out$boot_out_df[53,3] 
complete75_15pts_out$boot_out_df[53,3] 
complete50_2pts_out$boot_out_df[53,3] 
complete50_3pts_out$boot_out_df[53,3] 
complete50_6pts_out$boot_out_df[53,3] 
complete50_15pts_out$boot_out_df[53,3] 
complete25_2pts_out$boot_out_df[53,3] 
complete25_3pts_out$boot_out_df[53,3] 
complete25_6pts_out$boot_out_df[53,3] 
complete25_15pts_out$boot_out_df[53,3] 
complete0_2pts_out$boot_out_df[53,3] 
complete0_3pts_out$boot_out_df[53,3] 
complete0_6pts_out$boot_out_df[53,3] 
complete0_15pts_out$boot_out_df[53,3] 

# save data for shiny
complete75_2pts_df <- complete75_2pts_out$boot_out_df %>% mutate(n_points=2)
complete75_3pts_df <- complete75_3pts_out$boot_out_df %>% mutate(n_points=3)
complete75_6pts_df <- complete75_6pts_out$boot_out_df %>% mutate(n_points=6)
complete75_15pts_df <- complete75_15pts_out$boot_out_df %>% mutate(n_points=15)
complete75_df <- rbind(complete75_2pts_df, complete75_3pts_df, complete75_6pts_df, complete75_15pts_df)
write.csv(complete75_df, file=here("01_outdata", "completeness", "complete75_CIspp.csv"))

complete50_2pts_df <- complete50_2pts_out$boot_out_df %>% mutate(n_points=2)
complete50_3pts_df <- complete50_3pts_out$boot_out_df %>% mutate(n_points=3)
complete50_6pts_df <- complete50_6pts_out$boot_out_df %>% mutate(n_points=6)
complete50_15pts_df <- complete50_15pts_out$boot_out_df %>% mutate(n_points=15)
complete50_df <- rbind(complete50_2pts_df, complete50_3pts_df, complete50_6pts_df, complete50_15pts_df)
head(complete50_df) # check
tail(complete50_df) # check
write.csv(complete50_df, file=here("01_outdata", "completeness", "complete50_CIspp.csv"))

complete25_2pts_df <- complete25_2pts_out$boot_out_df %>% mutate(n_points=2)
complete25_3pts_df <- complete25_3pts_out$boot_out_df %>% mutate(n_points=3)
complete25_6pts_df <- complete25_6pts_out$boot_out_df %>% mutate(n_points=6)
complete25_15pts_df <- complete25_15pts_out$boot_out_df %>% mutate(n_points=15)
complete25_df <- rbind(complete25_2pts_df, complete25_3pts_df, complete25_6pts_df, complete25_15pts_df)
head(complete25_df) # check
tail(complete25_df) # check
write.csv(complete25_df, file=here("01_outdata", "completeness", "complete25_CIspp.csv"))

complete0_2pts_df <- complete0_2pts_out$boot_out_df %>% mutate(n_points=2)
complete0_3pts_df <- complete0_3pts_out$boot_out_df %>% mutate(n_points=3)
complete0_6pts_df <- complete0_6pts_out$boot_out_df %>% mutate(n_points=6)
complete0_15pts_df <- complete0_15pts_out$boot_out_df %>% mutate(n_points=15)
complete0_df <- rbind(complete0_2pts_df, complete0_3pts_df, complete0_6pts_df, complete0_15pts_df)
head(complete0_df) # check
tail(complete0_df) # check
write.csv(complete0_df, file=here("01_outdata", "completeness", "complete0_CIspp.csv"))


## Plot all scenarios
# set the naming vector as factor so labels are logically ordered
names_vec <- c("≥2 points", "≥3 points", "≥6 points", "≥15 points")
names_vec <- factor(names_vec, levels=c("≥2 points", "≥3 points", "≥6 points", "≥15 points"))

completeness0_plot <- ggplot_multi_lpi(list(complete0_2pts_out$boot_out_df, complete0_3pts_out$boot_out_df, complete0_6pts_out$boot_out_df, complete0_15pts_out$boot_out_df),
                                       names = names_vec, 
                                       facet=FALSE, 
                                       col="Spectral",
                                       ylims = c(0.7, 1.3)) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank()); completeness0_plot

completeness25_plot <- ggplot_multi_lpi(list(complete25_2pts_out$boot_out_df, complete25_3pts_out$boot_out_df, complete25_6pts_out$boot_out_df, complete25_15pts_out$boot_out_df),
                                        names = names_vec, 
                                        facet=FALSE, 
                                        col="Spectral",
                                        ylims = c(0.7, 1.3)) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank()); completeness25_plot

completeness50_plot <- ggplot_multi_lpi(list(complete50_2pts_out$boot_out_df, complete50_3pts_out$boot_out_df, complete50_6pts_out$boot_out_df, complete50_15pts_out$boot_out_df),
                                        names = names_vec, 
                                        facet=FALSE,
                                        col="Spectral",
                                        ylims = c(0.7, 1.3)) +
  #guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank()); completeness50_plot

completeness75_plot <- ggplot_multi_lpi(list(complete75_2pts_out$boot_out_df, complete75_3pts_out$boot_out_df, complete75_6pts_out$boot_out_df, complete75_15pts_out$boot_out_df),
                                      names = names_vec, 
                                      facet=FALSE, 
                                      col="Spectral",
                                      ylims = c(0.7, 1.3)) +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank()); completeness75_plot

#### 5.4: impact of time series length ----
# subset data by period
greaterthan20period.lpidata <- subset(greaterthan2points.lpidata, period >= 20)
greaterthan15period.lpidata <- subset(greaterthan2points.lpidata, period >= 15)
greaterthan10period.lpidata <- subset(greaterthan2points.lpidata, period >= 10)
greaterthan5period.lpidata <- subset(greaterthan2points.lpidata, period >= 5)

# how many time series are included in each subset?
nrow(greaterthan20period.lpidata) # 1366
nrow(greaterthan15period.lpidata) # 2017
nrow(greaterthan10period.lpidata) # 2694
nrow(greaterthan5period.lpidata) # 3370

## calculate LPI for time series with ≥20 year period
set.seed(89348)
period20.lpi <- LPIMain(create_infile(greaterthan20period.lpidata,
                                     name="01_outdata/period/>20period_data",
                                     start_col_name = "X1970",
                                     end_col_name = "X2022",
                                     CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
PLOT_MAX = 2022,
BOOT_STRAP_SIZE = 10000,
# DATA_LENGTH_MIN = 3,     # commented out, since we've already done this manually
VERBOSE=FALSE, 
LINEAR_MODEL_SHORT_FLAG = 1,
force_recalculation = 1
)

# bootstrap CIs by species lambdas
period20_spp_lambdas <- read.csv(here("01_outdata", "period", ">20period_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(28343)
period20_boot <- bootstrap_by_rows(period20_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
period20_boot_CI <- as.data.frame(period20_boot$interval_data) # save CI intervals in a separate object
period20_boot_CI$year <- as.numeric(period20_boot_CI$year) # change year from character to numeric

# tidy
period20_boot_df <- period20_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., period20.lpi %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(period20_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
period20_boot_df[53,] # final index in 2022 = 1.107471
nrow(greaterthan20period.lpidata) # 1405 popns included in this subset
length(unique(greaterthan20period.lpidata$Binomial)) # 617 species included in this subset


## calculate LPI for time series with ≥15 year period
set.seed(39484)
period15.lpi <- LPIMain(create_infile(greaterthan15period.lpidata,
                                      name="01_outdata/period/>15period_data",
                                      start_col_name = "X1970",
                                      end_col_name = "X2022",
                                      CUT_OFF_YEAR = 1970),
                        REF_YEAR = 1970,
                        PLOT_MAX = 2022,
                        BOOT_STRAP_SIZE = 10000,
                        # DATA_LENGTH_MIN = 3,     # commented out, since we've already done this manually
                        VERBOSE=FALSE, 
                        LINEAR_MODEL_SHORT_FLAG = 1,
                        force_recalculation = 1
)

# bootstrap CIs by species lambdas
period15_spp_lambdas <- read.csv(here("01_outdata","period", ">15period_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(828483)
period15_boot <- bootstrap_by_rows(period15_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
period15_boot_CI <- as.data.frame(period15_boot$interval_data) # save CI intervals in a separate object
period15_boot_CI$year <- as.numeric(period15_boot_CI$year) # change year from character to numeric

# tidy
period15_boot_df <- period15_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., period15.lpi %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(period15_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
period15_boot_df[53,] # final index in 2022 = 1.084123
nrow(greaterthan15period.lpidata) # 2077 popns included in this subset
length(unique(greaterthan15period.lpidata$Binomial)) # 716 species included in this subset


## calculate LPI for time series with ≥10 year period
set.seed(828749)
period10.lpi <- LPIMain(create_infile(greaterthan10period.lpidata,
                                      name="01_outdata/period/>10period_data",
                                      start_col_name = "X1970",
                                      end_col_name = "X2022",
                                      CUT_OFF_YEAR = 1970),
REF_YEAR = 1970,
PLOT_MAX = 2022,
BOOT_STRAP_SIZE = 10000,
# DATA_LENGTH_MIN = 3,     # commented out, since we've already done this manually
VERBOSE=FALSE, 
LINEAR_MODEL_SHORT_FLAG = 1,
force_recalculation = 1
)

# bootstrap CIs by species lambdas
period10_spp_lambdas <- read.csv(here("01_outdata","period", ">10period_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(394978)
period10_boot <- bootstrap_by_rows(period10_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
period10_boot_CI <- as.data.frame(period10_boot$interval_data) # save CI intervals in a separate object
period10_boot_CI$year <- as.numeric(period10_boot_CI$year) # change year from character to numeric

# tidy
period10_boot_df <- period10_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., period10.lpi %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(period10_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
period10_boot_df[53,] # final index in 2022 = 1.090301
nrow(greaterthan10period.lpidata) # 2774 popns included in this subset
length(unique(greaterthan10period.lpidata$Binomial)) # 799 species included in this subset


## calculate LPI for time series with ≥5 year period
set.seed(838292)
period5.lpi <- LPIMain(create_infile(greaterthan5period.lpidata,
                                     name="01_outdata/period/>5period_data",
                                     start_col_name = "X1970",
                                     end_col_name = "X2022",
                                     CUT_OFF_YEAR = 1970
),
REF_YEAR = 1970,
PLOT_MAX = 2022,
BOOT_STRAP_SIZE = 10000,
# DATA_LENGTH_MIN = 3,     # commented out, since we've already done this manually
VERBOSE=FALSE, 
LINEAR_MODEL_SHORT_FLAG = 1,
force_recalculation = 1
)

# bootstrap CIs by species lambdas
period5_spp_lambdas <- read.csv(here("01_outdata","period", ">5period_data_pops_lambda.csv"), header=TRUE) # read in spp lambdas
set.seed(28493)
period5_boot <- bootstrap_by_rows(period5_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
period5_boot_CI <- as.data.frame(period5_boot$interval_data) # save CI intervals in a separate object
period5_boot_CI$year <- as.numeric(period5_boot_CI$year) # change year from character to numeric

# tidy
period5_boot_df <- period5_boot_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., period5.lpi %>% 
              rownames_to_column(var="year") %>% 
              mutate(year = as.numeric(year)) %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(period5_boot_df, col="#c1e6db", line_col = "#66C2A5")

# summary stats
period5_boot_df[53,] # final index in 2022 = 0.976429
nrow(greaterthan5period.lpidata) # 3550 popns included in this subset
length(unique(greaterthan5period.lpidata$Binomial)) # 880 species included in this subset

## Plot all four scenarios (>20, >15, >10, >5 year period) together:
# order names so it plots in logical order
namesvec <- c("≥5 years", "≥10 years", "≥15 years", "≥20 years")
namesvec <- factor(namesvec, levels=c("≥5 years", "≥10 years", "≥15 years", "≥20 years"))

# code plot
period_plot <- ggplot_multi_lpi(list(period5_boot_df, period10_boot_df, period15_boot_df, period20_boot_df),
                                names = namesvec,
                                facet=TRUE) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); period_plot

# save for shiny
period5_boot_df2 <- period5_boot_df %>% mutate(period=5)
period10_boot_df2 <- period10_boot_df %>% mutate(period=10)
period15_boot_df2 <- period15_boot_df %>% mutate(period=15)
period20_boot_df2 <- period20_boot_df %>% mutate(period=20)
period_df <- rbind(period5_boot_df2, period10_boot_df2, period15_boot_df2, period20_boot_df2)
head(period_df) # check
tail(period_df) # check
write.csv(period_df, file=here("01_outdata", "period", "period_CIspp.csv"))

#### 6.1: process data for treatment of zeros ----

# let's get summary statistics of the proportion of zeroes in the current CAD LPI dataset 
# this means that we need to exclude time series with only two data points (zero considered a data point)

cad_z_3plus <- cad_z %>% 
  mutate(num.datapoints = rowSums(!is.na(select(., X1970:X2022)))) %>% # calculate the number of datapoints (non-NA values) per time series
  filter(num.datapoints>2) %>%    # filter for only time series with 3 or more datapoints 
  dplyr::select(-num.datapoints) # remove this col


## identify leading, middle, trailing zeros in the df 
# Calculate the nb of 0s present for each time series (ID)
nb_zeros_per_id <- cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  filter(value == 0) %>%
  group_by(ID) %>%
  summarise(n0 = n())

# How many rows have 0s?
nrow(nb_zeros_per_id) # 285 out of 
nrow(cad_z_3plus) # the 3758 time series

# Compare to the total number of values for that time series (non NULL)
nb_values_per_id <- cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  filter(!is.na(value)) %>%
  group_by(ID) %>%
  summarise(n = n()) 

# Getting a clean long dataset
cad_long <- cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year")
cad_long$Year=gsub("X","",cad_long$Year) # Remove the X to only have years

# Create df with info on zeros for each time series (each ID)
data_zeros <- left_join(x=nb_values_per_id,y=nb_zeros_per_id,by="ID") %>% 
  replace(is.na(.),0)
# ID = ID of the time series
# n = number of values in this time series (non NULL)
# n0 = number of zeros in this time series 


# calculate proportion of zeroes in the time series 
data_zeros %>% 
  mutate(prop0 = n0/n) %>% 
  arrange(desc(prop0)) %>% 
  summarise(min = min(prop0), 
            max = max(prop0), 
            mean = mean(prop0))

# Extracting the first non NULL value for each row
# In column data_zeros$first_value
cad_zeros <- cad_long %>% 
  filter(!is.na(value)) %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==1) %>% 
  rename(first_value = Year) %>% 
  dplyr::select(ID, first_value) %>% 
  left_join(x=cad_z,y=.,by="ID")

# Extracting the last non NULL value for each row
# In column data_zeros$last_value
cad_zeros <- cad_long %>% 
  filter(!value == "NULL") %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==n()) %>% 
  rename(last_value = Year) %>% 
  dplyr::select(ID, last_value) %>% 
  left_join(x=cad_zeros,y=.,by="ID")

# Extracting the first ZERO value for each row
# In column data_zeros$first_zero
cad_zeros <- cad_long %>% 
  filter(value == 0) %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==1) %>% 
  rename(first_zero = Year) %>% 
  select(ID, first_zero) %>% 
  left_join(x=cad_zeros,y=.,by="ID")

# Extracting the last ZERO value for each row
# In column data_zeros$last_zero
cad_zeros <- cad_long %>% 
  filter(value == 0) %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==n()) %>% 
  rename(last_zero = Year) %>% 
  select(ID, last_zero) %>% 
  left_join(x=cad_zeros,y=.,by="ID") 

# Calculate duration of the time series (nb of years between first and last non null)
cad_zeros$duration <- as.numeric(cad_zeros$last_value)-as.numeric(cad_zeros$first_value)

# Extracting the first non ZERO value for each row
# In column data_zeros$first_non0
cad_zeros <- cad_long %>% 
  filter(!value == "NULL"& !value==0) %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==1) %>% 
  rename(first_non0 = Year) %>% 
  select(ID, first_non0) %>% 
  left_join(x=cad_zeros,y=.,by="ID")

# Extracting the last non ZERO value for each row
# In column data_zeros$last_non0
cad_zeros <- cad_long %>% 
  filter(!value == "NULL"& !value==0) %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  filter(row_number()==n()) %>% 
  rename(last_non0 = Year) %>% 
  select(ID, last_non0) %>% 
  left_join(x=cad_zeros,y=.,by="ID")

## Count the location of all zeros
# Number of middle, start, end for each ID

# How many middle 0s?
cad_zeros <- cad_long %>% 
  filter(!value == "NULL") %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  mutate(firstnon0 = min(row_number()[value!=0])) %>% 
  mutate(lastnon0 = max(row_number()[value!=0])) %>% 
  filter(row_number()>firstnon0 & row_number()<lastnon0) %>% 
  filter(value==0) %>% 
  summarise(nb_middle0s = n()) %>% 
  left_join(x=cad_zeros,y=.,by="ID") 

# How many start 0s?
cad_zeros <- cad_long %>% 
  filter(!value == "NULL") %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  mutate(firstnon0 = min(row_number()[value!=0])) %>% 
  filter(row_number()<firstnon0) %>% 
  summarise(nb_start0s = n()) %>% 
  left_join(x=cad_zeros,y=.,by="ID") 

# How many end 0s?
cad_zeros <- cad_long %>% 
  filter(!value == "NULL") %>% 
  group_by(ID) %>% 
  arrange(Year) %>% 
  mutate(lastnon0 = max(row_number()[value!=0])) %>% 
  filter(row_number()>lastnon0) %>% 
  summarise(nb_end0s = n()) %>% 
  left_join(x=cad_zeros,y=.,by="ID") 

#### 6.2: explore characteristics of zeros ----

## How are the zeroes distributed in the dataset?
n0 <- cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>% # Change data format from 'wide' to 'long', putting all column names specified to a 'Year' column.
  filter(value == 0) %>% # Keep only data points that are equal to zero
  group_by(ID) %>%  # Create population groups
  summarise(n0 = n()) # Count number of zero data points in each ID
n0 %>%
  summarise(min(n0), 
            max(n0), 
            mean(n0)) # the maximum number of zeros in a dataset = 15, min = 1, mean = 4.64

## In what years do we see the zeroes? (proportional to the number of data points)
cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>%   # Change data format from 'wide' to 'long', putting all column names specified to a 'Year' column.
  filter(!is.na(value)) %>%  #remove empty slots
  group_by(Year) %>% # Enable summarise to count for each group (year)
  summarise(n = n()) %>% # Count number of data points
  left_join(., cad_zc %>%    # Add another data frame (specified within left join). Redo analysis but add a filter, keep only zeroes
              pivot_longer(X1970:X2022, names_to = "Year") %>%
              filter(value == 0) %>%
              group_by(Year) %>%
              summarise(n0 = n()), by = "Year") %>%
  mutate_at("n0", ~tidyr::replace_na(., 0)) %>%   #No zeros - change from NA to 0
  mutate(prop = n0/n) %>% # Calculate proportion (divide number of zeros by number of datapoints)
  ggplot(aes(x = Year, y = prop))+  # Plot with ggplot
  geom_bar(stat = "identity")+ #make sure proportion is used for y
  theme_bw()+  # theme to make nicer
  xlab("Year")+  #change x axis label
  ylab("Proportion of Zeroes (n0/n)")+ #change y axis label
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  #vertical text
# majority of zeros are from 2010-2022, and in a single year most occur in 2022

# which popn time series have zeroes?
# statistics
cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  filter(!is.na(value)) %>%
  filter(value == 0) %>%
  group_by(Taxa) %>%
  summarise(n0 = n())
# none in birds


## Taxonomic distribution of zeroes (as proportions)
# figure 
cad_z_3plus %>% 
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  filter(!is.na(value)) %>%
  group_by(Taxa) %>%
  summarise(n_datapoints = n()) %>%
  left_join(., cad_z_3plus %>% 
              pivot_longer(X1970:X2022, names_to = "Year") %>%
              filter(!is.na(value)) %>%
              filter(value == 0) %>%
              group_by(Taxa) %>%
              summarise(n0 = n()), by = "Taxa") %>% 
  mutate_at("n0", ~tidyr::replace_na(.,0)) %>% # Replace NAs in n0 (when there is no 0) with 0: no zeros found
  mutate(prop = n0/n_datapoints) %>%
  ggplot(aes(x = Taxa, y = prop))+  # Plot as bar chart
  geom_bar(stat = "identity")+
  xlab("Taxon")+
  ylab("Proportion of Zeroes (n0/n)")+
  theme_bw()
# most zeros occur in mammals, none in birds

#### 6.3: compare options for treatment of zeros ----

## options for treatment of zeros:
# 1. replace 0s with NA (C-LPI default)
# 2. add 1% of the mean value (inbuilt into LPIMain)
# 3. add minimum value of time series to zero (inbuilt into LPIMain)
# 4. add 1 to all values (inbuilt into LPIMain)
# 5. replace 0s with a small value (0.000001)
# 6. replace leading zeroes with NA, middle with NA, trailing with 1% of the mean
# 7. replace leading zeroes with 1% of the mean, middle with NA, trailing with 1% of the mean

## option 1: replace 0s with NA (C-LPI default)
# note: this is the same output as 2.1: calculate confidence intervals (3 methods) using species bootsrapping
ggplot_lpi(boot_spp_df, col="#fdd1c0", line_col = "#FC8D62")
boot_spp_df[53,] # the final (2022) C-LPI value using this method is 1.01151

## option 2: add 1% of the mean value (inbuilt into LPIMain)
# run LPIMain
set.seed(28485)
lpi2 <- LPIMain(create_infile(cad_z, 
                              name = "./01_outdata/zeros/zeros_lpi2", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1,
                ZERO_REPLACE_FLAG = 1)  # to add 1% of mean

# make rownames (year) to separate col
lpi2 <- lpi2 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi2) <- lpi2$year

# boostrap CIs by species lambdas 
lpi2_spp_lambdas <- read.csv(here("01_outdata","zeros", "zeros_lpi2_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(92847)
lpi2_boot_spp <- bootstrap_by_rows(lpi2_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi2_boot_spp_CI <- as.data.frame(lpi2_boot_spp$interval_data) # save CI intervals in a separate object
lpi2_boot_spp_CI$year <- as.numeric(lpi2_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi2_df <- lpi2_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi2 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

ggplot_lpi(lpi2_df, col="#fdd1c0", line_col = "#FC8D62")
lpi2_df[53,] # the final (2022) C-LPI value using this method is 1.015596

## option 3: add minimum value of time series to zero (inbuilt into LPIMain)
# run LPIMain
set.seed(28574)
lpi3 <- LPIMain(create_infile(cad_z, 
                              name = "./01_outdata/zeros/zeros_lpi3", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1,
                ZERO_REPLACE_FLAG = 0)  # to add minimum value

# make rownames (year) to separate col
lpi3 <- lpi3 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi3) <- lpi3$year

# boostrap CIs by species lambdas 
lpi3_spp_lambdas <- read.csv(here("01_outdata", "zeros", "zeros_lpi3_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(72947)
lpi3_boot_spp <- bootstrap_by_rows(lpi3_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi3_boot_spp_CI <- as.data.frame(lpi3_boot_spp$interval_data) # save CI intervals in a separate object
lpi3_boot_spp_CI$year <- as.numeric(lpi3_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi3_df <- lpi3_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi3 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(lpi3_df, col="#fdd1c0", line_col = "#FC8D62")
lpi3_df[53,] # the final (2022) C-LPI value using this method is 1.025049

## option 4: add 1 to all values (inbuilt into LPIMain)
# run LPIMain
set.seed(827473)
lpi4 <- LPIMain(create_infile(cad_z, 
                              name = "./01_outdata/zeros/zeros_lpi4", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1,
                ZERO_REPLACE_FLAG = 2)  # to add +1 to zero values

# make rownames (year) to separate col
lpi4 <- lpi4 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi4) <- lpi4$year

# boostrap CIs by species lambdas 
lpi4_spp_lambdas <- read.csv(here("01_outdata", "zeros", "zeros_lpi4_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(4856274)
lpi4_boot_spp <- bootstrap_by_rows(lpi4_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi4_boot_spp_CI <- as.data.frame(lpi4_boot_spp$interval_data) # save CI intervals in a separate object
lpi4_boot_spp_CI$year <- as.numeric(lpi4_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi4_df <- lpi4_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi4 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(lpi4_df, col="#fdd1c0", line_col = "#FC8D62")
lpi4_df[53,] # the final (2022) C-LPI value using this method is 0.9935973

## option 5: replace 0s with a small value (0.000001)
# tidy
cad_z_lpi5 <- cad_z %>%
  mutate(across(X1970:X2022, ~ifelse(. == 0, 0.000001, .))) # replace any zeroes in cols X1970-X2022 with 0.000001

# run LPIMain
set.seed(56373)
lpi5 <- LPIMain(create_infile(cad_z_lpi5, 
                              name = "./01_outdata/zeros/zeros_lpi5", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1) 

# make rownames (year) to separate col
lpi5 <- lpi5 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi5) <- lpi5$year

# boostrap CIs by species lambdas 
lpi5_spp_lambdas <- read.csv(here("01_outdata", "zeros", "zeros_lpi5_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(62947)
lpi5_boot_spp <- bootstrap_by_rows(lpi5_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi5_boot_spp_CI <- as.data.frame(lpi5_boot_spp$interval_data) # save CI intervals in a separate object
lpi5_boot_spp_CI$year <- as.numeric(lpi5_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi5_df <- lpi5_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi5 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(lpi5_df, col="#fdd1c0", line_col = "#FC8D62")
lpi5_df[53,] # the final (2022) C-LPI value using this method is 1.04001

## option 6: replace leading zeroes with NA, middle with NA, trailing with 1% of the mean
# format data--set leading & middle zeros to NA
cad_zeros_lpi6 <- cad_zeros %>% 
  select(ID, first_value, last_value, first_non0, last_non0, first_zero)

cad_z_lpi6 <- cad_z %>% 
  left_join(., cad_zeros_lpi6, by = "ID") %>%
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  mutate(YearAsNum = as.numeric(str_remove(Year, "X"))) %>% # Make years as numeric. Removing "X"
  rowwise() %>% 
  mutate(value = case_when(   # Replace 0s with nulls when Leading or middle.
    value == "0" & YearAsNum %in% c(first_value:first_non0) ~ NA, #LEADING 0s--Checking if the year is in between the first value and the first non 0 year
    value == "0" & !(YearAsNum %in% c(first_value:first_non0) | YearAsNum %in% (last_non0:last_value)) ~ NA, #Middle Values--checking if the year is NOT in between the first year with a value and the first non 0 year OR NOT in between the last non 0 year and the last year with a value
    .default = value)) %>%   # as deafault put the value found in value (works as "else")
  select(-YearAsNum) %>%  
  select(-first_value, -last_value, -first_non0, -last_non0, -first_zero) %>% #removing zeros cols
  pivot_wider(names_from = Year, values_from = value)  #changing back to wider format 

# run LPIMain
set.seed(726493)
lpi6 <- LPIMain(create_infile(cad_z_lpi6, 
                              name = "./01_outdata/zeros/zeros_lpi6", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1,
                ZERO_REPLACE_FLAG = 1)    # to add 1% of mean to zero observations

# make rownames (year) to separate col
lpi6 <- lpi6 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi6) <- lpi6$year

# boostrap CIs by species lambdas 
lpi6_spp_lambdas <- read.csv(here("01_outdata", "zeros", "zeros_lpi6_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(462859)
lpi6_boot_spp <- bootstrap_by_rows(lpi6_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi6_boot_spp_CI <- as.data.frame(lpi6_boot_spp$interval_data) # save CI intervals in a separate object
lpi6_boot_spp_CI$year <- as.numeric(lpi6_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi6_df <- lpi6_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi6 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(lpi6_df, col="#fdd1c0", line_col = "#FC8D62")
lpi6_df[53,] # the final (2022) C-LPI value using this method is 0.8541554

## option 7: replace leading zeroes with 1% of the mean, middle with NA, trailing with 1% of the mean
# format data--set only middle zeros to NA
cad_zeros_lpi7 <- cad_zeros %>% 
  select(ID, first_value, last_value, first_non0, last_non0, first_zero)

cad_z_lpi7 <- cad_z %>% 
  left_join(., cad_zeros_lpi7, by = "ID") %>%
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  mutate(YearAsNum = as.numeric(str_remove(Year, "X"))) %>% # Make years as numeric. Removing "X"
  rowwise() %>% 
  mutate(value = case_when(   # Replace 0s with nulls when in middle
    value == "0" & !(YearAsNum %in% c(first_value:first_non0) | YearAsNum %in% (last_non0:last_value)) ~ NA, #Middle Values--checking if the year is NOT in between the first year with a value and the first non 0 year OR NOT in between the last non 0 year and the last year with a value
    .default = value)) %>%   # as default put the value found in value (works as "else")
  select(-YearAsNum) %>%  
  select(-first_value, -last_value, -first_non0, -last_non0, -first_zero) %>% #removing zeros cols
  pivot_wider(names_from = Year, values_from = value)  #changing back to wider format 

# run LPIMain
set.seed(8264563)
lpi7 <- LPIMain(create_infile(cad_z_lpi7, 
                              name = "./01_outdata/zeros/zeros_lpi7", 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970), 
                REF_YEAR = 1970,
                PLOT_MAX=2022,
                LINEAR_MODEL_SHORT_FLAG = TRUE,
                BOOT_STRAP_SIZE = 10000,
                DATA_LENGTH_MIN = 3,
                VERBOSE=TRUE,
                SHOW_PROGRESS =FALSE,
                force_recalculation = 1,
                ZERO_REPLACE_FLAG = 1)    # to add 1% of mean to zero observations

# make rownames (year) to separate col
lpi7 <- lpi7 %>%
  mutate(year = as.numeric(rownames(.))) %>%   # set year as a numeric class
  filter(!year==2023)                          # remove this--we only want till 2022
rownames(lpi7) <- lpi7$year

# boostrap CIs by species lambdas 
lpi7_spp_lambdas <- read.csv(here("01_outdata", "zeros", "zeros_lpi7_pops_lambda.csv"), header=TRUE) # read in pops lambda (spp lambda) file
set.seed(729485)
lpi7_boot_spp <- bootstrap_by_rows(lpi7_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000)
lpi7_boot_spp_CI <- as.data.frame(lpi7_boot_spp$interval_data) # save CI intervals in a separate object
lpi7_boot_spp_CI$year <- as.numeric(lpi7_boot_spp_CI$year) # change year from character to numeric

# tidy (so that it plots bootstrapped CIs & lpi2 mean trendline)
lpi7_df <- lpi7_boot_spp_CI %>% 
  dplyr::select(year, Upper_CI, Lower_CI) %>% 
  rename(CI_high = Upper_CI,
         CI_low = Lower_CI) %>% 
  left_join(., lpi7 %>% 
              dplyr::select(year, LPI_final), 
            by="year") %>% 
  column_to_rownames(var="year")

# plot
ggplot_lpi(lpi7_df, col="#fdd1c0", line_col = "#FC8D62")
lpi7_df[53,] # the final (2022) C-LPI value using this method is 1.050577

# 1. replace 0s with NA (C-LPI default)
# 2. add 1% of the mean value (inbuilt into LPIMain)
# 3. add minimum value of time series to zero (inbuilt into LPIMain)
# 4. add 1 to all values (inbuilt into LPIMain)
# 5. replace 0s with a small value (0.000001)
# 6. replace leading zeroes with NA, middle with NA, trailing with 1% of the mean
# 7. replace leading zeroes with 1% of the mean, middle with NA, trailing with 1% of the mean


## show all trends together
zero_options_plot <- ggplot_multi_lpi(list(boot_spp_df, lpi2_df, lpi3_df, lpi4_df, lpi5_df, lpi6_df, lpi7_df), 
                             names=c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5", "Option 6", "Option 7"), 
                             col="Dark2", 
                             facet=TRUE,
                             ylims = c(0.7, 1.3)) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=8)); zero_options_plot

# save data for shiny
write.csv(lpi2_df, here("01_outdata", "zeros", "zeros_option2_CIspp.csv"))
write.csv(lpi3_df, here("01_outdata", "zeros", "zeros_option3_CIspp.csv"))
write.csv(lpi4_df, here("01_outdata", "zeros", "zeros_option4_CIspp.csv"))
write.csv(lpi5_df, here("01_outdata", "zeros", "zeros_option5_CIspp.csv"))
write.csv(lpi6_df, here("01_outdata", "zeros", "zeros_option6_CIspp.csv"))
write.csv(lpi7_df, here("01_outdata", "zeros", "zeros_option7_CIspp.csv"))


#### 7.1: process data for outlier analysis ----

# the following sections will require: 
# 1. the unweighted C-LPI (ie. the dataset with 0% of outliers removed)
# 2. species lambda values from the unweighted C-LPI (ie. spp_lambda object)

# remove additional columns to leave only lambda values & species names
spp_lambdas[ , c('X','Freq','X1970')] <- list(NULL)
colnames(spp_lambdas) # check

# convert lambda file to long form
melted_spp_lambdas = pivot_longer(spp_lambdas, cols = !c(Binomial), values_to = "lambda", names_to = "year")

# Get rid of NAs
melted_spp_lambdas_nona = subset(melted_spp_lambdas, !is.na(lambda))

# remove "X" from years col
melted_spp_lambdas_nona$year = as.numeric(paste(gsub("X", "", melted_spp_lambdas_nona$year)))

# Calculate lambda metrics
spp_lambda_metrics = melted_spp_lambdas_nona %>% 
  group_by(Binomial) %>%
  summarise(sum_lambda = sum(lambda))

# join lambdas to original canadian data file (cad)
lpi_data <- left_join(cad,spp_lambda_metrics,by = 'Binomial')

# check join has worked (unique lambda per spp/pop) 
(lpi_data$sum_lambda)

## define quantiles
# Get extreme species
lpi_data$sum_lambda = as.numeric(lpi_data$sum_lambda)
spp_lambdas_unique = lpi_data %>% 
  select(Binomial, sum_lambda) %>%
  distinct() %>% 
  filter(!is.na(sum_lambda)) # note: species with NAs for sum_lambda are species with only 2 data points (excluded in C-LPI)

# assign quantiles to spp lambdas
spp_low_thresh = quantile(spp_lambdas_unique$sum_lambda, probs = c(0.05, 0.1, 0.15))
spp_high_thresh = quantile(spp_lambdas_unique$sum_lambda, probs = (1 - c(0.05, 0.1, 0.15)))

#### 7.2: compare trends removing extreme low outliers ----

## LOW threshold
low_trends = list() # define empty list to write loop outputs to

# for loop to calculate LPI for each low threshold 
set.seed(8747351)
for (k in 1:length(spp_low_thresh)){
  
  nrow(lpi_data) # count rows in df
  
  low_threshold = spp_low_thresh[k] # define the threshold
  
  Canada_low_pops = subset(lpi_data, sum_lambda > low_threshold) # subset data to species with sum_lambda above threshold
  
  nrow(Canada_low_pops) # count rows in df
  
  # calculate LPIMain on subset df
  Canada_low_lpi <- LPIMain(create_infile(Canada_low_pops, 
                                          name = paste0("01_outdata/outliers/lower_threshold_", str_remove(names(low_threshold), "%")), # removing % from name because this breaks LPIMain
                                          start_col_name = "X1970",
                                          end_col_name = "X2022", 
                                          CUT_OFF_YEAR = 1970), 
                            REF_YEAR = 1970,
                            PLOT_MAX = 2022,
                            LINEAR_MODEL_SHORT_FLAG = TRUE,
                            BOOT_STRAP_SIZE = 10000,
                            VERBOSE=TRUE,
                            SHOW_PROGRESS =FALSE,
                            DATA_LENGTH_MIN = 3,
                            force_recalculation = 1)
  
  # calculate summary statistics
  Canada_low_lpi$npops = nrow(Canada_low_pops) # count number of popns
  Canada_low_lpi$nsp = length(unique(Canada_low_pops$Binomial)) # count number of species 
  Canada_low_lpi$threshold = low_threshold # get threshold
  Canada_low_lpi$pct = names(low_threshold) # assign name of threshold
  
  # create CIs by bootstrapping species lambdas
  lambda_file <- paste0("lower_threshold_", str_remove(names(low_threshold), "%"), "_pops_lambda.csv") # specify lambda file to read in 
  boot_spp_lambdas <- read.csv(here("01_outdata", "outliers", lambda_file)) # read in spp lambdas
  boot_out <- bootstrap_by_rows(boot_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000) # run bootstrap
  boot_out_CI <- as.data.frame(boot_out$interval_data) # save CI intervals in a separate object
  boot_out_CI$year <- as.numeric(boot_out_CI$year) # change year from character to numeric
  
  # tidy--add spp lambda bootstrapped CI to LPI_final and summary stats from above
  boot_out_df <- boot_out_CI %>% 
    dplyr::select(year, Upper_CI, Lower_CI) %>% 
    rename(CI_high = Upper_CI,
           CI_low = Lower_CI) %>% 
    left_join(., Canada_low_lpi %>% 
                rownames_to_column(var="year") %>% 
                mutate(year = as.numeric(year)) %>% 
                dplyr::select(year, LPI_final, npops, nsp, threshold, pct), 
              by="year") %>% 
    column_to_rownames(var="year")
  
  # write outputs to low_trends list
  low_trends[[names(low_threshold)]] <- boot_out_df
  
}

# inspect output
low_trends 

# plot
names_vec <- c("lower 5%", "lower 10%", "lower 15%")
names_vec <- factor(names_vec, levels=c("lower 5%", "lower 10%", "lower 15%"))
low_trend_plot <- ggplot_multi_lpi(list(low_trends$`5%`, low_trends$`10%`, low_trends$`15%`), 
                 names = names_vec, 
                 col="Dark2",
                 ylim=c(0.9,2.1)) +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank(), 
        legend.position = "top");low_trend_plot

# save for shiny
low_trends_df <- do.call(rbind, low_trends) %>% 
  rownames_to_column(var="year") %>% 
  mutate(year = str_remove(year, "15%.|10%.|5%."))
head(low_trends_df) # check
tail(low_trends_df) # check
write.csv(low_trends_df, file=here("01_outdata", "outliers", "lwr_outliers_CIspp.csv"), row.names = FALSE)


#### 7.3: compare trends removing extreme high outliers ----
## HIGH threshold
high_trends = list() # define empty list to write loop outputs to
set.seed(18473)

# for loop to calculate LPI for each low threshold 
for (k in 1:length(spp_high_thresh)){
  
  nrow(lpi_data) # count rows in df
  
  high_threshold = spp_high_thresh[k] # define the threshold
  
  Canada_high_pops = subset(lpi_data, sum_lambda < high_threshold) # subset data to species with sum_lambda below threshold
  
  nrow(Canada_high_pops) # count rows in df
  
  # calculate LPIMain on subset df
  Canada_high_lpi <- LPIMain(create_infile(Canada_high_pops, 
                                          name = paste0("01_outdata/outliers/higher_threshold_", str_remove(names(high_threshold), "%")), # removing % from name because this breaks LPIMain
                                          start_col_name = "X1970",
                                          end_col_name = "X2022", 
                                          CUT_OFF_YEAR = 1970), 
                            REF_YEAR = 1970,
                            PLOT_MAX = 2022,
                            LINEAR_MODEL_SHORT_FLAG = TRUE,
                            BOOT_STRAP_SIZE = 10000,
                            VERBOSE=TRUE,
                            SHOW_PROGRESS =FALSE,
                            DATA_LENGTH_MIN = 3,
                            force_recalculation = 1)
  
  # calculate summary statistics
  Canada_high_lpi$npops = nrow(Canada_high_pops) # count number of popns
  Canada_high_lpi$nsp = length(unique(Canada_high_pops$Binomial)) # count number of species 
  Canada_high_lpi$threshold = high_threshold # get threshold
  Canada_high_lpi$pct = names(high_threshold) # assign name of threshold
  
  # create CIs by bootstrapping species lambdas
  lambda_file <- paste0("higher_threshold_", str_remove(names(high_threshold), "%"), "_pops_lambda.csv") # specify lambda file to read in 
  boot_spp_lambdas <- read.csv(here("01_outdata", "outliers", lambda_file)) # read in spp lambdas
  boot_out <- bootstrap_by_rows(boot_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000) # run bootstrap
  boot_out_CI <- as.data.frame(boot_out$interval_data) # save CI intervals in a separate object
  boot_out_CI$year <- as.numeric(boot_out_CI$year) # change year from character to numeric
  
  # tidy--add spp lambda bootstrapped CI to LPI_final and summary stats from above
  boot_out_df <- boot_out_CI %>% 
    dplyr::select(year, Upper_CI, Lower_CI) %>% 
    rename(CI_high = Upper_CI,
           CI_low = Lower_CI) %>% 
    left_join(., Canada_high_lpi %>% 
                rownames_to_column(var="year") %>% 
                mutate(year = as.numeric(year)) %>% 
                dplyr::select(year, LPI_final, npops, nsp, threshold, pct), 
              by="year") %>% 
    column_to_rownames(var="year")
  
  # write outputs to low_trends list
  high_trends[[names(high_threshold)]] <- boot_out_df
  
}

# inspect output
high_trends 

# plot
names_vec <- c("upper 5%", "upper 10%", "upper 15%")
names_vec <- factor(names_vec, levels=c("upper 5%", "upper 10%", "upper 15%"))
high_trend_plot <- ggplot_multi_lpi(list(high_trends$`95%`, high_trends$`90%`, high_trends$`85%`), 
                 names = names_vec, 
                 col="Dark2",
                 ylim=c(0, 1.2)) +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12), 
        legend.title = element_blank(), 
        legend.position = "top");high_trend_plot

# save for shiny
high_trends_df <- do.call(rbind, high_trends) %>% 
  rownames_to_column(var="year") %>% 
  mutate(year = str_remove(year, "85%.|90%.|95%."))
head(high_trends_df) # check
tail(high_trends_df) # check
write.csv(high_trends_df, file=here("01_outdata", "outliers", "upr_outliers_CIspp.csv"), row.names = FALSE)


#### 7.4: compare trends removing extreme high & low outliers ----

## BOTH upper & lower extremes removed

both_trends = list() # define empty list to write loop outputs to
  
# for loop to calculate LPI for each upper & lower threshold together
set.seed(194739)
for (k in 1:length(spp_low_thresh)) {
  nrow(lpi_data)
  
  low_threshold = spp_low_thresh[k] # define the threshold
  high_threshold = spp_high_thresh[k] # define the threshold
  
  Canada_high_low_pops = subset(lpi_data, sum_lambda > low_threshold & sum_lambda < high_threshold) # subset data to species with sum_lambda below high threshold & above low threshold
  
  nrow(Canada_high_low_pops)
  
  # calculate LPIMain on subset df
  Canada_high_low_lpi <- LPIMain(create_infile(Canada_high_low_pops, 
                                           name = paste0("01_outdata/outliers/high_low_threshold_", str_remove(names(low_threshold), "%")), # removing % from name because this breaks LPIMain
                                           start_col_name = "X1970",
                                           end_col_name = "X2022", 
                                           CUT_OFF_YEAR = 1970), 
                             REF_YEAR = 1970,
                             PLOT_MAX = 2022,
                             LINEAR_MODEL_SHORT_FLAG = TRUE,
                             BOOT_STRAP_SIZE = 10000,
                             VERBOSE=TRUE,
                             SHOW_PROGRESS =FALSE,
                             DATA_LENGTH_MIN = 3,
                             force_recalculation = 1)
  
  # calculate summary statistics
  Canada_high_low_lpi$npops = nrow(Canada_high_low_pops) # count number of popns
  Canada_high_low_lpi$nsp = length(unique(Canada_high_low_pops$Binomial)) # count number of species 
  Canada_high_low_lpi$threshold = low_threshold # get threshold--assigning low threshold name, since more intuitive that we're removing upper & lower 5%
  Canada_high_low_lpi$pct = names(low_threshold) # assign name of threshold
  
  # create CIs by bootstrapping species lambdas
  lambda_file <- paste0("high_low_threshold_", str_remove(names(low_threshold), "%"), "_pops_lambda.csv") # specify lambda file to read in 
  boot_spp_lambdas <- read.csv(here("01_outdata", "outliers", lambda_file)) # read in spp lambdas
  boot_out <- bootstrap_by_rows(boot_spp_lambdas, species_column_name="Binomial" , start_col_name="X1970",end_col_name="X2022", iter=TRUE , N=10000) # run bootstrap
  boot_out_CI <- as.data.frame(boot_out$interval_data) # save CI intervals in a separate object
  boot_out_CI$year <- as.numeric(boot_out_CI$year) # change year from character to numeric
  
  # tidy--add spp lambda bootstrapped CI to LPI_final and summary stats from above
  boot_out_df <- boot_out_CI %>% 
    dplyr::select(year, Upper_CI, Lower_CI) %>% 
    rename(CI_high = Upper_CI,
           CI_low = Lower_CI) %>% 
    left_join(., Canada_high_low_lpi %>% 
                rownames_to_column(var="year") %>% 
                mutate(year = as.numeric(year)) %>% 
                dplyr::select(year, LPI_final, npops, nsp, threshold, pct), 
              by="year") %>% 
    column_to_rownames(var="year")
  
  # write outputs to low_trends list
  both_trends[[names(low_threshold)]] <- boot_out_df # assigning low threshold name, since more intuitive that we're removing upper & lower 5%
  
}

# inspect output
both_trends 
boot_spp_df[53,] # get final (2022) LPI value
# CI range = 1.140173 - 0.8977526 = 0.2424204

both_trends$`15%`[53,] # get final (2022) LPI value
# CI range = 1.033384 - 0.919998 = 0.113386

# plot
names_vec <- c("upper & lower 0%", "upper & lower 5%", "upper & lower 10%", "upper & lower 15%")
names_vec <- factor(names_vec, levels=c("upper & lower 0%", "upper & lower 5%", "upper & lower 10%", "upper & lower 15%"))
both_extremes_plot <- ggplot_multi_lpi(list(boot_spp_df, both_trends$`5%`, both_trends$`10%`, both_trends$`15%`), 
                 names = names_vec, 
                 facet=TRUE, 
                 col="Dark2", 
                 ylim=c(0.7,1.3)) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12));both_extremes_plot

# save for shiny
both_trends_df <- do.call(rbind, both_trends) %>% 
  rownames_to_column(var="year") %>% 
  mutate(year = str_remove(year, "5%.|10%.|15%."))
head(both_trends_df) # check
tail(both_trends_df) # check
write.csv(both_trends_df, file=here("01_outdata", "outliers", "upr_lwr_outliers_CIspp.csv"), row.names = FALSE)

#### 8: manuscript figures ----

## figure 1: treatment of zeros 
names_vec <- c("NA (C-LPI)", "+1% mean", "+minimum", "+1", "+0.000001", "NA, NA,\n +1% mean", "+1% mean, NA,\n +1% mean")
names_vec <- factor(names_vec, levels=c("NA (C-LPI)", "+1% mean", "+minimum", "+1", "+0.000001", "NA, NA,\n +1% mean", "+1% mean, NA,\n +1% mean"))
zero_options_plot <- ggplot_multi_lpi(list(boot_spp_df, lpi2_df, lpi3_df, lpi4_df, lpi5_df, lpi6_df, lpi7_df), 
                                      names=names_vec, 
                                      col="Dark2", 
                                      facet=TRUE,
                                      ylims = c(0.7, 1.3), 
                                      yrbreaks = 10) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=17),
        axis.text.x = element_text(size=17), 
        strip.text.x = element_text(size = 17)); zero_options_plot

# save plot
ggsave(here("03_figures", "fig1_zero_options.png"), zero_options_plot, width=17, height=7)

## figure 2: confidence intervals (3 methods)
cad_boot_CIs <- ggplot_multi_lpi(list(boot_spp_df, boot_pop_df, u_cad), 
                                 names=factor(c("Species (C-LPI)", "Population", "Year"), levels = c("Species (C-LPI)", "Population", "Year")), 
                                 col="Set2", 
                                 facet=TRUE, 
                                 ylim=c(0.7, 1.3),
                                 yrbreaks = 10) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=10)); cad_boot_CIs

img_bootstrap_methods_boxplot <- df_bootstrap_data_range %>% 
  mutate(method = case_when(method=="Species" ~ "Species (C-LPI)", 
                            TRUE ~ method), 
         method = factor(method, levels=c("Species (C-LPI)", "Population", "Year"))) %>% 
  ggplot(aes(x=method,y=range, fill=method)) + 
  geom_boxplot() + 
  theme_classic() + 
  ylab("Credible Interval Range") + 
  xlab("Bootstrap Method") + 
  theme(text = element_text(size=15)) +
  scale_fill_manual(values = c("#c1e6db", "#fdd1c0", "#d1d9ea")) + 
  guides(fill="none");img_bootstrap_methods_boxplot

fig2_CIplot <- ggarrange(cad_boot_CIs, img_bootstrap_methods_boxplot, labels="auto", font.label = list(size = 15));fig2_CIplot

# save plot
ggsave(here("03_figures", "fig2_CIplot.png"), fig2_CIplot, width=10,height=5)


## figure 3: length/fullness (number of data points, time series period)
namesvec <- c("≥2 points", "≥3 points (C-LPI)", "≥6 points", "≥15 points")
namesvec <- factor(namesvec, levels=c("≥2 points", "≥3 points (C-LPI)", "≥6 points", "≥15 points"))
num_datapts_plot <- ggplot_multi_lpi(list(subset_2pts_boot_df, subset_3pts_boot_df, subset_6pts_boot_df, subset_15pts_boot_df), 
                                     names = namesvec, 
                                     col="BrBG",
                                     facet=TRUE,
                                     ylim=c(0.7,1.3),
                                     yrbreaks = 10) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=10));num_datapts_plot

namesvec <- c("≥5 years", "≥10 years", "≥15 years", "≥20 years")
namesvec <- factor(namesvec, levels=c("≥5 years", "≥10 years", "≥15 years", "≥20 years"))
period_plot <- ggplot_multi_lpi(list(period5_boot_df, period10_boot_df, period15_boot_df, period20_boot_df),
                                names = namesvec,
                                col="RdGy",
                                facet=TRUE,
                                ylim=c(0.7,1.3), 
                                yrbreaks = 10) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=10)); period_plot

fig3_length_fullness <- ggarrange(num_datapts_plot+rremove("xlab"), period_plot, labels="auto", nrow=2);fig3_length_fullness

# save plot
ggsave(filename = "03_figures/fig3_length_fullness.png",fig3_length_fullness,width=10,height=7)


## figure 4: modelling decisions 
fig4_modelling <- ggplot_multi_lpi(list(boot_spp_loglin_df, boot_spp_lin_df, boot_spp_gam_df), 
                                   names=c("log linear (<6 points)", "linear (<6 points, C-LPI)", "GAM"), 
                                   col="Set1", 
                                   facet=TRUE,
                                   ylim=c(0.7, 1.3)) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=15), 
        axis.text.x = element_text(size=12)); fig4_modelling
ggsave(here("03_figures", "fig4_modelling.png"), fig4_modelling, width=8, height=5)

## figure 5: outlier removal 
names_vec <- c("0% (C-LPI)", "5%", "10%", "15%")
names_vec <- factor(names_vec, levels=c("0% (C-LPI)", "5%", "10%", "15%"))
both_extremes_plot <- ggplot_multi_lpi(list(boot_spp_df, both_trends$`5%`, both_trends$`10%`, both_trends$`15%`), 
                                       names = names_vec, 
                                       facet=TRUE, 
                                       col="Dark2", 
                                       ylim=c(0.7,1.3),
                                       yrbreaks = 10) +
  guides(col="none", fill="none") +
  theme(text = element_text(size=17), 
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(size=17));both_extremes_plot

# save
ggsave(filename = "03_figures/fig5_outlier_removal.png",both_extremes_plot,width=12,height=7)

## figure 6: weighted v unweighted trend
weighting_trend <- ggplot_multi_lpi(list(boot_spp_df, w_dat2), 
                                    names=c("Unweighted (C-LPI)", "Weighted by taxa"), 
                                    col="Set2", 
                                    facet=TRUE,
                                    ylims = c(0.7, 1.3), 
                                    yrbreaks = 10) + 
  guides(fill="none", colour="none") + 
  theme(text = element_text(size=17),
        axis.text.x = element_text(size=17), 
        strip.text.x = element_text(size = 17));weighting_trend

# save
ggsave(here("03_figures/fig6_weightingsplot.png"), weighting_trend, height=5, width=9)


## figure 7: shifting baselines
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
        text=element_text(size=17), 
        legend.position = "left", 
        legend.title = element_text(size = 17), 
        legend.text = element_text(size = 17)); baselines_linear_plot

baselines_linear_mean_lpi_boxplot <- baselines_linear_df |> 
  ggplot(aes(x = as.factor(initial_year), y = LPI_final, fill=initial_year))  + 
  geom_violin(width=0.8, alpha=0.5) + 
  geom_boxplot(width=0.3, color="slategrey", fill="white", alpha=0.9) + 
  scale_fill_scico_d(palette = "roma",name="Reference\n year") +
  guides(fill="none") +  # remove legend (same colour scheme as trend line legend)
  ylab("Index") + 
  xlab("Reference year") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), 
        text=element_text(size=17)); baselines_linear_mean_lpi_boxplot

baselines_linear_mean_lambdas_boxplot <- ggplot(baselines_linear_mean_lambdas |> filter(mean_lambda<1), aes(x = as.factor(initial_year), y = mean_lambda, fill=initial_year)) + # to remove initial years with 1 values
  geom_violin(width=0.8, alpha=0.5) + 
  geom_boxplot(width=0.3, color="slategrey", fill="white", alpha=0.9) + 
  scale_fill_scico_d(palette = "roma",name="Reference\n year") +
  guides(fill="none") +  # remove legend (same colour scheme as trend line legend)
  ylab("Mean lambda") + 
  xlab("Reference year") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), 
        text=element_text(size=17));baselines_linear_mean_lambdas_boxplot


# new format
baselinesplot <- ggarrange(baselines_linear_plot, 
                           baselines_linear_mean_lpi_boxplot, 
                           baselines_linear_mean_lambdas_boxplot, 
                           labels="auto", font.label = list(size = 17), ncol=3, common.legend = TRUE); baselinesplot

ggsave(filename = "03_figures/fig7_baselinesplot.png",baselinesplot,width=14,height=6)


## supplementary 1: location of zeros in the dataset 
# format data for plot
plot_dat <- cad_z %>%
  left_join(., cad_zeros %>% select(ID, duration, first_value, last_value), by = "ID") %>%
  #mutate_at(c("T_realm", "M_realm", "FW_realm"), ~ifelse(. == "NULL", NA, .)) %>% # Merge all the realm names together, not in 3 cols
  #mutate(realm = coalesce(T_realm, M_realm, FW_realm)) %>%
  pivot_longer(X1970:X2022, names_to = "Year") %>%
  mutate(Year = as.numeric(str_remove(Year, "X")), 
         first_value = as.numeric(first_value),
         last_value = as.numeric(last_value), 
         Taxa = case_when(Taxa=="Mammalia" ~ "Mammals",
                          Taxa=="Reptilia" ~ "Herps",
                          TRUE ~ Taxa)) %>% 
  filter(!(value == "NULL")) %>%
  mutate(value_label = ifelse(value == 0, "Zero value", "Non-zero value")) %>%
  #arrange(duration) %>%
  arrange(desc(last_value)) %>% 
  mutate(plot_row_id = row_number())

# plot 
zeros_strip_chart <- plot_dat %>% 
  ggplot(aes(x = Year, y = plot_row_id, color = value_label)) +
  geom_segment(aes(x = first_value, xend = last_value, y = plot_row_id, yend = plot_row_id),
               size = 0.1, col = "lightgrey",inherit.aes = FALSE) +
  geom_point(data=subset(plot_dat,value_label=="Non-zero value"), size = 0.2) + # plot non-zero pts behind zero pts
  geom_point(data=subset(plot_dat,value_label=="Zero value"), size = 0.2) + # make zero pts clearer by plotting in front
  scale_color_manual(values = c("darkgray", "red"), name="") +
  scale_x_continuous(breaks=seq(1950,2020,20)) + 
  scale_y_continuous(breaks=seq(0,60000,20000)) +
  facet_grid(System~Taxa)+
  theme_bw() +
  xlab("Year") +
  ylab("Population ID") +
  theme(strip.text = element_text(face="bold", size=12), 
        text = element_text(size=12), 
        legend.text = element_text(size=12), 
        legend.position = "top") + 
  guides(colour = guide_legend(override.aes = list(size=2)));zeros_strip_chart

ggsave(here("03_figures", "supp1_zeros_stripchart.png"), zeros_strip_chart, height=7, width=9)

## supplementary 2: completeness of time series (length/fullness)
completeness_plot <- ggarrange(completeness0_plot + rremove("xlab"), 
                               completeness25_plot + rremove("xlab") + rremove("ylab"), 
                               completeness50_plot, 
                               completeness75_plot + rremove("ylab"), 
                               labels="auto",common.legend=TRUE, font.label = list(size = 15));completeness_plot
ggsave(filename = "03_figures/supp2_completeness_plot.png",completeness_plot,width=7,height=7)

## supplementary 3: upper & lower extremes outlier removal
supp3_outlier <- ggarrange(high_trend_plot, 
                           low_trend_plot+rremove("ylab"), 
                           ncol=2, 
                           labels=c("a", "b"), 
                           font.label = list(size = 15));supp3_outlier

ggsave(here("03_figures", "supp3_outlier_removal.png"), supp3_outlier, height=5, width=9)



