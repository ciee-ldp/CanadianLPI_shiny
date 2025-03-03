#### weighted C-LPI bootstrap
#### run locally & in parallel process
#### authors: sarah ravoth, louise mcrae, robin freeman

# purpose: script to bootstrap weighted LPI index by species. this script was sent to 
# canada computing because this process is computationally intensive (estimated run
# time for 10,000 bootstraps is 100 hours). 

# <><><><><> step 0: load libraries <><><><><>
library(dplyr)
library(tidyverse)
library(rlpi)
library(purrr)
library(here)

here() # check path is correct

# <><><><><> step 1: read in data and tidy <><><><><>
cad <- read.csv(file=here("00_data", "CAD_Paper_withzeroes.csv"), na.strings="NULL", header=TRUE)  %>%
  mutate(X2009 = as.character(X2009)) %>%             # set from numeric to character so following line works
  mutate(across(X1970:X2022, ~na_if(., "Null"))) %>%  # replace any "Null" to NA 
  rename(original_id = ID) %>%      # set "ID" to original_id, and make a new ID corresponding to rownumber to avoid issues
  mutate(ID = row_number()) %>% 
  rename(Taxa = Taxonomic_group) %>%                      # rename this column to Taxa
  mutate(Taxa = case_when(Taxa=="Mammalia" ~ "Mammals",   # rename these redundant labels
                          Taxa=="Reptilia" ~ "Herps", 
                          TRUE ~ Taxa))

# Data to exclude: Finding the time series that are only NA
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

# Data to exclude: Finding the time series that only have zero values
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

# clean data--remove time series with only zeroes or nulls
cad <- cad %>% 
  filter(!ID %in% zeroids) %>% # remove populations that are only zeros
  filter(!ID %in% nullids)  # remove populations that are only NULL

# change all zeros to NA in population abundance columns (ie. X1970-X2022 values)
cad <- cad %>% 
  mutate(case_when(if_any(X1970:X2022, ~. ==0) ~ NA, 
                   TRUE ~ .))  # set all zeros to NA

# join binomial spp name to pop_lambdas
cad.names <- cad %>% 
  dplyr::select(ID, Binomial) %>% 
  rename(population_id = ID)


####################### first try non-parallel process to make sure everything works ####################### 

sp = unique(cad$Binomial)
boot_indices = list()
N_BOOT = 5

set.seed(9204)

for(i in 1:N_BOOT) {
  tryCatch({
    print(sprintf("Processing sample %d", i))
    
    # Sample speceis names from the species list with replacement
    sampled_species <- sample(sp, length(sp), replace = TRUE)
    
    # Count how many of each species we've sampled
    sampled_counts <- tibble(Binomial = sampled_species) %>%
      count(Binomial, name = "count")
    
    # Using this count to get the data for each species from the data frame
    # The right number of times
    # But also rename pop IDs appropriately
    sampled_data <- cad %>%
      inner_join(., sampled_counts, by = "Binomial") %>%   # Filter for species in sample_counts list
      group_by(Binomial) %>%                            # And for each species
      slice(rep(row_number(), times = count)) %>%       # Get each species data 'count' times
      mutate(ID = paste0(ID, "_", row_number())) %>%    # Added to make 'new' duplicated populations unique
      ungroup() %>%
      select(-count)                                    # Remove the count column
    
    ###
    ## subset original LPI data into each weightings group
    ###
    # subset by taxa
    Taxa = sampled_data$Taxa
    Aves <- Taxa == 'Aves'
    Mammalia <- Taxa == 'Mammals'
    Fishes <- Taxa == 'Fish'
    Herps <- Taxa == 'Herps'
    
    ###
    ## create infiles for each system & taxa subset
    # RF: I've needed to append Sys.getpid() to the infile name to stop the parallel processes from trying to access the same files
    ###
    aves_index <- create_infile(sampled_data, index_vector=Aves, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted", "aves"))
    mammalia_index <- create_infile(sampled_data, index_vector=Mammalia, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","mammalia"))
    herps_index <- create_infile(sampled_data, index_vector=Herps, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","herps"))
    fishes_index <- create_infile(sampled_data, index_vector=Fishes, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted","fishes"))
    
    
    ####
    # RF: Need to sort these flags out - only with save_plots = 0 and plot_lpi = 0 do we not have things trying to plot
    ####
    # Similarly, if BOOT_STRAP_SIZE is 1 and CI_FLAG is true, we get CI2 errors
    ####
    # this input works: "./01_outdata/weighted/global-weightings-taxa-local.txt"
    # this DOESN'T work: "~/01_outdata/weighted/global-weightings-taxa-local.txt"
    # this DOESN'T work: here("01_outdata", "weighted", "global-weightings-taxa-local.txt")
    w_boot_lpi <- LPIMain("./01_outdata/weighted/global-weightings-taxa-local.txt",
                    BOOT_STRAP_SIZE = 1,  # we only want to sample once per spp pool subset--the real resampling occurs via for loop
                    use_weightings = 1,              # to generate weighted LPI
                    #use_weightings_B = 0,
                    LINEAR_MODEL_SHORT_FLAG = 1,     # flag for CAD process
                    DATA_LENGTH_MIN = 3,             # include only time-series with >2 data points
                    VERBOSE=TRUE,
                    PLOT_MAX = 2022,
                    SHOW_PROGRESS = FALSE,
                    CI_FLAG = FALSE,     # don't calculate CIs bootstrapped by year
                    save_plots = 0,      # don't save plots
                    plot_lpi = 0,        # don't plot LPI trend
                    basedir = ".", 
                    force_recalculation = 1)
    
    #write.csv(boot_lpi_df, sprintf("../../boot_output/boot_index_%05d.csv", i))
    boot_indices[[i]] = w_boot_lpi
    
  }, error = function(e) {
    print("Error while processing sample")
    print(e)
    boot_indices[[i]] = NULL
  })
}


boot_indices # check
# working ok!


######################### now try in parallel ######################### 

#### unweighted (rob's original code) #### 

sp = unique(cad$Binomial)
boot_indices = list()
N_BOOT = 10

library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)

clusterCall(cl,function() {
  library(tibble)
  library(rlpi)
  library(dplyr)
  
})


boot_indices = foreach(i=1:N_BOOT, .combine='cbind', .errorhandling = 'remove') %dopar% { 
  print(sprintf("Processing sample %d", i))
  
  # Sample speceis names from the species list with replacement
  sampled_species <- sample(sp, length(sp), replace = TRUE)
  
  # Count how many of each species we've sampled
  sampled_counts <- tibble(Binomial = sampled_species) %>%
    count(Binomial, name = "count")
  
  # Using this count to get the data for each species from the data frame
  # The right number of times
  # But also rename pop IDs appropriately
  sampled_data <- cad %>%
    inner_join(., sampled_counts, by = "Binomial") %>%   # Filter for species in sample_counts list
    group_by(Binomial) %>%                            # And for each species
    slice(rep(row_number(), times = count)) %>%       # Get each species data 'count' times
    mutate(ID = paste0(ID, "_", row_number())) %>%    # Added to make 'new' duplicated populations unique
    ungroup() %>%
    select(-count)                                    # Remove the count column
  
  ###
  ## create infile
  # RF: I've needed to append Sys.getpid() to the infile name to stop the parallel processes from trying to access the same files
  ###
  infile_boot <- create_infile(sampled_data,start_col_name = "X1970",
                               end_col_name = "X2022",
                               name = sprintf("LDP_20241123_boot_%05d", i), 
                               CUT_OFF_YEAR = 1970)
  
  parallel_test <- LPIMain(infile_boot,
                  BOOT_STRAP_SIZE = 1,  # we only want to sample once per spp pool subset--the real resampling occurs via for loop
                  use_weightings = 1,              # to generate weighted LPI
                  #use_weightings_B = 0,
                  LINEAR_MODEL_SHORT_FLAG = 1,     # flag for CAD process
                  DATA_LENGTH_MIN = 3,             # include only time-series with >2 data points
                  VERBOSE=TRUE,
                  PLOT_MAX = 2022,
                  SHOW_PROGRESS = FALSE,
                  CI_FLAG = FALSE,     # don't calculate CIs bootstrapped by year
                  save_plots = 0,      # don't save plots
                  plot_lpi = 0,        # don't plot LPI trend
                  basedir = ".", 
                  force_recalculation = 1)
  
  
  boot_lpi_df = data.frame(parallel_test)
  return(boot_lpi_df)
  #print(sprintf("[%d] Writing index to file", i))
  #write.csv(boot_lpi_df, sprintf("./boot_output/boot_index_%05d.csv", i))
}

stopCluster(cl)

boot_indices
# it works!
# 1.5 mins to do 10 loops on 5 cores
# 2.5 mins to do 10 loops on 10 cores, so faster to do more on fewer?
# 2 mins to do 10 loops on 8 cores



#### weighted #### 

# not working--runs ok outside of the forloop, but when i execute the forloop it doesn't run
# suggests that there's an issue inside teh for loop??

library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)

clusterCall(cl,function() {
  library(tibble)
  library(rlpi)
  library(dplyr)
  
})

sp = unique(cad$Binomial)
boot_indices2 = list()
N_BOOT = 5
taxa_weightings <- read.table(here("01_outdata", "weighted", "global-weightings-taxa-local.txt"), header = TRUE, sep = "\t")

boot_indices2 = foreach(i = 1:N_BOOT, .combine = 'cbind', .errorhandling = 'remove') %dopar% { 
  print(sprintf("Processing sample %d", i))
 
  # Sample speceis names from the species list with replacement
  sampled_species <- sample(sp, length(sp), replace = TRUE)
  
  # Count how many of each species we've sampled
  sampled_counts <- tibble(Binomial = sampled_species) %>%
    count(Binomial, name = "count")
  
  # Using this count to get the data for each species from the data frame
  # The right number of times
  # But also rename pop IDs appropriately
  sampled_data <- cad %>%
    inner_join(., sampled_counts, by = "Binomial") %>%   # Filter for species in sample_counts list
    group_by(Binomial) %>%                            # And for each species
    slice(rep(row_number(), times = count)) %>%       # Get each species data 'count' times
    mutate(ID = paste0(ID, "_", row_number())) %>%    # Added to make 'new' duplicated populations unique
    ungroup() %>%
    select(-count)    
  
  ###
  ## subset original LPI data into each weightings group
  ###
  # subset by taxa
  Taxa = sampled_data$Taxa
  Aves <- Taxa == 'Aves'
  Mammalia <- Taxa == 'Mammals'
  Fishes <- Taxa == 'Fish'
  Herps <- Taxa == 'Herps'
  
  ###
  ## create infiles for each system & taxa subset
  # RF: I've needed to append Sys.getpid() to the infile name to stop the parallel processes from trying to access the same files
  ###
  aves_index <- create_infile(sampled_data, 
                              index_vector=Aves, 
                              start_col_name = "X1970",
                              end_col_name = "X2022", 
                              CUT_OFF_YEAR = 1970, 
                              name = here("01_outdata", "weighted", sprintf("aves_%05d", i))) 
  mammalia_index <- create_infile(sampled_data, index_vector=Mammalia, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name = here("01_outdata", "weighted",sprintf("mammalia_%05d", i)))
  herps_index <- create_infile(sampled_data, index_vector=Herps, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted",sprintf("herps_%05d", i)))
  fishes_index <- create_infile(sampled_data, index_vector=Fishes, start_col_name = "X1970",end_col_name = "X2022", CUT_OFF_YEAR = 1970, name=here("01_outdata", "weighted",sprintf("fishes_%05d", i)))
  
  ####
  # modify weightings file to reflect bootstrap iteration
  ####
  taxa_weightings[grepl("aves", taxa_weightings$FileName), ] <- str_replace( taxa_weightings[grepl("aves", taxa_weightings$FileName), ], "aves", sprintf("aves_%05d", i))
  taxa_weightings[grepl("mammalia", taxa_weightings$FileName), ] <- str_replace(taxa_weightings[grepl("mammalia", taxa_weightings$FileName), ], "mammalia", sprintf("mammalia_%05d", i))
  taxa_weightings[grepl("herps", taxa_weightings$FileName), ] <- str_replace(taxa_weightings[grepl("herps", taxa_weightings$FileName), ], "herps", sprintf("herps_%05d", i))
  taxa_weightings[grepl("fishes", taxa_weightings$FileName), ] <- str_replace(taxa_weightings[grepl("fishes", taxa_weightings$FileName), ], "fishes", sprintf("fishes_%05d", i))
  
  # i don't really want to have to write a file for every itr....is there a way to run it just using an R object??
  write_tsv(taxa_weightings, 
            file=here("01_outdata", "weighted", paste0("global-weightings-taxa-local", sprintf("_%05d", i), ".txt"))
  )
  #global_infile <- here("01_outdata", "weighted", paste0("global-weightings-taxa-local", sprintf("_%05d", i), ".txt"))
  
  ####
  # RF: Need to sort these flags out - only with save_plots = 0 and plot_lpi = 0 do we not have things trying to plot
  ####
  # Similarly, if BOOT_STRAP_SIZE is 1 and CI_FLAG is true, we get CI2 errors
  ####
  # this input works: "./01_outdata/weighted/global-weightings-taxa-local.txt"
  # this DOESN'T work: "~/01_outdata/weighted/global-weightings-taxa-local.txt"
  # this DOESN'T work: here("01_outdata", "weighted", "global-weightings-taxa-local.txt")
  
  w_parallel <- LPIMain(paste0("./01_outdata/weighted/", paste0("global-weightings-taxa-local", sprintf("_%05d", i), ".txt")),
                        BOOT_STRAP_SIZE = 1,  # we only want to sample once per spp pool subset--the real resampling occurs via for loop
                        use_weightings = 1,              # to generate weighted LPI
                        #use_weightings_B = 0,
                        LINEAR_MODEL_SHORT_FLAG = 1,     # flag for CAD process
                        DATA_LENGTH_MIN = 3,             # include only time-series with >2 data points
                        VERBOSE=TRUE,
                        PLOT_MAX = 2022,
                        SHOW_PROGRESS = FALSE,
                        CI_FLAG = FALSE,     # don't calculate CIs bootstrapped by year
                        save_plots = 0,      # don't save plots
                        plot_lpi = 0,        # don't plot LPI trend
                        basedir = ".", 
                        force_recalculation = 1)
  
  
  boot_lpi_df = data.frame(w_parallel)
  return(boot_lpi_df)
  #print(sprintf("[%d] Writing index to file", i))
  #write.csv(boot_lpi_df, sprintf("./boot_output/boot_index_%05d.csv", i))
}

stopCluster(cl)
boot_indices2

# does not run (ie defaults immediately to null)
# boot_indices2 returned as NULL



################################################# stop here :) #################################################

# <><><><><> step 3: tidy up outputs <><><><><>

boot_indices_df <- do.call(cbind, boot_indices) %>% 
  as.data.frame() %>%    # convert to df to name cols based on iterations
  set_names(c(1:N_BOOT)) # set col names based on bootstrap iteration

boot_indices_df <- as.matrix(boot_indices_df) # now back to matrix (much more efficient)

CI_out <- t(apply(X=boot_indices_df, MARGIN=1, FUN=quantile, c(0.025, 0.975))) # calculate lower & upper 95% CIs rowwise

# <><><><><> step 4: save outputs <><><><><>

write.csv(boot_indices_df, file=here("weighted_boot_indices.csv"), row.names=FALSE)
write.csv(CI_out, file=here("CIs_weighted_boot.csv"), row.names=FALSE)
write.csv(cad.names, file=here("names-test.csv"), row.names=FALSE)


