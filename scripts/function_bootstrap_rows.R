# This is a function that calculates the confidence intervals created from bootstraping
# by rows. See documentation in LPI-sensitivity_Group3_calculate_index_lambas.Rmd
# Date: Jun 7/2024
# Written by: Maria Isabel Arce-Plata (modified from the original file boostrap_species)

bootstrap_by_rows <- function(lambda_data, species_column_name="Binomial" , start_col_name ,end_col_name, iter=TRUE , N=100){
  library(dplyr)
  library(collapse)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(ggplot2)
  
  #function that calculates the index from the lambda data and returns a dataframe with the year and the index
  calculate_index_lambdas = function(lambda_data, index = NULL, species_column_name="Binomial" , start_col_name ,end_col_name ) { # used the same as in LPIMain but sugest to change to start_year and end_year
    
    if (!is.null(index)) {
      lambda_data = lambda_data[index, ]
    }
    
    # Rename species column to Binomial
    if(species_column_name!="Binomial"){
      lambda_data <- lambda_data |> rename(Binomial=species_column_name) 
    }

    years <- names(lambda_data)
    year_range <- years[grep(start_col_name,years):grep(end_col_name,years)]
      
    # Calculate species lambdas - average of pop-lambdas per Binomial
    # sp_lambda = lambda_data |> 
    #   group_by(Binomial) |> 
    #   summarise_at(.vars = vars(start_col_name:end_col_name),
    #                .funs = mean, na.rm=T)
    sp_lambda <- lambda_data |> select(Binomial,contains(year_range)) |> fgroup_by(Binomial) |> # version with fmean
      fmean()
    
    # sp_lambda = data.frame(sp_lambda)
    
    # convert NaNs to NAs
    sp_lambda <- sp_lambda |> mutate(across(start_col_name:end_col_name, ~na_if(., NaN)))
    
    # get total column mean
    index_lambda <- sp_lambda |> select(-Binomial) |> fmean() # does it need na.rm or something?
    
    #print(index_lambda)
    # Calculate index
    index_lambda[start_col_name]=0 # Make baseline year == 0
    
    index = cumprod(10^index_lambda)
    
    index = data.frame(index)
    
    # remove Xs from column names when loading with read.csv
    rownames(index) = str_replace(rownames(data.frame(index_lambda)),"[[:alpha:]]","") 
    
    #Add year column to results for plotting (usually just has rownames)
    index <- index |> rownames_to_column(var="year")
    
    return(index)
  }
  
  # To just calculate the index from lambda values
  if(iter==FALSE){
    # Calculate index from lambdas and plot
    lambda_index <- calculate_index_lambdas(lambda_data= lambda_data, #species_column_name="SpeciesSSet",
                                                start_col_name = start_col_name, end_col_name = end_col_name)
    
    # plot index values
    g_new_index = ggplot(lambda_index) + geom_line(aes(x=year, y=index), group=1, linewidth=1) + ylim(0, 5)+
      theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw() 
    
    # return index and plot
    return(list(data=lambda_index,plot=g_new_index))
  
  # To sample from lambda_data N times, select sample_idx rows and measure the confidence intervals  
  }else{
    
    # Calculate index from lambdas and plot
    lambda_index <- calculate_index_lambdas(lambda_data= lambda_data, #species_column_name="SpeciesSSet",
                                                start_col_name = start_col_name, end_col_name = end_col_name)
    
    # Empty list for results
    boot_indices_new = list()
    for (i in 1:N) {
      print(sprintf("Iteration number: %d", i))
      # Create sample of rows numbers (populations) to include
      sample_idx = sample(1:nrow(lambda_data), size = nrow(lambda_data), replace = T)
      
      # Calculate index using sample vector
      b_lambda_index = calculate_index_lambdas(lambda_data, index = sample_idx,#species_column_name="SpeciesSSet",
                                                   start_col_name = start_col_name, end_col_name = end_col_name)
      
      # Transpose single column of results into single row
      boot_indices_new[[i]] = b_lambda_index
    }
    
    # Combined list of rows into one data frame with calumn names having the ID of the iteration
    boot_indices_df = boot_indices_new |> setNames(1:N) |> 
      bind_rows(.id="name") |> arrange(year)
    
    # Plot bootstrapped indices with transparency
    g_boot = ggplot(boot_indices_df) + 
      geom_line(aes(x=year, y=index, group=name), linewidth=1, alpha=0.08) + 
      ylim(0, 2) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    # Get mean and 95% intervals
    intervals_boot_indices_nested <- boot_indices_df |> nest_by(year) |>  
      mutate(quantiles = list(
        quantile(data$index, probs = c(0.025, 0.975)) |> setNames(c("Lower_CI","Upper_CI")) |> bind_rows()), # get quantiles and set names
        mean_lpi = list(mean(data$index))) # measure mean within years
    
    intervals_boot_indices_df <- intervals_boot_indices_nested |> 
      unnest(c(quantiles,mean_lpi)) |> select(-data) |> data.frame() |> ungroup() |> mutate(Index=lambda_index$index)
    
    # plot intervals
    g = ggplot(intervals_boot_indices_df) + 
      geom_ribbon(aes(x=year, y=Index, ymax = Upper_CI, ymin = Lower_CI,group=1), alpha = 0.5, fill="blue") +
      geom_line(aes(x=year, y=Index,group=1), linewidth=1, color="black") + 
      scale_x_discrete(breaks= seq(min(intervals_boot_indices_df$year),max(intervals_boot_indices_df$year),10)) +
      ylim(0, 2) + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    return(list(bootstrap_data=boot_indices_df,interval_data=intervals_boot_indices_df,plot=g))
    
  }
  
}





