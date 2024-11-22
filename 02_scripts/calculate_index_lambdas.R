calculate_index_lambdas_new = function(lambda_data, index = NULL, species_column_name="Binomial" , start_col_name ,end_col_name ) { # used the same as in LPIMain but sugest to change to start_year and end_year
  library(dplyr)
  library(collapse)
  
  if (!is.null(index)) {
    lambda_data = lambda_data[index, ]
  }
  
  # Rename species column to Binomial
  if(species_column_name!="Binomial"){
    lambda_data <- lambda_data |> rename(Binomial=species_column_name) 
  }
  
  # Calculate species lambdas - average of pop-lambdas per Binomial
  #sp_lambda = lambda_data |> 
  #  group_by(Binomial) |> 
  #  summarise_at(.vars = vars(start_col_name:end_col_name),
  #               .funs = mean, na.rm=T)

   sp_lambda <- lambda_data |> fgroup_by(Binomial) |> # version with fmean
     fmean() %>%
     select(-population_id)
  
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
  
  rownames(index) = str_replace(rownames(data.frame(index_lambda)),"[[:alpha:]]","") # remove Xs from column names when loading with read.csv
  
  #Add year column to results for plotting (usually just has rownames)
  index <- index |> rownames_to_column(var="year")
  
  return(index)
}
