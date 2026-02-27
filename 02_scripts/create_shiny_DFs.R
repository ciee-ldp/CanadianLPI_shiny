library(tidyverse)


# Zeros -------------------------------------------------------------------

df0 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-species.csv")
df0 <- df0 |> 
  rename("CI_low" = Lower_CI,
         "CI_high" = Upper_CI,
         "LPI_final" = Index)
df0$type <- "NA (C-LPI)"

df1 <- read_csv("./01_outdata/zeros/zeros_option2_CIspp.csv")
df1 <- rename(df1, "year" = `...1`)
df1$type <- "onepercent_mean"

df2 <- read_csv("./01_outdata/zeros/zeros_option3_CIspp.csv")
df2 <- rename(df2, "year" = `...1`)
df2$type <- "minimum"

df3 <- read_csv("./01_outdata/zeros/zeros_option4_CIspp.csv")
df3 <- rename(df3, "year" = `...1`)
df3$type <- "one"

df4 <- read_csv("./01_outdata/zeros/zeros_option5_CIspp.csv")
df4 <- rename(df4, "year" = `...1`)
df4$type <- "small_value"

df5 <- read_csv("./01_outdata/zeros/zeros_option6_CIspp.csv")
df5 <- rename(df5, "year" = `...1`)
df5$type <- "na_na_onepercent"

df6 <- read_csv("./01_outdata/zeros/zeros_option7_CIspp.csv")
df6 <- rename(df6, "year" = `...1`)
df6$type <- "onepercent_na_onepercent"

zeros_df <- rbind(df0, df1, df2, df3, df4, df5, df6)

write_csv(zeros_df, "./01_outdata/zeros/zeros_df.csv")
save(zeros_df, file = "./01_outdata/shiny/zeros_df.Rda")


# Modelling ---------------------------------------------------------------


df1 <- read_csv("./01_outdata/modelling/all-gam-sppCIs.csv")
df1 <- rename(df1, "year" = `...1`)
df1$type <- "all_gams"

df2 <- read_csv("./01_outdata/modelling/short-linear-sppCIs.csv")
df2 <- rename(df2, "year" = `...1`)
df2$type <- "linear"

df3 <- read_csv("./01_outdata/modelling/short-loglinear-sppCIs.csv")
df3 <- rename(df3, "year" = `...1`)
df3$type <- "log_linear"

modelling_df <- rbind(df1, df2, df3)

write_csv(modelling_df, "./01_outdata/modelling/modelling_df.csv")
save(modelling_df, file = "./01_outdata/shiny/modelling_df.Rda")


# Credible intervals ------------------------------------------------------

df1 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-population.csv")
df1 <- df1 |> rename("CI_high" = "Upper_CI",
                    "CI_low" = "Lower_CI",
                    "LPI_final" = "Index")
df1$type <- "pop"

df2 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-species.csv")
df2 <- df2 |> rename("CI_high" = "Upper_CI",
                     "CI_low" = "Lower_CI",
                     "LPI_final" = "Index")
df2$type <- "species"

df3 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-year.csv")
df3$type <- "year"
df3 <- df3 |> select(-`...1`) |> 
  select(year, CI_low, CI_high, LPI_final, type)

credible_df <- rbind(df1, df2, df3)

write_csv(credible_df, "./01_outdata/credible_intervals/credible_df.csv")
save(credible_df, file = "./01_outdata/shiny/credible_df.Rda")


# Weights ----------------------------------------------------------------
df1 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-species.csv")
df1 <- df1 |> rename("CI_high" = "Upper_CI",
                     "CI_low" = "Lower_CI",
                     "LPI_final" = "Index")
df1$type <- "unweighted"
df2 <- read_csv("./01_outdata/weighted/weighted_lpi_sppCI_data.csv") |> 
  rename(year = `...1`)
df2$type <- "weighted"

weight_df <- rbind(df1, df2)

write_csv(weight_df, "./01_outdata/weighted/weight_df.csv")
save(weight_df, file = "./01_outdata/shiny/weight_df.Rda")


# Baseline year -----------------------------------------------------------

base_df <- read_csv("./01_outdata/baseline_years/CLPI_baseline-years_sppCIs.csv")
save(base_df, file = "01_outdata/shiny/base_df.Rda")

# Outliers ----------------------------------------------------------------

df1 <- read_csv("./01_outdata/credible_intervals/CLPI_CIs-by-species.csv")
df1 <- df1 |> rename("CI_high" = "Upper_CI",
                     "CI_low" = "Lower_CI",
                     "LPI_final" = "Index")
df1$pct <- "0%"

df2<- read_csv("./01_outdata/outliers/upr_lwr_outliers_CIspp.csv") |> 
  select(year, CI_low, CI_high, LPI_final, pct)

outlier_df <- rbind(df1, df2)

save(outlier_df, file = "01_outdata/shiny/outlier_df.Rda")


# Completeness ------------------------------------------------------------
df1 <- read_csv("./01_outdata/completeness/complete0_CIspp.csv") |> 
  rename(year = `...1`)
df2 <- read_csv("./01_outdata/completeness/complete25_CIspp.csv")
df3 <- read_csv("./01_outdata/completeness/complete50_CIspp.csv")
df4 <- read_csv("./01_outdata/completeness/complete75_CIspp.csv")

# Number and completeness of data points ---------------------------------------------------

df1 <- read_csv("01_outdata/completeness/complete0_CIspp.csv") |> 
  rename(year = `...1`)
df1$year <- rep(seq(1970,2022),4)
df1$type = "0%"

df2 <- read_csv("01_outdata/completeness/complete25_CIspp.csv") |> 
  rename(year = `...1`)
df2$type = "25%"
df2$year <- rep(seq(1970,2022),4)

df3 <- read_csv("01_outdata/completeness/complete50_CIspp.csv") |> 
  rename(year = `...1`)
df3$type = "50%"
df3$year <- rep(seq(1970,2022),4)

df4 <- read_csv("01_outdata/completeness/complete75_CIspp.csv") |> 
  rename(year = `...1`)
df4$type = "75%"
df4$year <- rep(seq(1970,2022),4)

completeness_df <- rbind(df1, df2, df3, df4)
save(completeness_df, file = "01_outdata/shiny/completeness_df.Rda")

# Length of time series ---------------------------------------------------

length_df <- read_csv("./01_outdata/period/period_CIspp.csv") |> 
  rename(year = `...1`)

# fixing an error in the years
length_df$year <- rep(seq(1970,2022),4)

save(length_df, file = "01_outdata/shiny/length_df.Rda")

