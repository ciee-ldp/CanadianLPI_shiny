library(tidyverse)


# Zeros -------------------------------------------------------------------


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

df5 <- read_csv("./01_outdata/zeros/zeros_option5_CIspp.csv")
df5 <- rename(df5, "year" = `...1`)
df5$type <- "na_na_onepercent"

df6 <- read_csv("./01_outdata/zeros/zeros_option5_CIspp.csv")
df6 <- rename(df6, "year" = `...1`)
df6$type <- "onepercent_na_onepercent"

zeros_df <- rbind(df1, df2, df3, df4, df5, df6)

write_csv(zeros_df, "./01_outdata/zeros/zeros_df.csv")


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

write_csv(modelling_df, "./01_outdata/zeros/modelling_df.csv")


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

write_csv(credible_df, "./01_outdata/zeros/credible_df.csv")


# Baseline years - linear ----------------------------------------------------------


