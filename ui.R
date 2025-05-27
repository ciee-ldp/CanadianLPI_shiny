ui <-  tagList(navbarPage(
  theme = "paper",
  "Living Planet Index",
  tabPanel("Treatment of Zero Values", 
           mainPanel(
             selectInput(
               "zeros",
               label = "How to deal with zeroes in time-series data",
               choices = unique(zeros_df$type),
               # choices = c("1% mean", "+minimum", "+1", "+0.000001", "NA, NA, +1% mean", "+1% mean, NA, +1% mean"),
               multiple = TRUE
             ),
             uiOutput("zero_plot")
           )
  ),
  
  tabPanel("Credible Intervals", 
           mainPanel(
             selectInput(
               "conf_int",
               label = "Methods for calculating credible intervals",
               choices = unique(credible_df$type),
               multiple = TRUE
             ),
             uiOutput("ci_plot")
           )),
  
  tabPanel("Temporal Representation",
           # sidebarPanel(
           #   selectInput(
           #     "num_data_pts", "Number of Years Surveyed", c("at least 2", "at least 3", "at least 6", "at least 15"),
           #     multiple = FALSE
           #   )
           # ),
           mainPanel(
             tabsetPanel(
               tabPanel(
                 "Number of Years Surveyed",
                 selectInput(
                   "num_years",
                   label = "Number of years surveyed",
                   choices = unique(num_df$cutoff),
                   multiple = TRUE
                 ),
                 uiOutput("num_plot") # depends on input
               ),
               tabPanel(
                 "Length",
                 selectInput(
                   "len",
                   label = "Length of time series",
                   choices = unique(length_df$cutoff),
                   multiple = TRUE
                 ),
                 uiOutput("len_plot")
               ),
               tabPanel(
                 "Completeness",
                 selectInput(
                   "comp",
                   label = "Completeness of time series",
                   choices = unique(completeness_df$cutoff),
                   multiple = TRUE
                 ),
                 uiOutput("comp_plot")
               )
             )
           )),
  
  tabPanel("Modelling of short time series", 
           mainPanel(
             selectInput(
               "model_choices",
               label = "Choosing how to model short time series",
               choices = unique(modelling_df$type),
               multiple = TRUE
             ),
             uiOutput("model_plot")
           )),
  
  tabPanel("Outliers", 
           mainPanel(
             selectInput(
               "outliers",
               label = "Removal of outliers",
               choices = unique(outlier_df$pct),
               multiple = TRUE
             ),
             uiOutput("outlier_plot")
           )),
  
  tabPanel("Weightings", 
           mainPanel(
             selectInput(
               "weights",
               label = "Whether to weight the C-LPI",
               choices = unique(weight_df$type),
               multiple = TRUE
             ),
             uiOutput("weight_plot")
           )),
  
  tabPanel("Baseline year", 
           mainPanel(
             selectInput(
               "base_year",
               label = "Selection of the baseline year",
               choices = unique(base_df$initial_year),
               multiple = TRUE
             ),
             uiOutput("year_plot")
           ))
))