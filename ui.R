ui <-  tagList(navbarPage(
  theme = "paper",
  "Living Planet Index",
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
  tabPanel("Dealing with Zeroes", 
           mainPanel(
             selectInput(
               "zeros",
               label = "How to deal with zeroes in time-series data",
               choices = unique(zero_option_data$zero_option),
               multiple = TRUE
             ),
             uiOutput("zero_plot")
           )
           ),
  tabPanel("Confidence Intervals", 
           mainPanel(
             selectInput(
               "conf_int",
               label = "Methods for calculating confidence intervals",
               choices = c("species", "population", "year"),
               multiple = TRUE
             ),
             uiOutput("ci_plot")
           ))
))