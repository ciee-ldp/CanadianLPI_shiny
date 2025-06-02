### ui.R ###

library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
  includeCSS("www/style.css"),
  dashboardPage(
    dashboardHeader(title = "Living Planet Index"),
    dashboardSidebar(
      # Add logo image
      tags$img(src = "logo.jpg", height = "100px", style = "display: block; margin-left: auto; margin-right: auto;"),
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "Home", icon = icon("home")),
        menuItem("Treatment of Zeros", tabName = "Zeros", icon = icon("circle")),
        menuItem("Uncertainty", tabName = "Uncertainty", icon = icon("exclamation-triangle")),
        menuItem("Time series length", tabName = "length", icon = icon("clock")),
        menuItem("Time series completeness", tabName = "completeness", icon = icon("signal")),
        menuItem("Modelling", tabName = "Modelling", icon = icon("project-diagram")),
        menuItem("Outliers", tabName = "Outliers", icon = icon("chart-line")),
        menuItem("Weighting", tabName = "Weighting", icon = icon("balance-scale")),
        menuItem("Baseline Year", tabName = "Baseline_Year", icon = icon("calendar"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      tabItems(
        tabItem(tabName = "Home",
                h2("Welcome to the Living Planet Index Explorer"),
                p("This is a companion app to a manuscript recently submitted to FACETS. It is designed to helps users explore different analytical decisions when calculating the Canadian Living Planet Index (C-LPI), a biodiversity indicator."),
                p("Use the sidebar to navigate through different modules"),
                tags$ul(
                  tags$li("📉 View how the C-LPI changes based on how zeros and outliers in the dataset are dealt with"),
                  tags$li("🧪 Explore different ways of incorporating uncertainty into the C-LPI"),
                  tags$li("⏱️ Understand how temporal representation in the data affect trends, i.e. gaps in a population's time series, the number of data points and overall temporal range of a time series, and modelling decisions around dealing with short time series"),
                  tags$li("🌿 Identify how weighting by species richness (global LPI) compares to the unweighted C-LPI"),
                  tags$li("📅 Compare how trends change by selecting different reference (baseline) years")
                ),
                br(),
                p("Click on any menu item to get started."),
                br(), br(),
                tags$div(
                  id = "footer-text",
                  "This app was developed by Dr. Sandra Emry, with data compiled by Sarah Ravoth"
                )
        ),
        tabItem(tabName = "Zeros",
                h2("Treatment of zeros"),
                p("Mathematically, a number cannot be divided by zero. In order to address population counts of zero in the analysis, it is possible to either treat zeros as missing values or add a small quantity to zeros for mathematical purposes. Of the options explored to date, most have little impact on the final index value of the C-LPI, though one option produces a more stable trend and one, more negative."),
                uiOutput("zero_select"),
                uiOutput("zero_plot")
        ),
        tabItem(tabName = "Uncertainty",
                h2("Evaluating uncertainty"),
                p("he credible intervals include the range of indices that can be fit into the existing dataset, capturing the variability within the data. They do not incorporate the uncertainty associated with population counts of individual studies. The credible intervals are multiplicative and increase in width over time as the uncertainty of previous years are inherited by the rest of the trend. The credible intervals around the final index value represent uncertainty around that value in relation to the baseline. Similarly, the final index value reported is relative to the baseline value in 1970. The C-LPI adopts an approach that produces the widest credible intervals to more fully capture the uncertainty in producing indices of relative abundance."),
                uiOutput("ci_select"),
                uiOutput("ci_plot")
        ),
        tabItem(tabName = "length",
                h2("Time series length"),
                p("add text"),
                uiOutput("length_select"),
                uiOutput("length_plot")
        ),
        tabItem(tabName = "completeness",
                h2("Number of data points required"),
                p("The global LPI requires a minimum of two data points per population time series to evaluate an overall trend in abundance, while the C-LPI requires three (the more data, the more robust the trend). There does not appear to be a discernable pattern among the number of data points required, with fewer (two or three) showcasing a 10 per cent decline, a requirement of six producing a stable trend, but then a requirement of 15 plummeting back to a decline."),
                uiOutput("comp_select"),
                uiOutput("num_select"),
                uiOutput("comp_plot")
        ),
        tabItem(tabName = "Modelling",
                h2("Model interpolation"),
                p("Different mathematical options exist for interpolating values between the start and end year of a population time series. Of the options explored to date, they have little impact on the final index value of the C-LPI."),
                uiOutput("model_select"),
                uiOutput("model_plot")
        ),
        tabItem(tabName = "Outliers",
                h2("Acknowledging outliers"),
                p("Removing extreme values (those that are considered extreme increases or decreases in abundance) has no effect on the final index value (always a 10 per cent decline) but does help refine the credible intervals so that they no longer cross the baseline value in 1970."),
                uiOutput("outlier_select"),
                uiOutput("outlier_plot")
        ),
        tabItem(tabName = "Weighting",
                h2("Consideration of weighting"),
                p("Let’s assume we have two population time series for one species. One time series accounts for 90 per cent of the population and shows a drastic decline, whereas the other time series accounts for 10 per cent of the population and exhibits a slight increase. The C-LPI weights both series equally to give an average trend in abundance for the species, thereby masking the fact that most of the population is in decline. Another option is to proportionally weight the index based on species richness by taxonomic group in Canada. However, both approaches yield a similar final index value."),
                uiOutput("weight_select"),
                uiOutput("weight_plot")
        ),
        tabItem(tabName = "Baseline_Year",
                h2("Selection of baseline"),
                p("When assessing relative abundance, the choice of a baseline year impacts the final index value. Because the C-LPI unveils consistent, incremental declines over time, the choice of baseline year marginally affects the final index value — with a longer time frame yielding a more negative value compared to a shorter time frame for analysis. Notably, less data is included when the time frame is shortened."),
                uiOutput("year_select"),
                uiOutput("year_plot")
        )
      )
    )
  )
)
)

