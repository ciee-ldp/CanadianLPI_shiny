### ui.R ###

library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
  includeCSS("www/style.css"),
  dashboardPage(
    dashboardHeader(title = "Living Planet Index"),
    dashboardSidebar(
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
      # tags$head(
      #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      # ),
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
                p("add text"),
                uiOutput("zero_select"),
                uiOutput("zero_plot")
        ),
        tabItem(tabName = "Uncertainty",
                h2("Calculation of credible intervals"),
                p("add text"),
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
                h2("Time series completeness"),
                p("add text"),
                uiOutput("comp_select"),
                uiOutput("num_select"),
                uiOutput("comp_plot")
        ),
        tabItem(tabName = "Modelling",
                h2("How to handle short time series"),
                p("add text"),
                uiOutput("model_select"),
                uiOutput("model_plot")
        ),
        tabItem(tabName = "Outliers",
                h2("Outlier removal"),
                p("add text"),
                uiOutput("outlier_select"),
                uiOutput("outlier_plot")
        ),
        tabItem(tabName = "Weighting",
                h2("To weight or not to weight"),
                p("add text"),
                uiOutput("weight_select"),
                uiOutput("weight_plot")
        ),
        tabItem(tabName = "Baseline_Year",
                h2("Baseline year selection"),
                p("add text"),
                uiOutput("year_select"),
                uiOutput("year_plot")
        )
      )
    )
  )
)
)

