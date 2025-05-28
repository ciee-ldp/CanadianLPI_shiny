### ui.R ###

library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
  includeCSS("www/style.css"),
  dashboardPage(
    dashboardHeader(title = "Living Planet Index"),
    dashboardSidebar(
      sidebarMenu(
        # HTML(paste0(
        #   "<br>",
        #   "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='US-NationalParkService-Logo.svg' width = '186'></a>",
        #   "<br>",
        #   "<p style = 'text-align: center;'><small><a href='https://www.nps.gov/subjects/hfc/arrowhead-artwork.htm' target='_blank'>NPS logo disclaimer</a></small></p>",
        #   "<br>"
        # )),
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

