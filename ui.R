### ui.R ###

library(shiny)
library(shinydashboard)

shinyUI(
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
        menuItem("Number of data points", tabName = "data_points", icon = icon("signal")),
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
                h2("Canadian Living Planet Index Data Explorer"),
                p("Tracking how wildlife is doing over time isn’t simple, but the Canadian Living Planet Index (C-LPI) helps by measuring changes in vertebrate population size. This metric uses 1970 as a baseline, which is assigned a value of 1.0. Over time, values above or below 1.0 indicate an increase or decrease, respectively, in the average monitored wildlife population abundance. Changes within ±0.05 of the baseline are considered stable."),
                br(),
                p("The C-LPI is used domestically to assess the relative change of average vertebrate abundance over time, and has been modified from its global counterpart — adopting differing methodological choices. However, there is no clear consensus on the most appropriate analytical methods, particularly as they pertain to the treatment of zeros, credible intervals and uncertainty, time series length and number of data points required, modelling of short time series, removal of outliers, weighting species, and the impact of baseline year selection. This application serves to transparently explore multiple methodological options for each of these decision points to improve transparency and accountability in reporting. The accompanying data and methodology underlying this application are available within our peer reviewed publication."),
                br(),
                p("Use the sidebar to navigate through different modules"),
                tags$ul(
                  tags$li("➗ View how the index changes based on how zero values are treated"),
                  tags$li("📊 Explore different ways of incorporating uncertainty"),
                  tags$li("🔢 Understand how the number of data points required influences overarching trends"),
                  tags$li("📉 Gain insight into different options for interopolating values (i.e., modelling)"),
                  tags$li("🔽 Evaluate the impact of outliers on the index"),
                  tags$li("🌿 Identify how weighting by species richness compares to equal weightings"),
                  tags$li("📅 Assess how trends change by selecting different reference (baseline) years")
                ),
                br(),
                p("Sincere thanks to all the individuals and organizations who have contributed data to the Canadian Living Planet Index. Your valuable efforts in monitoring, reporting and sharing biodiversity trends are essential for advancing the understanding of the state of wildlife in Canada and are instrumental in shaping a future where people and wildlife can thrive."),
                br(), 
                p("To learn more about the Canadian Living Planet Index and its underlying data, we encourage you to read our accompanying publications."),
                tags$a(href="https://www.wwf.ca", "Currie et al. 2025. Living Planet report Canada 2025 Technical Supplement, WWF-Canada"),
                br(),
                tags$a(href="https://www.wwf.ca", "Currie et al. 2025. Navigating methodological decisions: Balancing rigor and data volume of the Canadian Living Planet Index, FACETS"),
                br(),
                tags$a(href="https://www.facetsjournal.com/doi/10.1139/facets-2022-0063", "Currie et al. 2022. Assessing the representation of species included within the Canadian Living Planet Index, FACETS"),
                br(), 
                tags$div(
                  id = "footer-text",
                  "Emry, S. Ravoth, S. & Currie, J. 2025"
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
        tabItem(tabName = "data_points",
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

