### server.R ###

shinyServer(function(input, output, session) {
  output$zero_select <- renderUI({
    selectInput("zeros", "Select zero treatment:", choices = c("NA (C-LPI)" = "NA (C-LPI)",
                                                               "+ 1% of the mean" = "onepercent_mean",
                                                               "+ minimum value" = "minimum",
                                                               "+ 1" = "one",
                                                               "+ 0.000001" = "small_value",
                                                               "+ NA, NA, + 1% of the mean" = "na_na_onepercent",
                                                               "+ 1% of the mean, NA, + 1% of the mean" = "onepercent_na_onepercent"), multiple = TRUE)
  })
  output$ci_select <- renderUI({
    selectInput("conf_int", "Select credible interval type:", choices = c("Population" = "pop", 
                                                                          "Species" = "species",
                                                                          "Year" = "year"), multiple = TRUE)
  })
  output$length_select <- renderUI({
    selectInput("length", "Select time series length:", choices = unique(length_df$period), multiple = TRUE)
  })
  
  output$comp_select <- renderUI({
    selectInput("comp", "Select % of completeness:", choices = unique(completeness_df$type), multiple = FALSE)
  })
  
  output$num_select <- renderUI({
    selectInput("num_years", "Select number of data points:", choices = unique(completeness_df$n_points), multiple = TRUE)
  })
  
  
  output$model_select <- renderUI({
    selectInput("model_choices", "Select modelling choices:", choices = c("GAM" = "all_gams",
                                                                          "Linear (<6 points, C-LPI)" = "linear",
                                                                          "Log linear (<6 points)" = "log_linear"), multiple = TRUE)
  })
  output$outlier_select <- renderUI({
    selectInput("outliers", "Select outlier filters:", choices = c("0% (C-LPI)" = "0%",
                                                                   "5%" = "5%", 
                                                                   "10%" = "10%",
                                                                   "15%" = "15%"), multiple = TRUE)
  })
  output$weight_select <- renderUI({
    selectInput("weights", "Select weighting type:", choices = c("Unweighted (C-LPI)" = "unweighted", 
                                                                 "Weighted by taxa" = "weighted"), multiple = TRUE)
  })
  output$year_select <- renderUI({
    selectInput("base_year", "Select baseline year:", choices = unique(base_df$initial_year), multiple = TRUE)
  })
  
  # PLOT OUTPUTS
  plot_placeholder <- function(message) {
    ggplot() +
      annotate("text", x = 1, y = 1, label = "Please select an option above to populate the graph", size = 6) +
      xlim(0, 2) + ylim(0, 2) +
      theme_void()
  }
  
  output$length_plot <- renderUI({ plotOutput("p3")})
  output$p3 <- renderPlot({
    if (is.null(input$length) || length(input$length) == 0) return(plot_placeholder())
    
    length_df %>% 
      mutate(period = as.character(period)) %>% 
      filter(period %in% input$length) %>%
      ggplot(aes(x = year, y = LPI_final, color = period)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = period), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("5" = "#CA0020", "10" = "#F4A582", "15" = "#BABABA", "20" = "#404040"), guide = "none") +
      scale_fill_manual(values = c("5" = "#CA0020", "10" = "#F4A582", "15" = "#BABABA", "20" = "#404040")) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "length of time series (years)") +
      ylim(0.7, 1.3) +
      theme_classic()
  })
  
  output$comp_plot <- renderUI({ plotOutput("p2") })
  output$p2 <- renderPlot({
    if (is.null(input$comp) || is.null(input$num_years) || length(input$num_years) == 0) return(plot_placeholder())
    completeness_df %>% 
      filter(type %in% input$comp) %>%
      filter(n_points %in% input$num_years) |> 
      mutate(n_points = as.character(n_points)) %>%
      ggplot(aes(x = year, y = LPI_final, color = n_points)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = n_points), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("2" = "#D7191C", "3" = "#FDAE61", "6" = "#ABDDA4", "15" = "#2B83BA" ), guide = "none") +
      scale_fill_manual(values = c("2" = "#D7191C", "3" = "#FDAE61", "6" = "#ABDDA4", "15" = "#2B83BA" ), guide = "none") +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year") +
      ylim(0.7, 1.3) +
      theme_classic()
  })
  
  output$zero_plot <- renderUI({ plotOutput("p4") })
  output$p4 <- renderPlot({
    if (is.null(input$zeros) || length(input$zeros) == 0) {
      return(plot_placeholder())
    } else {
      zeros_df %>% filter(type %in% input$zeros) %>%
        ggplot(aes(x = year, y = LPI_final, color = type)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
        geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = type), alpha = 0.5) +
        geom_line() +
        scale_colour_manual(values = c(
          "NA (C-LPI)" = "#1B9E77FF",
          "minimum" = "#7570B3FF",
          "onepercent_mean" = "#D95F02FF",
          "one" = "#E7298AFF",
          "small_value" = "#66A61EFF",
          "na_na_onepercent" = "#E6AB02FF",
          "onepercent_na_onepercent" = "#A6761DFF"
        ), guide = "none") +
        scale_fill_manual(values = c(
          "NA (C-LPI)" = "#1B9E77FF",
          "minimum" = "#7570B3FF",
          "onepercent_mean" = "#D95F02FF",
          "one" = "#E7298AFF",
          "small_value" = "#66A61EFF",
          "na_na_onepercent" = "#E6AB02FF",
          "onepercent_na_onepercent" = "#A6761DFF"
        ), labels = c("NA (C-LPI)", 
                      "minimum value",
                      "+ 1% of mean",
                      "+ 1",
                      "+ 0.000001",
                      "+ NA, NA, + 1% of mean",
                      "+ 1% of the mean, NA, + 1% of mean")) +
        labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Treatment of Zeroes") +
        ylim(0.7, 1.3) +
        theme_classic()
    }
  })
  
  output$year_plot <- renderUI({ plotOutput("p5") })
  output$p5 <- renderPlot({
    if (is.null(input$base_year) || length(input$base_year) == 0) {
      return(plot_placeholder())
    } else {
    base_df %>% filter(initial_year %in% input$base_year) %>%
      mutate(initial_year = factor(initial_year)) %>%
      ggplot(aes(x = Year, y = LPI_final, color = initial_year)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = initial_year), alpha = 0.5) +
      geom_line() +
      scale_color_manual(values = c("1970" = "#7E1700", "1975" = "#995215", "1980" = "#AF7F2A",
                                    "1985" = "#C7B354", "1990" = "#CFE3A3", "1995" = "#A4E5D2",
                                    "2000" = "#5DC0D2", "2005" = "#3191C1", "2010" = "#2064AE",
                                    "2015" = "#023198"), guide = "none")+ 
      scale_fill_manual(values = c("1970" = "#7E1700", "1975" = "#995215", "1980" = "#AF7F2A",
                                   "1985" = "#C7B354", "1990" = "#CFE3A3", "1995" = "#A4E5D2",
                                   "2000" = "#5DC0D2", "2005" = "#3191C1", "2010" = "#2064AE",
                                   "2015" = "#023198")) +
      ylim(0.8, 1.2) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Baseline year choice") +
      theme_classic()
    }
  })
  
  output$ci_plot <- renderUI({ plotOutput("p6") })
  output$p6 <- renderPlot({
    if (is.null(input$conf_int) || length(input$conf_int) == 0) { 
      return(plot_placeholder(""))
    } else {
    credible_df %>% filter(type %in% input$conf_int) %>%
      ggplot(aes(x = year, y = LPI_final, color = type)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = type), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB"), guide = "none") +
      scale_fill_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB"), labels = c("Population",
                                                                                "Species (C-LPI)", 
                                                                                "Year")) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Type of credible interval") +
      ylim(0.7, 1.3) +
      theme_classic()
    }
  })
  
  output$model_plot <- renderUI({ plotOutput("p7") })
  output$p7 <- renderPlot({
    if (is.null(input$model_choices) || length(input$model_choices) == 0) {
      return(plot_placeholder())
    } else {
    modelling_df %>% filter(type %in% input$model_choices) %>%
      ggplot(aes(x = year, y = LPI_final, color = type)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = type), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("all_gams" = "#E31A1C", 
                                     "linear" = "#1F78B4", 
                                     "log_linear" = "#33A02C"), guide = "none") +
      scale_fill_manual(values = c("all_gams" = "#E31A1C", 
                                   "linear" = "#1F78B4", 
                                   "log_linear" = "#33A02C"), 
                        labels = c("GAM",
                                   "linear (<6 points, C-LPI)",
                                   "log linear (<6 points)")) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Modelling choices") +
      ylim(0.7, 1.3) +
      theme_classic()
    }
  })
  
  output$outlier_plot <- renderUI({ plotOutput("p8") })
  output$p8 <- renderPlot({
    if (is.null(input$outliers) || length(input$outliers) == 0) { 
      return(plot_placeholder())
    } else {
    outlier_df %>% filter(pct %in% input$outliers) %>%
      ggplot(aes(x = year, y = LPI_final, color = pct)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = pct), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("0%" = "#1B9E77FF", 
                                     "5%" = "#D95F02FF", 
                                     "10%" = "#7570B3FF", 
                                     "15%" = "#E7298AFF"), guide = "none") +
      scale_fill_manual(values = c("0%" = "#1B9E77FF", 
                                   "5%" = "#D95F02FF", 
                                   "10%" = "#7570B3FF", 
                                   "15%" = "#E7298AFF"),
                        labels = c("0% (C-LPI", "5%", "10%", "15%")) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "% of species lamda removals") +
      ylim(0.7, 1.3) +
      theme_classic()
    }
  })
  
  output$weight_plot <- renderUI({ plotOutput("p9") })
  output$p9 <- renderPlot({
    if (is.null(input$weights) || length(input$weights) == 0) { 
      return(plot_placeholder())
    } else {
    weight_df %>% filter(type %in% input$weights) %>%
      ggplot(aes(x = year, y = LPI_final, color = type)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = type), alpha = 0.5) +
      geom_line() +
      scale_colour_manual(values = c("unweighted" = "#1B9E77FF", 
                                     "weighted" = "#D95F02FF"), guide = "none") +
      scale_fill_manual(values = c("unweighted" = "#1B9E77FF", 
                                   "weighted" = "#D95F02FF"),
                        labels = c("Unweighted (C-LPI)", "Weighted by taxa")) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Weightings") +
      ylim(0.7, 1.3) +
      theme_classic()
    }
  })
  
})