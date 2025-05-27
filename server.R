

server <- (function(input, output, session) {
  output$num_plot <- renderUI({
    plotOutput("p1")
  })
  
  output$p1 <- renderPlot({
    num_years <- req(input$num_years)
    
    p1 <-
      num_df %>% filter(cutoff %in% num_years) %>%
      ggplot(data = ., aes(x = year, y = LPI_final)) +
      geom_hline(yintercept = 1,
                 linetype = "dashed",
                 color = "black") +
      geom_line(aes(color = cutoff)) +
      geom_ribbon(aes(
        ymin = CI_low,
        ymax = CI_high,
        fill = cutoff
      ), alpha = 0.5) +
      scale_colour_manual(
        values = c(
          "at least 2" = "#D3E3CAFF",
          "at least 3" = "#BED6B3FF",
          "at least 6" = "#92A587FF",
          "at least 15" = "#4A5438FF"
        ),
        guide = "none"
      ) +
      scale_fill_manual(
        values = c(
          "at least 2" = "#D3E3CAFF",
          "at least 3" = "#BED6B3FF",
          "at least 6" = "#92A587FF",
          "at least 15" = "#4A5438FF"
        ),
        guide = "none"
      ) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year") +
      ylim(0.5, 1.55) +
      theme_classic()
    
    print(p1)
    
  })
  
  output$len_plot <- renderUI({
    plotOutput("p3")
  })
  
  output$p3 <- renderPlot({
    len <- req(input$len)
    
    p3 <-
      length_df %>% filter(cutoff %in% len) %>%
      mutate(cutoff = factor(cutoff, levels = c("at least 10", "at least 5", "at least 2"))) %>%
      ggplot(data = ., aes(x = year, y = LPI_final)) +
      geom_hline(yintercept = 1,
                 linetype = "dashed",
                 color = "black") +
      geom_line(aes(color = cutoff)) +
      geom_ribbon(aes(
        ymin = CI_low,
        ymax = CI_high,
        fill = cutoff
      ), alpha = 0.5) +
      scale_colour_manual(
        values = c(
          "at least 10" = "#0570b0",
          "at least 5" = "#74a9cf",
          "at least 2" = "#bdc9e1"
        ),
        guide = "none"
      ) +
      scale_fill_manual(
        values = c(
          "at least 10" = "#0570b0",
          "at least 5" = "#74a9cf",
          "at least 2" = "#bdc9e1"
        ),
        guide = "none"
      ) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year") +
      ylim(0.5, 1.55) +
      theme_classic()
    
    print(p3)
    
  })
  
  output$comp_plot <- renderUI({
    plotOutput("p2")
  })
  
  output$p2 <- renderPlot({
    comp <- req(input$comp)
    
    p2 <-
      completeness_df %>% filter(cutoff %in% comp) %>%
      mutate(cutoff = factor(cutoff, levels = c("75%", "50%", "25%"))) %>%
      ggplot(data = ., aes(x = year, y = LPI_final)) +
      geom_hline(yintercept = 1,
                 linetype = "dashed",
                 color = "black") +
      geom_line(aes(color = cutoff)) +
      geom_ribbon(aes(
        ymin = CI_low,
        ymax = CI_high,
        fill = cutoff
      ), alpha = 0.5) +
      scale_colour_manual(
        values = c(
          "75%" = "#6a51a3",
          "50%" = "#9e9ac8",
          "25%" = "#cbc9e2"
        ),
        guide = "none"
      ) +
      scale_fill_manual(
        values = c(
          "75%" = "#6a51a3",
          "50%" = "#9e9ac8",
          "25%" = "#cbc9e2"
        ),
        guide = "none"
      ) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year") +
      ylim(0.5, 1.55) +
      theme_classic()
    
    print(p2)
    
  })
  
  output$zero_plot <- renderUI({
    plotOutput("p4")
  })
  
  output$p4 <- renderPlot({
    zeros <- req(input$zeros)
    
    p4 <- 
      zeros_df %>% 
      filter(type %in% zeros) %>%
      ggplot(data = ., aes(x = year, y = LPI_final, color = type)) +
      geom_hline(yintercept = 1,
                 linetype = "dashed",
                 color = "black") +
      geom_line(aes(color = type)) +
      geom_ribbon(aes(
        ymin = CI_low,
        ymax = CI_high,
        fill = type
      ), alpha = 0.5) +
      scale_colour_manual(
        values = c(
          "minimum" = "#CC3D24FF",
          "onepercent_mean" = "#6DAE90FF",
          "one" = "#30B4CCFF",
          "small_value" = "#004F7AFF",
          "na_na_onepercent" = "#F3C558FF", 
          "onepercent_na_onepercent" = "slategrey"
        ),   
        labels = c("minimum value", "1% of the mean", "+ 1", "+ 0.0000001", "NA, NA, 1% of the mean", 
                   "1% of the mean, NA, 1% of the mean"),
        guide  = "none"
      ) +
      scale_fill_manual(
        values = c(
          "minimum" = "#CC3D24FF",
          "onepercent_mean" = "#6DAE90FF",
          "one" = "#30B4CCFF",
          "small_value" = "#004F7AFF",
          "na_na_onepercent" = "#F3C558FF", 
          "onepercent_na_onepercent" = "slategrey"
        ),   
        labels = c("minimum value", "1% of the mean", "+ 1", "+ 0.0000001", "NA, NA, 1% of the mean", 
                   "1% of the mean, NA, 1% of the mean")
      ) +
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Treatment of Zeroes") +
      # ylim(0.5, 1.55) +
      theme_classic()
    
    print(p4)
    
  })
  
  
  output$year_plot <- renderUI({
    plotOutput("p5")
  })
  
  output$p5 <- renderPlot({
    base_year <- req(input$base_year)
    
    p5 <- 
      base_df %>% 
      filter(initial_year %in% base_year) %>%
      mutate(initial_year = factor(initial_year)) |> 
      ggplot(aes(x = Year, y = LPI_final, color = initial_year)) +
      geom_hline(yintercept = 1,
                 linetype = "dashed",
                 color = "black") +
      geom_line(aes(color = initial_year)) +
      geom_ribbon(aes(
        ymin = CI_low,
        ymax = CI_high,
        fill = initial_year
      ), alpha = 0.5) +
      scale_colour_paletteer_d("MetBrewer::Hiroshige", guide  = "none") + 
      scale_fill_paletteer_d("MetBrewer::Hiroshige") + 
      labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Baseline year choice") +
      # ylim(0.5, 1.55) +
      theme_classic()
    
    print(p5)
  })
    
    output$ci_plot <- renderUI({
      plotOutput("p6")
    })
    
    output$p6 <- renderPlot({
      conf_int <- req(input$conf_int)
      
      p6 <- 
        credible_df %>% 
        filter(type %in% conf_int) %>%
        ggplot(aes(x = year, y = LPI_final, color = type)) +
        geom_hline(yintercept = 1,
                   linetype = "dashed",
                   color = "black") +
        geom_line(aes(color = type)) +
        geom_ribbon(aes(
          ymin = CI_low,
          ymax = CI_high,
          fill = type
        ), alpha = 0.5) +
        scale_colour_paletteer_d("MetBrewer::Hiroshige", guide  = "none") + 
        scale_fill_paletteer_d("MetBrewer::Hiroshige") + 

        labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Type of credible interval") +
        # ylim(0.5, 1.55) +
        theme_classic()
      
      print(p6)
      
    })
    
    output$model_plot <- renderUI({
      plotOutput("p7")
    })
    
    output$p7 <- renderPlot({
      model_choices <- req(input$model_choices)
      
      p7 <- 
        modelling_df %>% 
        filter(type %in% model_choices) %>%
        ggplot(aes(x = year, y = LPI_final, color = type)) +
        geom_hline(yintercept = 1,
                   linetype = "dashed",
                   color = "black") +
        geom_line(aes(color = type)) +
        geom_ribbon(aes(
          ymin = CI_low,
          ymax = CI_high,
          fill = type
        ), alpha = 0.5) +
        scale_colour_paletteer_d("MetBrewer::Hiroshige", guide  = "none") + 
        scale_fill_paletteer_d("MetBrewer::Hiroshige") + 
        
        labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Modelling choices") +
        # ylim(0.5, 1.55) +
        theme_classic()
      
      print(p7)
      
    })
    output$outlier_plot <- renderUI({
      plotOutput("p8")
    })
    
    output$p8 <- renderPlot({
      outliers <- req(input$outliers)
      
      p8 <- 
        outlier_df %>% 
        filter(pct %in% outliers) %>%
        ggplot(aes(x = year, y = LPI_final, color = pct)) +
        geom_hline(yintercept = 1,
                   linetype = "dashed",
                   color = "black") +
        geom_line(aes(color = pct)) +
        geom_ribbon(aes(
          ymin = CI_low,
          ymax = CI_high,
          fill = pct
        ), alpha = 0.5) +
        scale_colour_paletteer_d("MetBrewer::Hiroshige", guide  = "none") + 
        scale_fill_paletteer_d("MetBrewer::Hiroshige") + 
        
        labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Outlier removals") +
        # ylim(0.5, 1.55) +
        theme_classic()
      
      print(p8)
      
    })
    
    output$weight_plot <- renderUI({
      plotOutput("p9")
    })
    
    output$p9 <- renderPlot({
      weights <- req(input$weights)
      
      p9 <- 
        weight_df %>% 
        filter(type %in% weights) %>%
        ggplot(aes(x = year, y = LPI_final, color = type)) +
        geom_hline(yintercept = 1,
                   linetype = "dashed",
                   color = "black") +
        geom_line(aes(color = type)) +
        geom_ribbon(aes(
          ymin = CI_low,
          ymax = CI_high,
          fill = type
        ), alpha = 0.5) +
        scale_colour_paletteer_d("MetBrewer::Hiroshige", guide  = "none") + 
        scale_fill_paletteer_d("MetBrewer::Hiroshige") + 
        
        labs(y = "Living Planet Index (1970 = 1)", x = "Year", fill = "Outlier removals") +
        # ylim(0.5, 1.55) +
        theme_classic()
      
      print(p9)
      
    })
})
