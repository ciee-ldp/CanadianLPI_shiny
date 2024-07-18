

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
  
})
