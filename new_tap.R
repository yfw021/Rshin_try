  # --- NEW: Tab 5: Gap Analysis ---
  tabPanel("Gap Analysis", icon = icon("balance-scale"),
    fluidPage(
      titlePanel("Gap Analysis: Firm Capacity vs. Demand"),
      p("This section compares the total projected firm capacity against the total forecasted demand to identify potential future shortfalls or surpluses (the 'gap')."),
      selectInput("gap_region", "Select Region:",
                  choices = c("Texas", "New Mexico", "System"), selected = "System"),
      plotlyOutput("gap_plot"),
      hr(),
      h3("Gap Analysis Data"),
      DT::dataTableOutput("gap_table")
    )
  ),
  # --- END OF NEW TAB ---

  # --- NEW: Tab 5 Gap Analysis Logic ---
  
  # Reactive expression to get the right columns based on selection
  selected_gap_data <- reactive({
    req(input$gap_region)
    
    if (input$gap_region == "Texas") {
      data <- gap_analysis_data %>% select(Years, Capacity = Firm_Capa_TX, Demand = Total_Demand_TX, Gap = Gap_TX)
    } else if (input$gap_region == "New Mexico") {
      data <- gap_analysis_data %>% select(Years, Capacity = Firm_Capa_NM, Demand = Total_Demand_NM, Gap = Gap_NM)
    } else { # System
      data <- gap_analysis_data %>% select(Years, Capacity = Firm_Capa_SY, Demand = Total_Demand_SY, Gap = Gap_SY)
    }
    
    # Rename columns for clarity in the table
    data %>% rename("Year" = Years, "Total Firm Capacity (MW)" = Capacity, "Total Demand (MW)" = Demand, "Gap (MW)" = Gap)
  })
  
  # Gap Plot
  output$gap_plot <- renderPlotly({
    plot_data <- selected_gap_data() %>%
      pivot_longer(
        cols = c("Total Firm Capacity (MW)", "Total Demand (MW)"),
        names_to = "Metric",
        values_to = "Value"
      )
    
    p <- ggplot(plot_data, aes(x = Year, y = Value, color = Metric, group = Metric)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_color_manual(values = c("Total Firm Capacity (MW)" = "#00A651", "Total Demand (MW)" = "#F27F03")) +
      labs(
        title = paste(input$gap_region, "Firm Capacity vs. Demand"),
        y = "Megawatts (MW)",
        x = "Year",
        color = ""
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p)
  })
  
  # Gap Table
  output$gap_table <- DT::renderDataTable({
    table_data <- selected_gap_data()
    
    datatable(table_data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle(
        'Gap (MW)',
        color = styleInterval(c(0), c('red', 'green'))
      )
  })
  # --- END OF NEW LOGIC ---
