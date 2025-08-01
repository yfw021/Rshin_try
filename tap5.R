# --- SERVER LOGIC ---
server <- function(input, output) {

  # === TAB 2: INSTALLED CAPACITY LOGIC ===
  
  # Reactive expression to generate plot data. This is efficient as it only
  # recalculates when its inputs (the filters) change.
  plot_data_reactive <- reactive({
    req(input$year_slider) # Ensure slider has a value before proceeding
    
    # Filter by generic status
    filtered <- capacity_df
    if (input$generic_filter != "all") {
      filtered <- filtered %>% filter(Generic_resources == input$generic_filter)
    }
    
    # Filter by type
    if (!("All" %in% input$type_filter)) {
      filtered <- filtered %>% filter(type %in% input$type_filter)
    }

    # Project capacity for each year in the selected range
    # Create a grid of every resource and every year, then filter for active ones
    tidyr::crossing(Generation_Resources = filtered$Generation_Resources, Year = seq(input$year_slider[1], input$year_slider[2])) %>%
      left_join(filtered, by = "Generation_Resources") %>%
      filter(Year >= First_Year, Year <= Last_Year) %>%
      group_by(Year, type) %>%
      summarise(Total_Capacity = sum(Capacity, na.rm = TRUE), .groups = 'drop')
  })
  
  # Render the capacity bar plot
  output$capacity_plot <- renderPlot({
    plot_df <- plot_data_reactive()
    ggplot(plot_df, aes(x = Year, y = Total_Capacity, fill = type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Projected Installed Capacity by Type", x = "Year", y = "Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
  })

  # Reactive expression for map/table data (generators active at the end of the range)
  map_table_data_reactive <- reactive({
    req(input$year_slider)
    end_year <- input$year_slider[2]
    
    # Filter by generic status
    filtered <- capacity_df
    if (input$generic_filter != "all") {
      filtered <- filtered %>% filter(Generic_resources == input$generic_filter)
    }
    
    # Filter by type
    if (!("All" %in% input$type_filter)) {
      filtered <- filtered %>% filter(type %in% input$type_filter)
    }

    # Filter for generators active in the final year of the slider
    filtered %>% filter(First_Year <= end_year, Last_Year >= end_year)
  })

  # Render the map
  output$capacity_map <- renderLeaflet({
    map_df <- map_table_data_reactive()
    leaflet(data = map_df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~sqrt(Capacity) / 2, # Scale radius by capacity
        color = ~colorFactor("viridis", type)(type),
        popup = ~paste0("<strong>", Generation_Resources, "</strong><br>",
                         "Type: ", type, "<br>",
                         "Capacity: ", Capacity, " MW")
      )
  })
  
  # Render the data table
  output$capacity_table <- DT::renderDataTable({
    DT::datatable(map_table_data_reactive(), options = list(pageLength = 5))
  })
  
  
  # === TAB 3: ALLOCATION CAPACITY LOGIC ===
  
  output$allocation_plot <- renderPlot({
    plot_df <- dashboard_df %>%
      group_by(Years, type)
    
    if (input$category_selector_alloc == "Texas") {
      plot_df <- plot_df %>% summarise(Total_Sum = sum(TX_LR_sum), .groups = 'drop')
      title_text <- "Texas Allocated Capacity"
    } else if (input$category_selector_alloc == "New Mexico") {
      plot_df <- plot_df %>% summarise(Total_Sum = sum(NM_LR_sum), .groups = 'drop')
      title_text <- "New Mexico Allocated Capacity"
    } else { # System
      plot_df <- plot_df %>% summarise(Total_Sum = sum(TX_LR_sum + NM_LR_sum), .groups = 'drop')
      title_text <- "System Allocated Capacity"
    }
    
    ggplot(plot_df, aes(x = Years, y = Total_Sum, fill = type)) +
      geom_area() +
      labs(title = title_text, x = "Year", y = "Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
  })
  
  # Render the static tables (data is already pre-computed)
  output$texas_alloc_table <- DT::renderDataTable({ DT::datatable(tx_alloc_table_data, options = list(scrollX = TRUE)) })
  output$nm_alloc_table <- DT::renderDataTable({ DT::datatable(nm_alloc_table_data, options = list(scrollX = TRUE)) })
  output$system_alloc_table <- DT::renderDataTable({ DT::datatable(system_alloc_table_data, options = list(scrollX = TRUE)) })
  
  # === TAB 4: FIRM CAPACITY LOGIC ===
  # This follows the exact same pattern as Tab 3, just with different columns.
  
  output$firm_capacity_plot <- renderPlot({
    plot_df <- dashboard_df %>%
      group_by(Years, type)
      
    if (input$category_selector_firm == "Texas") {
      plot_df <- plot_df %>% summarise(Total_Sum = sum(Firm_capa_TX), .groups = 'drop')
      title_text <- "Texas Firm Capacity"
    } else if (input$category_selector_firm == "New Mexico") {
      plot_df <- plot_df %>% summarise(Total_Sum = sum(Firm_capa_NM), .groups = 'drop')
      title_text <- "New Mexico Firm Capacity"
    } else { # System
      plot_df <- plot_df %>% summarise(Total_Sum = sum(Firm_capa_TX + Firm_capa_NM), .groups = 'drop')
      title_text <- "System Firm Capacity"
    }
    
    ggplot(plot_df, aes(x = Years, y = Total_Sum, fill = type)) +
      geom_area() +
      labs(title = title_text, x = "Year", y = "Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
  })
  
  # Render the static firm capacity tables
  output$texas_firm_table <- DT::renderDataTable({ DT::datatable(tx_firm_table_data, options = list(scrollX = TRUE)) })
  output$nm_firm_table <- DT::renderDataTable({ DT::datatable(nm_firm_table_data, options = list(scrollX = TRUE)) })
  output$system_firm_table <- DT::renderDataTable({ DT::datatable(system_firm_table_data, options = list(scrollX = TRUE)) })
}

# --- RUN THE APP ---
shinyApp(ui = ui, server = server)
