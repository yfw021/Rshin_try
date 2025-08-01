# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# L&R (Load and Resources) Dashboard in R Shiny
# Author: Gemini
# Date: July 31, 2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

# 2. MOCK DATA GENERATION ----
# In a real-world scenario, you would load your data here, e.g., using read.csv()
# IMPORTANT: Replace this section with your actual data loading code.

# --- Mock Data for Tab 2: capaciti_install_generic ---
set.seed(42)
capaciti_install_generic <- tibble(
  Generation_Resources = paste("Generator", LETTERS[1:20]),
  Latitude = runif(20, 31.7, 36.5),
  Longitude = runif(20, -108, -103),
  Capacity = sample(50:500, 20, replace = TRUE),
  First_Year = sample(2010:2028, 20, replace = TRUE),
  Last_Year = sample(2045:2055, 20, replace = TRUE),
  type = sample(c("gas", "solar", "dr", "wind", "battery", "btmsolar"), 20, replace = TRUE),
  Generic_resources = sample(0:1, 20, replace = TRUE)
)

# Pre-process data for Tab 2: Create a long-format dataframe for yearly projection
capacity_by_year <- capaciti_install_generic %>%
  crossing(Year = 2026:2050) %>%
  mutate(Projected_Capacity = ifelse(Year >= First_Year & Year <= Last_Year, Capacity, 0)) %>%
  select(Year, Generation_Resources, Latitude, Longitude, Projected_Capacity, type, Generic_resources, First_Year, Last_Year)

# --- Mock Data for Tab 3 & 4: dashboard_final ---
set.seed(123)
# Creating a base structure
dashboard_final_base <- expand_grid(
  Generation_Resources = paste("Plant", 1:15),
  Years = 2026:2050
)

# Joining details to the base
dashboard_final <- dashboard_final_base %>%
  left_join(
    tibble(
      Generation_Resources = paste("Plant", 1:15),
      First_Year = sample(2015:2027, 15, replace = TRUE),
      Last_Year = sample(2048:2055, 15, replace = TRUE),
      type = sample(c("gas", "solar", "wind", "battery"), 15, replace = TRUE),
      Category = sample(c("Texas", "New Mexico"), 15, replace = TRUE),
      L_R_Summary = sample(c("1.1 RIOGRANDE", "3.2. BATTERY STORAGE", "2.1 RENEWABLE PPAs - Existing"), 15, replace = TRUE)
    ),
    by = "Generation_Resources"
  ) %>%
  mutate(
    # Simulate values only for active years
    base_val = runif(n(), 10, 80),
    is_active = ifelse(Years >= First_Year & Years <= Last_Year, 1, 0),
    TX_LR_sum = ifelse(Category == "Texas", base_val * is_active, 0),
    NM_LR_sum = ifelse(Category == "New Mexico", base_val * is_active, 0),
    Firm_capa_TX = TX_LR_sum * runif(n(), 0.4, 0.8),
    Firm_capa_NM = NM_LR_sum * runif(n(), 0.3, 0.7)
  ) %>%
  mutate(
    SY_LR_sum = TX_LR_sum + NM_LR_sum,
    Firm_capa_SY = Firm_capa_TX + Firm_capa_NM,
    Category = "System" # This column isn't used for filtering but for context
  )


# 3. UI (USER INTERFACE) ----
ui <- navbarPage(
  title = "L&R Dashboard",
  theme = shinythemes::shinytheme("lumen"),
  
  # -- Tab 1: Introduction --
  tabPanel("Introduction",
    icon = icon("info-circle"),
    fluidPage(
      h1("Load and Resources Analysis Tool", align = "center"),
      hr(),
      fluidRow(
        column(6,
          h3("About This Tool"),
          p("This dashboard provides a comprehensive overview of our company's Load and Resources (L&R) portfolio. It allows users to explore installed and projected capacity, analyze resource allocation across different jurisdictions, and review firm capacity outlooks through 2050."),
          p("Use the tabs at the top to navigate between different analyses.")
        ),
        column(6,
          h3("Vocabulary"),
          tags$ul(
            tags$li(tags$strong("Load:"), " The amount of electrical power demanded by customers at any given time."),
            tags$li(tags$strong("Resources:"), " The generation assets (power plants, batteries, etc.) available to meet the load."),
            tags$li(tags$strong("Installed Capacity:"), " The maximum output (in Megawatts, MW) that a generating unit is rated to produce."),
            tags$li(tags$strong("Firm Capacity:"), " The capacity that can be relied upon to be available during peak load hours. This is often a derated value of installed capacity, especially for intermittent resources like solar and wind."),
            tags$li(tags$strong("PPA (Power Purchase Agreement):"), " A contract to buy electricity from a third-party generator.")
          )
        )
      ),
      hr(),
      h2("Meet the Team", align = "center"),
      p("The dedicated team behind the L&R analysis.", align="center"),
      br(),
      # Team layout: 3 rows of 3
      fluidRow(
        lapply(1:3, function(i) {
          column(4, align = "center",
            # Use placeholder images. Replace with your actual image files in the 'www' folder.
            tags$img(src = paste0("team", i, ".png"), height = "120px", width = "120px", style = "border-radius: 50%; object-fit: cover;"),
            h4(paste("Team Member", i)),
            p(em("Role / Short Bio"))
          )
        })
      ),
      br(),
      fluidRow(
        lapply(4:6, function(i) {
          column(4, align = "center",
            tags$img(src = paste0("team", i, ".png"), height = "120px", width = "120px", style = "border-radius: 50%; object-fit: cover;"),
            h4(paste("Team Member", i)),
            p(em("Role / Short Bio"))
          )
        })
      ),
      br(),
      fluidRow(
        lapply(7:9, function(i) {
          column(4, align = "center",
            tags$img(src = paste0("team", i, ".png"), height = "120px", width = "120px", style = "border-radius: 50%; object-fit: cover;"),
            h4(paste("Team Member", i)),
            p(em("Role / Short Bio"))
          )
        })
      )
    )
  ),
  
  # -- Tab 2: Installed Capacity --
  tabPanel("Installed Capacity",
    icon = icon("bolt"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Filters"),
        sliderInput("year_range_tab2", "Select Year Range:",
                    min = 2026, max = 2050, value = c(2026, 2050), sep = ""),
        selectInput("type_filter_tab2", "Filter by Resource Type:",
                    choices = c("All", unique(capaciti_install_generic$type)),
                    selected = "All"),
        radioButtons("generic_filter_tab2", "Filter by Generic Resources:",
                     choices = c("All", "Yes" = 1, "No" = 0),
                     selected = "All")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(6,
            h4("Generator Map"),
            leafletOutput("capacity_map", height = "400px")
          ),
          column(6,
            h4("Projected Capacity by Type (MW)"),
            plotlyOutput("capacity_plot", height = "400px")
          )
        ),
        hr(),
        fluidRow(
          column(12,
            h4("Filtered Data"),
            DTOutput("capacity_table")
          )
        )
      )
    )
  ),
  
  # -- Tab 3: Allocation Capacity --
  tabPanel("Allocation Capacity",
    icon = icon("chart-pie"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Jurisdiction"),
        selectInput("category_select_tab3", "View Allocation For:",
                    choices = c("Texas", "New Mexico", "System"),
                    selected = "System")
      ),
      mainPanel(
        width = 9,
        h4(textOutput("allocation_plot_title")),
        plotlyOutput("allocation_plot"),
        hr(),
        h4("Allocation Summary Tables (MW)"),
        tabsetPanel(
          tabPanel("Texas", DTOutput("alloc_table_tx")),
          tabPanel("New Mexico", DTOutput("alloc_table_nm")),
          tabPanel("System", DTOutput("alloc_table_sy"))
        )
      )
    )
  ),
  
  # -- Tab 4: Firm Capacity --
  tabPanel("Firm Capacity",
    icon = icon("thumbs-up"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Select Jurisdiction"),
        selectInput("category_select_tab4", "View Firm Capacity For:",
                    choices = c("Texas", "New Mexico", "System"),
                    selected = "System")
      ),
      mainPanel(
        width = 9,
        h4(textOutput("firm_plot_title")),
        plotlyOutput("firm_plot"),
        hr(),
        h4("Firm Capacity Summary Tables (MW)"),
        tabsetPanel(
          tabPanel("Texas", DTOutput("firm_table_tx")),
          tabPanel("New Mexico", DTOutput("firm_table_nm")),
          tabPanel("System", DTOutput("firm_table_sy"))
        )
      )
    )
  ),

  # -- Tab 5: FAQ --
  tabPanel("FAQ",
    icon = icon("question-circle"),
    fluidPage(
      h2("Frequently Asked Questions"),
      hr(),
      h4("Q1: Where does the data for this dashboard come from?"),
      p("A: The data is sourced from the corporate integrated resource planning database, which includes asset information, retirement schedules, and contractual obligations."),
      hr(),
      h4("Q2: How is 'Firm Capacity' calculated?"),
      p("A: Firm capacity is calculated by applying a specific derating factor (or 'Effective Load Carrying Capability' - ELCC) to the installed capacity of each resource. This factor varies by resource type; for example, solar has a lower factor than a natural gas plant due to its intermittency."),
      hr(),
      h4("Q3: Can I download the data from the tables?"),
      p("A: Yes. The tables in the dashboard include buttons to export the data in various formats like CSV, Excel, or PDF.")
    )
  )
)

# 4. SERVER LOGIC ----
server <- function(input, output, session) {

  # === TAB 2: INSTALLED CAPACITY LOGIC ===
  
  # Reactive expression for filtered data based on UI controls
  filtered_data_tab2 <- reactive({
    data <- capacity_by_year %>%
      filter(
        Year >= input$year_range_tab2[1],
        Year <= input$year_range_tab2[2]
      )
    
    if (input$type_filter_tab2 != "All") {
      data <- data %>% filter(type == input$type_filter_tab2)
    }
    
    if (input$generic_filter_tab2 != "All") {
      data <- data %>% filter(Generic_resources == input$generic_filter_tab2)
    }
    
    data
  })
  
  # Reactive expression for map data (unique locations active in the time range)
  map_data <- reactive({
    active_generators <- capacity_by_year %>%
      filter(
        Year >= input$year_range_tab2[1],
        Year <= input$year_range_tab2[2],
        Projected_Capacity > 0
      ) %>%
      pull(Generation_Resources) %>%
      unique()
      
    data <- capaciti_install_generic %>%
        filter(Generation_Resources %in% active_generators)

    if (input$type_filter_tab2 != "All") {
      data <- data %>% filter(type == input$type_filter_tab2)
    }
    
    if (input$generic_filter_tab2 != "All") {
      data <- data %>% filter(Generic_resources == input$generic_filter_tab2)
    }

    data
  })

  # Render the Leaflet map
  output$capacity_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = min(capaciti_install_generic$Longitude), lat1 = min(capaciti_install_generic$Latitude),
        lng2 = max(capaciti_install_generic$Longitude), lat2 = max(capaciti_install_generic$Latitude)
      )
  })

  # Observe changes and update the map markers
  observe({
    df <- map_data()
    leafletProxy("capacity_map", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste(
          "<b>", Generation_Resources, "</b><br>",
          "Type:", type, "<br>",
          "Capacity:", Capacity, "MW"
        )
      )
  })

  # Render the capacity plot
  output$capacity_plot <- renderPlotly({
    plot_data <- filtered_data_tab2() %>%
      group_by(Year, type) %>%
      summarise(Total_Capacity = sum(Projected_Capacity), .groups = 'drop')
      
    p <- ggplot(plot_data, aes(x = Year, y = Total_Capacity, fill = type, text = paste("Type:", type, "<br>Capacity:", round(Total_Capacity,1), "MW"))) +
      geom_bar(stat = "identity") +
      labs(title = NULL, x = "Year", y = "Projected Capacity (MW)") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
      
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # Render the data table
  output$capacity_table <- renderDT({
    datatable(
      filtered_data_tab2() %>% select(Year, Generation_Resources, type, Projected_Capacity, Generic_resources),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE,
      filter = 'top'
    )
  })

  # === TAB 3: ALLOCATION CAPACITY LOGIC ===

  # Reactive data for the plot
  allocation_plot_data <- reactive({
    req(input$category_select_tab3)
    
    if (input$category_select_tab3 == "Texas") {
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(TX_LR_sum), .groups = 'drop')
    } else if (input$category_select_tab3 == "New Mexico") {
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(NM_LR_sum), .groups = 'drop')
    } else { # System
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(SY_LR_sum), .groups = 'drop')
    }
    df
  })
  
  # Render plot title
  output$allocation_plot_title <- renderText({
    paste("Allocated Capacity for", input$category_select_tab3)
  })

  # Render the allocation plot
  output$allocation_plot <- renderPlotly({
    p <- ggplot(allocation_plot_data(), aes(x = Years, y = CapacitySum, fill = type)) +
      geom_area(alpha = 0.8) +
      labs(x = "Year", y = "Allocated Capacity (MW)", fill = "Resource Type") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
      
    ggplotly(p)
  })
  
  # Prepare and render the static allocation tables
  # Function to reshape data
  prepare_wide_table <- function(data, value_col) {
    data %>%
      select(L_R_Summary, Years, !!sym(value_col)) %>%
      group_by(L_R_Summary, Years) %>%
      summarise(Value = sum(!!sym(value_col)), .groups = 'drop') %>%
      pivot_wider(names_from = Years, values_from = Value, values_fill = 0)
  }
  
  output$alloc_table_tx <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "TX_LR_sum"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })
  output$alloc_table_nm <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "NM_LR_sum"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })
  output$alloc_table_sy <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "SY_LR_sum"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })

  # === TAB 4: FIRM CAPACITY LOGIC ===
  
  # Reactive data for the plot
  firm_plot_data <- reactive({
    req(input$category_select_tab4)
    
    if (input$category_select_tab4 == "Texas") {
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(Firm_capa_TX), .groups = 'drop')
    } else if (input$category_select_tab4 == "New Mexico") {
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(Firm_capa_NM), .groups = 'drop')
    } else { # System
      df <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(CapacitySum = sum(Firm_capa_SY), .groups = 'drop')
    }
    df
  })
  
  # Render plot title
  output$firm_plot_title <- renderText({
    paste("Firm Capacity for", input$category_select_tab4)
  })

  # Render the firm capacity plot
  output$firm_plot <- renderPlotly({
    p <- ggplot(firm_plot_data(), aes(x = Years, y = CapacitySum, fill = type)) +
      geom_area(alpha = 0.8) +
      labs(x = "Year", y = "Firm Capacity (MW)", fill = "Resource Type") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
      
    ggplotly(p)
  })

  # Prepare and render the static firm capacity tables
  output$firm_table_tx <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "Firm_capa_TX"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })
  output$firm_table_nm <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "Firm_capa_NM"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })
  output$firm_table_sy <- renderDT({
    datatable(prepare_wide_table(dashboard_final, "Firm_capa_SY"), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE))
  })

}

# 5. RUN THE APP ----
shinyApp(ui = ui, server = server)
