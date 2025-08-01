# 1. LOAD LIBRARIES ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

# 2. LOAD & PRE-PROCESS DATA ----



# Pre-process 'Installed Capacity' data: Create a long-format dataset for projections
projected_capacity_long <- capaciti_install_generic %>%
  crossing(Year = 2026:2050) %>%
  mutate(
    Projected_Capacity = ifelse(Year >= First_Year & Year <= Last_Year, Capacity, 0)
  )

# Pre-process 'Allocation' and 'Firm' data for System view
dashboard_final_system <- dashboard_final %>%
  mutate(
    System_Alloc_Capa = TX_LR_sum + NM_LR_sum,
    System_Firm_Capa = Firm_capa_TX + Firm_capa_NM
  )

# Define color palette for resource types
type_colors <- c(
  "gas" = "#F27F03", 
  "solar" = "#F9E200", 
  "dr" = "#A64A99", 
  "wind" = "#00AEEF", 
  "battery" = "#00A651", 
  "btmsolar" = "#F7B538"
)

# 3. UI (USER INTERFACE) ----
ui <- navbarPage(
  title = "L&R Dashboard",
  theme = shinythemes::shinytheme("lumen"),
  
  # --- Tab 1: Introduction ---
  tabPanel("Introduction",
           icon = icon("info-circle"),
           fluidPage(
             titlePanel("Load & Resource Planning Tool"),
             h3("Introduction"),
             p("This dashboard provides an interactive platform for analyzing Load and Resource (L&R) data. It allows users to explore installed capacity projections, allocation scenarios, and firm capacity assessments for the Texas, New Mexico, and System-wide electricity grids."),
             

             
             h3("Meet the Team"),
             p("This tool was developed by a dedicated team of analysts and developers."),
             fluidRow(
               # Create a column for each team member. Repeat this block for all 9 members.
               column(4,
                      tags$img(src = "team1.png", width = "100px", height = "100px", style="border-radius: 50%;"),
                      h5("Team Member 1"),
                      p("Role & Short Bio. Lorem ipsum dolor sit amet.")
               ),
               column(4,
                      tags$img(src = "team2.png", width = "100px", height = "100px", style="border-radius: 50%;"),
                      h5("Team Member 2"),
                      p("Role & Short Bio. Consectetur adipiscing elit.")
               ),
               column(4,
                      tags$img(src = "team3.png", width = "100px", height = "100px", style="border-radius: 50%;"),
                      h5("Team Member 3"),
                      p("Role & Short Bio. Sed do eiusmod tempor.")
               )
               # ... Add more columns for members 4-9
             )
           )
  ),
  
  # --- Tab 2: L&R Installed Capacity ---
  tabPanel("Installed Capacity",
           icon = icon("bolt"),
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Filters"),
               sliderInput("year_range", "Select Year Range:",
                           min = 2026, max = 2050, value = c(2026, 2035), sep = ""),
               selectInput("type_filter", "Filter by Resource Type:",
                           choices = c("All", unique(capaciti_install_generic$type))),
               radioButtons("generic_filter", "Filter by Generic Resources:",
                            choices = c("All" = "all", "Yes" = "1", "No" = "0"), selected = "all")
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 column(7, plotlyOutput("capacity_plot", height = "400px")),
                 column(5, leafletOutput("map", height = "400px"))
               ),
               hr(),
               DT::dataTableOutput("capacity_table")
             )
           )
  ),
  
  # --- Tab 3: Allocation Capacity ---
  tabPanel("Allocation Capacity",
           icon = icon("chart-pie"),
           fluidPage(
             titlePanel("Resource Allocation Capacity"),
             selectInput("alloc_region", "Select Region:",
                         choices = c("Texas", "New Mexico", "System"), selected = "System"),
             plotlyOutput("allocation_plot"),
             hr(),
             h3("Allocation Summary Tables (MW)"),
             tabsetPanel(
               tabPanel("Texas", DT::dataTableOutput("alloc_table_tx")),
               tabPanel("New Mexico", DT::dataTableOutput("alloc_table_nm")),
               tabPanel("System", DT::dataTableOutput("alloc_table_sys"))
             )
           )
  ),
  
  # --- Tab 4: Firm Capacity ---
  tabPanel("Firm Capacity",
           icon = icon("check-circle"),
           fluidPage(
             titlePanel("Firm Capacity Analysis"),
             selectInput("firm_region", "Select Region:",
                         choices = c("Texas", "New Mexico", "System"), selected = "System"),
             plotlyOutput("firm_plot"),
             hr(),
             h3("Firm Capacity Summary Tables (MW)"),
             tabsetPanel(
               tabPanel("Texas", DT::dataTableOutput("firm_table_tx")),
               tabPanel("New Mexico", DT::dataTableOutput("firm_table_nm")),
               tabPanel("System", DT::dataTableOutput("firm_table_sys"))
             )
           )
  ),
  
  # --- Tab 5: FAQ ---
  tabPanel("FAQ",
           icon = icon("question-circle"),
           fluidPage(
             titlePanel("Frequently Asked Questions"),
             h4("Q1: Where does the data come from?"),
             p("A: The data is sourced from internal planning documents and reflects the most current projections for load and resources."),
             h4("Q2: How is 'Firm Capacity' calculated?"),
             p("A: Firm capacity is determined by applying specific derating factors to the installed capacity of each resource, accounting for technology type, expected availability, and seasonal performance."),
             h4("Q3: What does 'Generic Resource' mean?"),
             p("A: A generic resource is a placeholder for a future resource that is needed to meet reliability standards but has not yet been specifically identified or contracted."),
             h4("Q4: Can I download the data?"),
             p("A: Yes, on the 'Installed Capacity' tab, the table at the bottom includes buttons to export the filtered data to CSV or Excel.")
           )
  )
)

# 4. SERVER (LOGIC) ----
server <- function(input, output, session) {
  
  # --- Tab 2: L&R Installed Capacity Logic ---
  
  # Reactive expression for filtered data
  filtered_capacity_data <- reactive({
    data <- projected_capacity_long %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    # Filter by type
    if (input$type_filter != "All") {
      data <- data %>% filter(type == input$type_filter)
    }
    
    # Filter by generic resource
    if (input$generic_filter != "all") {
      data <- data %>% filter(Generic_resources == as.numeric(input$generic_filter))
    }
    
    data
  })
  
  # Capacity Plot
  output$capacity_plot <- renderPlotly({
    plot_data <- filtered_capacity_data() %>%
      group_by(Year, type) %>%
      summarise(Total_Capacity = sum(Projected_Capacity, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(plot_data, aes(x = Year, y = Total_Capacity, fill = type)) +
      geom_col() +
      scale_fill_manual(values = type_colors) +
      labs(title = paste("Projected Installed Capacity:", input$year_range[1], "-", input$year_range[2]),
           x = "Year", y = "Total Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Map
  output$map <- renderLeaflet({
    map_data <- filtered_capacity_data() %>%
      # Use only the first year in the range to get a single point per generator
      filter(Year == input$year_range[1] & Projected_Capacity > 0) %>%
      distinct(Generation_Resources, .keep_all = TRUE)
    
    pal <- colorFactor(palette = unlist(type_colors), domain = names(type_colors))
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Capacity) / 5, # Scale radius by capacity
        color = ~pal(type),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>", Generation_Resources, "</b><br>",
          "Type:", type, "<br>",
          "Capacity:", Capacity, "MW"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~type, title = "Resource Type")
  })
  
  # Data Table
  output$capacity_table <- DT::renderDataTable({
    DT::datatable(
      filtered_capacity_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  # --- Tab 3: Allocation Capacity Logic ---
  
  # Allocation Plot
  output$allocation_plot <- renderPlotly({
    req(input$alloc_region)
    
    if (input$alloc_region == "Texas") {
      plot_data <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(TX_LR_sum, na.rm = TRUE), .groups = 'drop')
      title_text <- "Texas Allocation Capacity by Resource Type"
    } else if (input$alloc_region == "New Mexico") {
      plot_data <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(NM_LR_sum, na.rm = TRUE), .groups = 'drop')
      title_text <- "New Mexico Allocation Capacity by Resource Type"
    } else { # System
      plot_data <- dashboard_final_system %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(System_Alloc_Capa, na.rm = TRUE), .groups = 'drop')
      title_text <- "System-wide Allocation Capacity by Resource Type"
    }
    
    p <- ggplot(plot_data, aes(x = Years, y = Capacity, fill = type)) +
      geom_area(stat = "identity") +
      scale_fill_manual(values = type_colors) +
      labs(title = title_text, x = "Year", y = "Allocated Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Allocation Tables (Static)
  create_summary_table <- function(data, value_col) {
    table_data <- data %>%
      select(L_R_Summary, Years, !!sym(value_col)) %>%
      pivot_wider(names_from = Years, values_from = !!sym(value_col), values_fn = sum)
    
    DT::datatable(table_data, 
                  options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, info = FALSE, ordering=FALSE),
                  rownames = FALSE)
  }
  
  output$alloc_table_tx <- DT::renderDataTable(create_summary_table(dashboard_final, "TX_LR_sum"))
  output$alloc_table_nm <- DT::renderDataTable(create_summary_table(dashboard_final, "NM_LR_sum"))
  output$alloc_table_sys <- DT::renderDataTable(create_summary_table(dashboard_final_system, "System_Alloc_Capa"))
  
  # --- Tab 4: Firm Capacity Logic ---
  
  # Firm Capacity Plot
  output$firm_plot <- renderPlotly({
    req(input$firm_region)
    
    if (input$firm_region == "Texas") {
      plot_data <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(Firm_capa_TX, na.rm = TRUE), .groups = 'drop')
      title_text <- "Texas Firm Capacity by Resource Type"
    } else if (input$firm_region == "New Mexico") {
      plot_data <- dashboard_final %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(Firm_capa_NM, na.rm = TRUE), .groups = 'drop')
      title_text <- "New Mexico Firm Capacity by Resource Type"
    } else { # System
      plot_data <- dashboard_final_system %>%
        group_by(Years, type) %>%
        summarise(Capacity = sum(System_Firm_Capa, na.rm = TRUE), .groups = 'drop')
      title_text <- "System-wide Firm Capacity by Resource Type"
    }
    
    p <- ggplot(plot_data, aes(x = Years, y = Capacity, fill = type)) +
      geom_area(stat = "identity") +
      scale_fill_manual(values = type_colors) +
      labs(title = title_text, x = "Year", y = "Firm Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Firm Capacity Tables (Static)
  output$firm_table_tx <- DT::renderDataTable(create_summary_table(dashboard_final, "Firm_capa_TX"))
  output$firm_table_nm <- DT::renderDataTable(create_summary_table(dashboard_final, "Firm_capa_NM"))
  output$firm_table_sys <- DT::renderDataTable(create_summary_table(dashboard_final_system, "System_Firm_Capa"))
}

# 5. RUN APP ----
shinyApp(ui = ui, server = server)
