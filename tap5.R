# Load libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)

# --- DATA LOADING ---
capacity_df <- read_csv("capaciti_install_generic.csv") %>%
  mutate(
    First_Year = as.numeric(First_Year),
    Last_Year = as.numeric(Last_Year)
  )

dashboard_df <- read_csv("dashboard_final.csv") %>%
  mutate(Years = as.numeric(Years))

# --- PRE-COMPUTING STATIC TABLES FOR TABS 3 & 4 ---
# This is efficient because it's done only once when the app starts.

# Allocation Capacity Tables (Tab 3)
pivot_and_format <- function(df, value_col) {
  df %>%
    # Use !!as.symbol() to use the string variable `value_col` as a column name
    pivot_wider(id_cols = L_R_Summary, names_from = Years, values_from = !!as.symbol(value_col), values_fn = sum)
}

tx_alloc_table_data <- dashboard_df %>% pivot_and_format("TX_LR_sum")
nm_alloc_table_data <- dashboard_df %>% pivot_and_format("NM_LR_sum")
system_alloc_table_data <- dashboard_df %>%
  mutate(SY_LR_sum = TX_LR_sum + NM_LR_sum) %>%
  pivot_and_format("SY_LR_sum")

# Firm Capacity Tables (Tab 4)
tx_firm_table_data <- dashboard_df %>% pivot_and_format("Firm_capa_TX")
nm_firm_table_data <- dashboard_df %>% pivot_and_format("Firm_capa_NM")
system_firm_table_data <- dashboard_df %>%
  mutate(SY_Firm_sum = Firm_capa_TX + Firm_capa_NM) %>%
  pivot_and_format("SY_Firm_sum")


# --- UI DEFINITION ---
ui <- navbarPage(
  title = "L&R Dashboard",
  
  # --- TAB 1: INTRODUCTION ---
  tabPanel("Introduction",
           fluidPage(
             h2("Welcome to the L&R Planning Tool"),
             p("This web tool provides insights into the projected load and resources for our electrical system through 2050."),
             hr(),
             h2("Vocabulary"),
             tags$dl(
               tags$dt("Load"),
               tags$dd("The amount of electrical power demanded by consumers."),
               tags$dt("Resource"),
               tags$dd("A generator or other asset capable of supplying electricity (e.g., solar farm, gas plant, battery)."),
               tags$dt("Installed Capacity"),
               tags$dd("The maximum output (in Megawatts) that a resource can produce under ideal conditions.")
             ),
             hr(),
             h2("Meet the Team"),
             fluidRow(
               # Create 9 of these column blocks for the team members
               column(4,
                      div(class = "team-member-card", # You can add CSS for this class
                          img(src = "person1.png", height = "100px"), # Put images in a 'www' subfolder
                          h4("Team Member 1"),
                          p("Role and short intro.")
                      )
               ),
               column(4, # Member 2... ),
                      column(4, # Member 3... )
                      )
               )
             ),
             
             # --- TAB 2: L&R INSTALLED CAPACITY ---
             tabPanel("Installed Capacity",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Filters"),
                          sliderInput("year_slider", "Select Year Range:",
                                      min = 2026, max = 2050, value = c(2026, 2050), step = 1, sep = ""),
                          selectInput("type_filter", "Filter by Resource Type:",
                                      choices = c("All", unique(capacity_df$type)), selected = "All", multiple = TRUE),
                          radioButtons("generic_filter", "Filter by Generic Status:",
                                       choices = c("All" = "all", "Yes" = 1, "No" = 0), selected = "all")
                        ),
                        mainPanel(
                          # The outputs are generated in the server logic
                          plotOutput("capacity_plot"),
                          leafletOutput("capacity_map"),
                          DT::dataTableOutput("capacity_table")
                        )
                      )
             ),
             
             # --- TAB 3: ALLOCATION CAPACITY ---
             tabPanel("Allocation Capacity",
                      fluidPage(
                        radioButtons("category_selector_alloc", "Select Category:",
                                     choices = c("Texas", "New Mexico", "System"), selected = "System", inline = TRUE),
                        hr(),
                        plotOutput("allocation_plot"),
                        hr(),
                        h3("Texas Allocation Summary"),
                        DT::dataTableOutput("texas_alloc_table"),
                        h3("New Mexico Allocation Summary"),
                        DT::dataTableOutput("nm_alloc_table"),
                        h3("System Allocation Summary"),
                        DT::dataTableOutput("system_alloc_table")
                      )
             ),
             
             # --- TAB 4: FIRM CAPACITY ---
             tabPanel("Firm Capacity",
                      fluidPage(
                        radioButtons("category_selector_firm", "Select Category:",
                                     choices = c("Texas", "New Mexico", "System"), selected = "System", inline = TRUE),
                        hr(),
                        plotOutput("firm_capacity_plot"),
                        hr(),
                        h3("Texas Firm Capacity Summary"),
                        DT::dataTableOutput("texas_firm_table"),
                        h3("New Mexico Firm Capacity Summary"),
                        DT::dataTableOutput("nm_firm_table"),
                        h3("System Firm Capacity Summary"),
                        DT::dataTableOutput("system_firm_table")
                      )
             ),
             
             # --- TAB 5: FAQ ---
             tabPanel("FAQ",
                      fluidPage(
                        h2("Frequently Asked Questions (FAQ)"),
                        p(strong("Q: What is the source of this data?")),
                        p("A: The data is sourced from our internal planning models, updated as of July 2025."),
                        p(strong("Q: How is 'Firm Capacity' calculated?")),
                        p("A: Firm capacity is determined by applying effective load carrying capability (ELCC) percentages to intermittent resources like solar and wind, and considering forced outage rates for thermal units.")
                      )
             )
           )
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
           


