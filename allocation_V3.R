# L&R Dashboard App - V3

# 1. LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(janitor)

# 2. DATA PREPARATION ----
# --- Create Sample Data (User should replace this with their actual CSV files) ---
set.seed(42)
capaciti_install_generic <- data.frame(
  Generation_Resources = paste("Generator", LETTERS[1:20]),
  Latitude = c(runif(17, 31.0, 36.5), NA, NA, NA),
  Longitude = c(runif(17, -108.0, -103.0), NA, NA, NA),
  Capacity = sample(50:500, 20, replace = TRUE),
  First_Year = sample(2020:2026, 20, replace = TRUE),
  Last_Year = sample(2045:2055, 20, replace = TRUE),
  type = sample(c("gas", "solar", "dr", "wind", "battery", "btmsolar"), 20, replace = TRUE),
  Generic_resources = sample(0:1, 20, replace = TRUE)
)

years_range <- 2026:2050
lr_summary_items <- c("1.1 RIOGRANDE", "3.2. BATTERY STORAGE", "2.1 RENEWABLE PPAs", "4.0 OTHER")
merged_df4dashboard <- crossing(Years = years_range, L_R_Summary = lr_summary_items) %>%
  mutate(
    type = case_when(
      grepl("BATTERY", L_R_Summary) ~ "battery",
      grepl("RENEWABLE", L_R_Summary) ~ "solar",
      TRUE ~ "gas"
    ),
    TX_LR_sum = abs(rnorm(n(), 100, 30)) * (1 + (Years - 2026) * 0.02),
    NM_LR_sum = abs(rnorm(n(), 50, 15)) * (1 + (Years - 2026) * 0.01)
  )

# --- Real Data Loading (User should uncomment these lines) ---
# capaciti_install_generic <- read.csv("capaciti_install_generic.csv")
# merged_df4dashboard <- read.csv("merged_df4dashboard.csv")

capacity_by_year <- crossing(Generation_Resources = capaciti_install_generic$Generation_Resources, Year = 2026:2050) %>%
  left_join(capaciti_install_generic, by = "Generation_Resources") %>%
  mutate(Active_Capacity = ifelse(Year >= First_Year & Year <= Last_Year, Capacity, 0))

# 3. UI DEFINITION ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "L&R Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Installed Capacity", tabName = "installed_capacity", icon = icon("chart-bar")),
      menuItem("Allocation Capacity", tabName = "allocation_capacity", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # == Tab 1: Introduction ==
      tabItem(tabName = "intro",
        fluidRow(
          box(
            title = "Welcome to the Load & Resources Web Tool", status = "primary", solidHeader = TRUE, width = 12,
            p("This dashboard provides an interactive platform to analyze and visualize Load and Resource (L&R) data. You can explore installed generation capacity projections and capacity allocations across different regions.")
          ),
          box(
            title = "Vocabulary", status = "primary", width = 12, collapsible = TRUE,
            tags$dl(
              tags$dt("Load:"),
              tags$dd("The amount of electrical power demanded by consumers on the grid."),
              tags$dt("Resources:"),
              tags$dd("The generation assets (like power plants, solar farms, batteries) available to meet the load."),
              tags$dt("Capacity (MW):"),
              tags$dd("The maximum output an electricity generator can physically produce, measured in Megawatts (MW).")
            )
          ),
          box(
            title = "Meet The Team", status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
                lapply(1:9, function(i) {
                    column(width = 4, align = "center",
                        tags$img(src = paste0("member", i, ".png"), height = "120px", width = "120px", style = "border-radius: 50%;"),
                        h4(paste("Team Member", i)),
                        p("Short bio or role description for this team member goes here.")
                    )
                })
            )
          )
        )
      ),
      
      # == Tab 2: L&R Installed Capacity ==
      tabItem(tabName = "installed_capacity",
        fluidRow(
          box(
            title = "Filters", status = "warning", solidHeader = TRUE, width = 3,
            sliderInput("year_range_slider", "Select Year Range:",
                        min = 2026, max = 2050, value = c(2026, 2050), sep = ""),
            selectInput("type_filter", "Filter by Resource Type:",
                        choices = c("All", unique(capaciti_install_generic$type))),
            radioButtons("generic_filter", "Filter by Generic Resources:",
                         choices = c("All", "Yes" = 1, "No" = 0))
          ),
          column(width = 9,
            box(
              title = "Capacity Map & Plot", status = "primary", solidHeader = TRUE, width = NULL,
              fluidRow(
                column(width = 6, leafletOutput("capacity_map")),
                column(width = 6, plotlyOutput("capacity_plot"))
              )
            ),
            box(
              title = "Data Table", status = "primary", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("capacity_table")
            )
          )
        )
      ),
      
      # == Tab 3: Allocation Capacity ==
      tabItem(tabName = "allocation_capacity",
        fluidRow(
          box(
            title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
            selectInput("category_select", "Select Category:",
                        choices = c("Texas", "New Mexico", "System"), selected = "System")
          ),
          box(
            title = "Capacity Allocation by Type", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("allocation_plot")
          ),
          box(
            title = "Allocation Tables by Category", status = "info", solidHeader = TRUE, width = 12,
            tabsetPanel(
                tabPanel("Texas", DT::dataTableOutput("texas_table")),
                tabPanel("New Mexico", DT::dataTableOutput("nm_table")),
                tabPanel("System", DT::dataTableOutput("system_table"))
            )
          )
        )
      )
    )
  )
)

# 4. SERVER LOGIC ----
server <- function(input, output, session) {
  
  # --- TAB 2: INSTALLED CAPACITY SERVER LOGIC ---
  
  filtered_capacity_data <- reactive({
    data <- capacity_by_year %>%
      filter(Year >= input$year_range_slider[1] & Year <= input$year_range_slider[2])
    
    if (input$type_filter != "All") {
      data <- data %>% filter(type == input$type_filter)
    }
    
    if (input$generic_filter != "All") {
      data <- data %>% filter(Generic_resources == input$generic_filter)
    }
    
    return(data)
  })
  
  output$capacity_plot <- renderPlotly({
    plot_data <- filtered_capacity_data() %>%
      group_by(Year) %>%
      summarise(Total_Capacity = sum(Active_Capacity, na.rm = TRUE))
    
    p <- ggplot(plot_data, aes(x = Year, y = Total_Capacity)) +
      geom_col(fill = "#3c8dbc") +
      labs(title = "Total Installed Capacity Over Time", x = "Year", y = "Total Capacity (MW)") +
      theme_minimal()
      
    ggplotly(p)
  })
  
  output$capacity_map <- renderLeaflet({
    map_data <- filtered_capacity_data() %>%
        filter(Active_Capacity > 0) %>%
        distinct(Generation_Resources, .keep_all = TRUE)

    known_locations <- map_data %>% filter(!is.na(Latitude) & !is.na(Longitude))
    unknown_locations <- map_data %>% filter(is.na(Latitude) | is.na(Longitude))

    m <- leaflet() %>% addTiles()
    
    if(nrow(known_locations) > 0) {
      m <- m %>% addMarkers(
        data = known_locations,
        lng = ~Longitude, lat = ~Latitude,
        popup = ~paste("<b>", Generation_Resources, "</b><br>",
                       "Type: ", type, "<br>",
                       "Capacity: ", Capacity, " MW")
      )
    }

    if(nrow(unknown_locations) > 0) {
      m <- m %>%
        addRectangles(
          lng1 = -125, lat1 = 49,
          lng2 = -115, lat2 = 50,
          fillColor = "grey", fillOpacity = 0.5,
          color = "black", weight = 1,
          label = "Unknown Locations"
        ) %>%
        addCircleMarkers(
          data = unknown_locations,
          lng = runif(nrow(unknown_locations), -124.5, -115.5),
          lat = runif(nrow(unknown_locations), 49.1, 49.9),
          radius = 5,
          color = "red",
          popup = ~paste("<b>", Generation_Resources, "</b><br>",
                         "Type: ", type, "<br>",
                         "Location: Unknown")
        )
    }
    m
  })
  
  output$capacity_table <- DT::renderDataTable({
    display_data <- filtered_capacity_data() %>%
      select(Generation_Resources, Year, type, Capacity, Active_Capacity, First_Year, Last_Year) %>%
      filter(Active_Capacity > 0)

    DT::datatable(display_data, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  
  # --- TAB 3: ALLOCATION CAPACITY SERVER LOGIC ---
  
  allocation_plot_data <- reactive({
    req(input$category_select)
    df <- merged_df4dashboard
    
    if (input$category_select == "Texas") {
      df %>% group_by(Years, type) %>% summarise(Capacity = sum(TX_LR_sum, na.rm = TRUE), .groups = 'drop')
    } else if (input$category_select == "New Mexico") {
      df %>% group_by(Years, type) %>% summarise(Capacity = sum(NM_LR_sum, na.rm = TRUE), .groups = 'drop')
    } else { # System
      df %>% mutate(System_sum = TX_LR_sum + NM_LR_sum) %>%
        group_by(Years, type) %>% summarise(Capacity = sum(System_sum, na.rm = TRUE), .groups = 'drop')
    }
  })
  
  output$allocation_plot <- renderPlotly({
    p <- ggplot(allocation_plot_data(), aes(x = Years, y = Capacity, fill = type)) +
      geom_area(alpha=0.7 , size=0.5, colour="white") +
      labs(title = paste("Capacity Allocation for", input$category_select),
           x = "Year", y = "Capacity (MW)", fill = "Resource Type") +
      theme_minimal()
      
    ggplotly(p)
  })
  
  # UPDATED: Helper function to round values and add totals
  create_allocation_table <- function(data, value_col) {
    value_col_enquo <- enquo(value_col)
    
    table_data <- data %>%
      select(L_R_Summary, Years, !!value_col_enquo) %>%
      pivot_wider(names_from = Years, values_from = !!value_col_enquo, values_fill = list(value = 0)) %>%
      # **NEW**: Round all numeric columns to 0 decimal places
      mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
      adorn_totals("row", name = "Total", fill = "-")
      
    DT::datatable(table_data, 
                  options = list(scrollX = TRUE, searching = FALSE, paging = FALSE, ordering = FALSE), 
                  rownames = FALSE)
  }
  
  output$texas_table <- DT::renderDataTable({
    create_allocation_table(merged_df4dashboard, TX_LR_sum)
  })
  
  output$nm_table <- DT::renderDataTable({
    create_allocation_table(merged_df4dashboard, NM_LR_sum)
  })
  
  output$system_table <- DT::renderDataTable({
    system_data <- merged_df4dashboard %>% mutate(System_sum = TX_LR_sum + NM_LR_sum)
    create_allocation_table(system_data, System_sum)
  })

}

# 5. RUN THE APP ----
shinyApp(ui = ui, server = server)
