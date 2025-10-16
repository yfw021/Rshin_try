# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# --- 1. Sample Data ---
# Create a sample data frame with your specified columns.
# In a real-world scenario, you would load this from a CSV file or a database.
data <- data.frame(
  CompanyName = c("El Paso Solar", "Westwind Energy", "Desert Storage LLC", "Grid-Scale Power", "Cali Hybrid", "Plains Wind", "Sunshine Co", "Future Project (No Location)"),
  Ownership = c("Utility", "IPP", "IPP", "Utility", "Community", "IPP", "Utility", "IPP"),
  COD = as.Date(c("2024-05-20", "2023-11-10", "2025-01-15", "2024-08-01", "2023-09-22", "2022-12-30", "2024-10-18", "2026-01-01")),
  Term = c(20, 25, 15, 20, 15, 25, 20, 15),
  TechnologyType = c("Solar", "Wind", "Storage", "Solar", "Hybrid", "Wind", "Solar", "Storage"),
  GenerationCapacity = c(150, 200, 0, 100, 75, 250, 120, 0),
  StorageCapacity = c(50, 0, 200, 0, 100, 0, 20, 300),
  Duration = c(4, 0, 8, 0, 4, 0, 2, 12),
  Latitude = c(31.84, 34.05, 33.74, 29.76, 36.77, 39.73, 32.71, NA),
  Longitude = c(-106.41, -118.24, -116.97, -95.36, -119.41, -104.99, -96.79, NA),
  stringsAsFactors = FALSE
)

# Define coordinates for the El Paso boundary polygon
el_paso_boundary <- data.frame(
  lng = c(-106.55, -106.5, -106.4, -106.35, -106.45, -106.55),
  lat = c(31.9, 31.75, 31.7, 31.78, 31.88, 31.9)
)


# --- 2. Define UI (User Interface) ---
# The UI defines the layout and appearance of your web app.
ui <- fluidPage(
  # App title
  titlePanel("Energy Projects Map Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      h4("Filters"),
      # Selector for Technology Type
      selectInput("tech_type", "Technology Type:",
                  choices = c("All", unique(data$TechnologyType)),
                  selected = "All"),
      
      # Selector for Ownership model
      selectInput("ownership_type", "Ownership:",
                  choices = c("All", unique(data$Ownership)),
                  selected = "All"),
      
      hr(), # Add a horizontal line for separation
      h5("Data Summary"),
      textOutput("unknown_location_text"),
      
      width = 3 # Adjust sidebar width
    ),
    
    # Main panel for displaying the map
    mainPanel(
      leafletOutput("map", width = "100%", height = "800px"),
      width = 9 # Adjust main panel width
    )
  )
)

# --- 3. Define Server Logic ---
# The server function contains the instructions to build and render the app's output.
server <- function(input, output, session) {
  
  # Create a reactive expression that filters the data based on user input
  # This version still contains projects with NA locations
  filtered_data_with_na <- reactive({
    # Start with the full dataset
    df <- data
    
    # Filter by Technology Type if a specific type is selected
    if (input$tech_type != "All") {
      df <- df %>% filter(TechnologyType == input$tech_type)
    }
    
    # Filter by Ownership if a specific type is selected
    if (input$ownership_type != "All") {
      df <- df %>% filter(Ownership == input$ownership_type)
    }
    
    return(df)
  })
  
  # Create a second reactive expression that removes rows with NA lat/lon
  # This data will be used for plotting on the map
  data_for_map <- reactive({
    filtered_data_with_na() %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
  })
  
  # Render text to show the count of projects with unknown locations
  output$unknown_location_text <- renderText({
    df <- filtered_data_with_na()
    unknown_count <- sum(is.na(df$Latitude) | is.na(df$Longitude))
    
    if (unknown_count > 0) {
      paste(unknown_count, "project(s) have an unknown location and are not shown on the map.")
    } else {
      "All projects in the current filter have a known location."
    }
  })
  
  # Create a color palette for the different technology types
  # Using a categorical palette since TechnologyType is a factor
  tech_palette <- colorFactor(palette = "viridis", domain = data$TechnologyType)
  
  # Render the initial leaflet map
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      # Add a polygon boundary for the El Paso area
      addPolygons(
        data = el_paso_boundary,
        lng = ~lng, lat = ~lat,
        fillColor = "transparent",
        color = "#03F",
        weight = 2,
        opacity = 0.6,
        popup = "El Paso Area Boundary"
      ) %>%
      addLegend(
        "bottomright", 
        pal = tech_palette, 
        values = ~TechnologyType,
        title = "Technology Type",
        opacity = 0.9
      )
  })
  
  # Use an observer to watch for changes in the filtered data and update the map
  # This is more efficient than re-rendering the entire map
  observe({
    # Get the filtered data that has valid locations
    df <- data_for_map()
    
    # Create a custom icon set based on the Technology Type
    # This uses the Font Awesome icon library
    map_icons <- awesomeIcons(
      icon = case_when(
        df$TechnologyType == "Solar"   ~ "sun",
        df$TechnologyType == "Wind"    ~ "fan",
        df$TechnologyType == "Storage" ~ "car-battery",
        df$TechnologyType == "Hybrid"  ~ "leaf",
        TRUE                           ~ "question-circle" # Default icon
      ),
      library = "fa",
      markerColor = tech_palette(df$TechnologyType), # Reuse the existing color palette
      iconColor = "#FFFFFF" # Set icon color to white for contrast
    )
    
    # Create the popup content
    popup_content <- paste(
      "<h5>", df$CompanyName, "</h5>",
      "<b>Technology:</b>", df$TechnologyType, "<br>",
      "<b>Ownership:</b>", df$Ownership, "<br>",
      "<b>Gen Capacity:</b>", df$GenerationCapacity, "MW<br>",
      "<b>Storage Capacity:</b>", df$StorageCapacity, "MWh<br>",
      "<b>COD:</b>", df$COD
    )
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>% # Clear existing markers
      # Use addAwesomeMarkers instead of addCircleMarkers
      addAwesomeMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        icon = map_icons, # Apply the custom icons
        popup = popup_content
      )
  })
}

# --- 4. Run the Application ---
shinyApp(ui, server)
