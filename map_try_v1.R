# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# --- 1. Sample Data ---
# Create a sample data frame with your specified columns.
# In a real-world scenario, you would load this from a CSV file orss a database.
data <- data.frame(
  CompanyName = c("El Paso Solar", "Westwind Energy", "Desert Storage LLC", "Grid-Scale Power", "Cali Hybrid", "Plains Wind", "Sunshine Co"),
  Ownership = c("Utility", "IPP", "IPP", "Utility", "Community", "IPP", "Utility"),
  COD = as.Date(c("2024-05-20", "2023-11-10", "2025-01-15", "2024-08-01", "2023-09-22", "2022-12-30", "2024-10-18")),
  Term = c(20, 25, 15, 20, 15, 25, 20),
  TechnologyType = c("Solar", "Wind", "Storage", "Solar", "Hybrid", "Wind", "Solar"),
  GenerationCapacity = c(150, 200, 0, 100, 75, 250, 120),
  StorageCapacity = c(50, 0, 200, 0, 100, 0, 20),
  Duration = c(4, 0, 8, 0, 4, 0, 2),
  Latitude = c(31.84, 34.05, 33.74, 29.76, 36.77, 39.73, 32.71),
  Longitude = c(-106.41, -118.24, -116.97, -95.36, -119.41, -104.99, -96.79),
  stringsAsFactors = FALSE
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
  filtered_data <- reactive({
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
  
  # Create a color palette for the different technology types
  # Using a categorical palette since TechnologyType is a factor
  tech_palette <- colorFactor(palette = "viridis", domain = data$TechnologyType)
  
  # Render the initial leaflet map
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
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
    # Get the filtered data
    df <- filtered_data()
    
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
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        radius = 8,
        color = ~tech_palette(TechnologyType),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = popup_content
      )
  })
}

# --- 4. Run the Application ---
shinyApp(ui, server)


