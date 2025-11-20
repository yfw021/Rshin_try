library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(DT)

# --- 1. Data Definition (Static) ---
# Since we removed the upload, this is your main data source.
# You can replace this block with your real data.frame logic.
mock_data <- data.frame(
  Company = c("Acme Corp", "Globex", "Soylent Corp", "Initech", "Umbrella Corp"),
  Latitude = c(34.0522, 40.7128, 37.7749, 51.5074, 48.8566),
  Longitude = c(-118.2437, -74.0060, -122.4194, -0.1278, 2.3522),
  Resources = c("Solar", "Wind", "Solar", "Oil", "Nuclear")
)

# --- 2. UI Definition ---
ui <- fluidPage(
  titlePanel("Resource Location Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Removed File Input
      h4("Filters"),
      helpText("Select resources to view on the map."),
      
      # Dynamic Filter
      uiOutput("resource_selector")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map View", leafletOutput("map", height = "80vh")),
        tabPanel("Whole Data", DTOutput("data_table"))
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # UI: Resource Selector
  output$resource_selector <- renderUI({
    # We use mock_data directly now
    checkboxGroupInput("selected_resources", 
                       "Filter by Resource Group:", 
                       choices = unique(mock_data$Resources), 
                       selected = unique(mock_data$Resources))
  })
  
  # Reactive: Filter Data
  filtered_data <- reactive({
    req(input$selected_resources)
    mock_data %>% filter(Resources %in% input$selected_resources)
  })
  
  # Reactive: Color Palette
  color_pal <- reactive({
    colorFactor(topo.colors(5), domain = mock_data$Resources)
  })
  
  # Output: Data Table
  output$data_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Output: Map Structure
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  # Observer: Update Map Elements
  observe({
    req(filtered_data())
    df <- filtered_data()
    pal <- color_pal()
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      # 1. The Dots
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = ~pal(Resources),
        stroke = FALSE,
        fillOpacity = 0.8,
        radius = 8,
        popup = ~paste("<strong>Company:</strong>", Company, "<br>",
                       "<strong>Resource:</strong>", Resources)
      ) %>%
      # 2. The Transparent Labels
      addLabelOnlyMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~paste0(Company, " (", round(Latitude, 3), ", ", round(Longitude, 3), ")"),
        labelOptions = labelOptions(
          noHide = TRUE,       # Always visible
          direction = 'top',   # Text above dot
          textOnly = TRUE,     # <--- THIS MAKES THE BOX TRANSPARENT (No background)
          style = list(
            "color" = "black",
            "font-family" = "monospace",
            "font-size" = "12px",
            "font-weight" = "bold",      # Bold makes it easier to read without a box
            "text-shadow" = "1px 1px 0px white" # Adds a tiny white outline for contrast
          )
        )
      ) %>%
      clearControls() %>%
      addLegend("bottomright", pal = pal, values = ~Resources,
                title = "Resource Group", opacity = 1)
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
