# Load necessary libraries for the Shiny app.
# If you don't have them installed, run the following commands in your R console:
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("DT")

library(shiny)
library(dplyr)
library(DT)

# --- Sample Data ---
# This is the same sample data frame from the original script.
# In a real application, you might load this from a CSV or a database.
df <- data.frame(
  stringsAsFactors = FALSE,
  machine = c("Machine A", "Machine B", "Machine C", "Machine D"),
  capacity = c(100, 150, 120, 200),
  first_year = c(2010, 2015, 2020, 2027),
  last_year = c(2025, 2030, 2035, 2040)
)

# --- User Interface (UI) ---
# Defines the layout and appearance of the web app.
ui <- fluidPage(
  
  # Add a title to the dashboard.
  titlePanel("Machine Capacity Projection Dashboard"),
  
  # Use a sidebar layout for user inputs and main content.
  sidebarLayout(
    
    # The sidebar panel is for input controls.
    sidebarPanel(
      h4("Controls"),
      # Add a slider to let the user select the range of years.
      # The default range is set from 2026 to 2040.
      sliderInput("projection_years_slider",
                  "Select Projection Year Range:",
                  min = 2020,
                  max = 2050,
                  value = c(2026, 2040),
                  sep = "") # Removes the comma from the year display
    ),
    
    # The main panel is for displaying the output.
    mainPanel(
      h3("Projected Capacity Table"),
      p("The table below shows the projected capacity for each machine based on the selected year range. A value of '0' indicates the machine is not operational in that year."),
      # Use DTOutput for an interactive table.
      DTOutput("capacity_table")
    )
  )
)

# --- Server Logic ---
# Defines the backend logic that powers the app.
server <- function(input, output) {
  
  # Create a reactive expression for the projected data.
  # This code will re-run automatically whenever the slider input changes.
  projected_data <- reactive({
    
    # Get the start and end years from the slider input.
    start_year <- input$projection_years_slider[1]
    end_year <- input$projection_years_slider[2]
    
    # Define the full range of years based on the slider.
    projection_years <- start_year:end_year
    
    # Start with the original dataframe.
    projected_df <- df
    
    # Loop through each year in the selected range.
    for (year in projection_years) {
      col_name <- as.character(year)
      
      # Use the same logic as the original script to calculate capacity for the year.
      projected_df <- projected_df %>%
        mutate(!!col_name := ifelse(last_year >= year & first_year <= year, capacity, 0))
    }
    
    # Return the final projected data frame.
    return(projected_df)
  })
  
  # Render the reactive data frame as an interactive table.
  output$capacity_table <- renderDT({
    # Call the reactive expression to get the data.
    datatable(projected_data(),
              options = list(
                scrollX = TRUE, # Allow horizontal scrolling for many columns
                pageLength = 10 # Show 10 rows per page
              ),
              rownames = FALSE, # Hide row names
              class = 'cell-border stripe') # Add styling
  })
  
}

# --- Run the Application ---
# This command starts the Shiny web application.
shinyApp(ui = ui, server = server)

