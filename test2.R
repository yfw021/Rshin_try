# Load necessary libraries for the Shiny app.
# If you don't have them installed, run the following commands in your R console:
# install.packages(c("shiny", "dplyr", "DT", "ggplot2"))

library(shiny)
library(dplyr)
library(DT)
library(ggplot2) # Added for plotting

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
  
  titlePanel("Machine Capacity Dashboard"),
  
  # Use a navlistPanel for left-side navigation.
  navlistPanel(
    well = TRUE, # Adds a gray background to the nav list for better visibility
    widths = c(2, 10), # Sets column widths for nav (2/12) and content (10/12)

    "Navigation", # A header for the navigation list

    # --- Page 1: Introduction ---
    tabPanel("Introduction",
             # Use a jumbotron for a nice visual introduction
             tags$div(class = "jumbotron",
               tags$h1("Welcome to the Capacity Projection Tool", class = "display-4"),
               tags$p("This interactive dashboard is designed to help you visualize and analyze machine capacity over time.", class = "lead"),
               tags$hr(class = "my-4"),
               tags$p("Navigate to the 'Dashboard' tab to get started. You can select a range of years to see the projected operational capacity for each machine, both in a detailed table and a summary plot."),
               tags$a(class = "btn btn-primary btn-lg", href = "#", role = "button", "Learn more") # A dummy button for effect
             ),
             
             # Add some more descriptive sections
             fluidRow(
               column(6,
                 h3("How It Works"),
                 p("The dashboard uses a simple logic: a machine's capacity is counted for a given year only if that year falls between its specified 'first year' and 'last year' of operation. Otherwise, its capacity is considered zero."),
               ),
               column(6,
                 h3("Features"),
                 tags$ul(
                   tags$li("Interactive year selection with a slider."),
                   tags$li("A summary plot showing total capacity across all machines."),
                   tags$li("A detailed, searchable, and sortable data table."),
                   tags$li("A fixed column in the table to easily identify machines when scrolling.")
                 )
               )
             )
    ),

    # --- Page 2: The Main Dashboard ---
    tabPanel("Dashboard",
             # The sidebarLayout for the dashboard is now nested within the main content area.
             sidebarLayout(
               
               # The sidebar panel is for input controls.
               sidebarPanel(
                 h4("Controls"),
                 # Add a slider to let the user select the range of years.
                 sliderInput("projection_years_slider",
                             "Select Projection Year Range:",
                             min = 2020,
                             max = 2050,
                             value = c(2026, 2040),
                             sep = "") # Removes the comma from the year display
               ),
               
               # The main panel now displays the plot and table sequentially.
               mainPanel(
                 # --- Section 1: Summary Plot ---
                 h3("Total Capacity Over Time"),
                 p("This plot shows the total available capacity across all machines for each year in the selected range."),
                 plotOutput("capacity_plot"),
                 
                 hr(), 
                 
                 # --- Section 2: Detailed Table ---
                 h3("Projected Capacity Table"),
                 p("The table below shows the projected capacity for each machine. A value of '0' indicates the machine is not operational in that year."),
                 DTOutput("capacity_table")
               )
             )
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
    
    projection_years <- start_year:end_year
    projected_df <- df
    
    for (year in projection_years) {
      col_name <- as.character(year)
      projected_df <- projected_df %>%
        mutate(!!col_name := ifelse(last_year >= year & first_year <= year, capacity, 0))
    }
    return(projected_df)
  })
  
  # Render the reactive data frame as an interactive table.
  output$capacity_table <- renderDT({
    datatable(projected_data(),
              extensions = 'FixedColumns',
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                fixedColumns = list(leftColumns = 1) 
              ),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
  
  # Render the summary plot.
  output$capacity_plot <- renderPlot({
    
    data_to_plot <- projected_data()
    start_year <- input$projection_years_slider[1]
    end_year <- input$projection_years_slider[2]
    year_cols <- as.character(start_year:end_year)

    summary_data <- data_to_plot %>%
      select(all_of(year_cols)) %>%
      colSums() %>%
      as.data.frame() %>%
      rename(TotalCapacity = ".") %>%
      tibble::rownames_to_column("Year") %>%
      mutate(Year = as.numeric(Year))

    ggplot(summary_data, aes(x = Year, y = TotalCapacity)) +
      geom_col(fill = "#2c7fb8", alpha = 0.8) +
      geom_text(aes(label = TotalCapacity), vjust = -0.5, color = "black", size = 4) +
      labs(
        title = "Total Projected Capacity by Year",
        x = "Year",
        y = "Total Capacity"
      ) +
      theme_minimal(base_size = 14) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(limits = c(0, max(summary_data$TotalCapacity, na.rm = TRUE) * 1.1))
  })
}

# --- Run the Application ---
shinyApp(ui = ui, server = server)
