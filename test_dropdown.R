# Load necessary libraries for the Shiny app.
# If you don't have them installed, run the following commands in your R console:
# install.packages(c("shiny", "dplyr", "DT", "ggplot2", "httr", "jsonlite", "tidyr"))

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(httr)      # For making API requests
library(jsonlite)  # For handling JSON data
library(tidyr)     # For reshaping data for the plot

# --- Sample Data ---
# Expanded data with a new "auto" column.
df <- data.frame(
  stringsAsFactors = FALSE,
  machine = c("Machine A", "Machine B", "Machine C", "Machine D", "Machine E", "Machine F", "Machine G", "Machine H"),
  type = c("Type A", "Type A", "Type B", "Type B", "Type C", "Type C", "Type A", "Type B"),
  region = c("TX", "NM", "TX", "NM+TX", "TX", "NM", "NM+TX", "TX"),
  auto = c(1, 0, 1, 1, 0, 1, 0, 1), # NEW: Auto column (1=Yes, 0=No)
  Capacity = c(100, 150, 120, 200, 90, 110, 180, 140),
  First_year = c(2010, 2015, 2020, 2027, 2012, 2018, 2022, 2028),
  Last_Year = c(2025, 2030, 2035, 2040, 2028, 2032, 2038, 2042)
)

# --- User Interface (UI) ---
ui <- fluidPage(
  
  titlePanel("Machine Capacity Dashboard"),
  
  # Use a navlistPanel for left-side navigation.
  navlistPanel(
    well = TRUE, 
    widths = c(2, 10),

    "Navigation", 

    # --- Page 1: Introduction ---
    tabPanel("Introduction",
             tags$div(class = "jumbotron",
               tags$h1("Welcome to the Capacity Projection Tool", class = "display-4"),
               tags$p("This interactive dashboard is designed to help you visualize and analyze machine capacity over time.", class = "lead"),
               tags$hr(class = "my-4"),
               tags$p("Navigate to the 'Dashboard' tab to get started. Use the AI Assistant to ask questions about the data shown.")
             )
    ),

    # --- Page 2: The Main Dashboard ---
    tabPanel("Dashboard",
             sidebarLayout(
               
               sidebarPanel(
                 width = 4, # Widen sidebar to accommodate assistant
                 h4("Controls"),
                 
                 # --- Filter Controls ---
                 selectInput("region_select", "Filter by Region:",
                             choices = c("All", "TX", "NM", "NM+TX")),
                 
                 # NEW: Auto filter dropdown
                 selectInput("auto_select", "Filter by Auto:",
                             choices = c("All", "Yes" = 1, "No" = 0)),
                 
                 sliderInput("projection_years_slider",
                             "Select Projection Year Range:",
                             min = 2020,
                             max = 2050,
                             value = c(2026, 2040),
                             sep = ""),
                 
                 # --- AI Assistant Section ---
                 hr(),
                 h4("AI Assistant"),
                 p("Ask a question about the data in the table.", style = "font-size: 0.9em; color: grey;"),
                 passwordInput("api_key_input", "Enter your Gemini API Key:"),
                 textInput("user_question", "Your Question:", placeholder = "e.g., Which machine is active in 2032?"),
                 actionButton("ask_button", "Ask Assistant"),
                 
                 h5("Assistant's Response:"),
                 uiOutput("assistant_response_ui")
               ),
               
               mainPanel(
                 width = 8,
                 # --- Section 1: Summary Plot ---
                 plotOutput("capacity_plot"),
                 hr(), 
                 # --- Section 2: Detailed Table ---
                 DTOutput("capacity_table")
               )
             )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive Data Projection Logic ---
  # This now filters by region and auto status.
  projected_data <- reactive({
    
    # Start with the full dataset
    filtered_df <- df
    
    # Filter data based on region selection
    if (input$region_select != "All") {
      filtered_df <- filtered_df %>% filter(region == input$region_select)
    }
    
    # Filter data based on auto selection
    if (input$auto_select != "All") {
      filtered_df <- filtered_df %>% filter(auto == input$auto_select)
    }
    
    projection_years <- input$projection_years_slider[1]:input$projection_years_slider[2]
    projected_df <- filtered_df
    
    # Return an empty frame if filter results in no data, to prevent errors
    if (nrow(projected_df) == 0) {
      return(data.frame())
    }
    
    for (year in projection_years) {
      col_name <- as.character(year)
      projected_df <- projected_df %>%
        mutate(!!col_name := ifelse(Last_Year >= year & First_year <= year, Capacity, 0))
    }
    # Reorder columns for clarity
    projected_df %>% select(machine, type, region, auto, everything())
  })
  
  # --- Table and Plot Outputs ---
  output$capacity_table <- renderDT({
    req(nrow(projected_data()) > 0) # Ensure data is available before rendering
    datatable(projected_data(), extensions = 'FixedColumns',
              options = list(scrollX = TRUE, pageLength = 10, 
                             fixedColumns = list(leftColumns = 4)), # Freeze first four columns
              rownames = FALSE, class = 'cell-border stripe')
  })
  
  output$capacity_plot <- renderPlot({
    plot_data <- projected_data()
    req(nrow(plot_data) > 0) # Ensure data is available
    
    year_cols <- as.character(input$projection_years_slider[1]:input$projection_years_slider[2])
    
    summary_data <- plot_data %>%
      select(type, all_of(year_cols)) %>%
      pivot_longer(cols = -type, names_to = "Year", values_to = "Capacity") %>%
      group_by(Year, type) %>%
      summarise(TotalCapacity = sum(Capacity, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Year = as.numeric(Year))
      
    total_labels <- summary_data %>%
      group_by(Year) %>%
      summarise(TotalLabel = sum(TotalCapacity, na.rm = TRUE))

    ggplot(summary_data, aes(x = Year, y = TotalCapacity, fill = type)) +
      geom_col(position = "stack") +
      geom_text(
        data = total_labels,
        aes(x = Year, y = TotalLabel, label = TotalLabel),
        inherit.aes = FALSE, vjust = -0.5, color = "black", size = 4
      ) +
      labs(
        title = paste("Total Projected Capacity for Region:", input$region_select),
        x = "Year", y = "Total Capacity", fill = "Machine Type"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_brewer(palette = "Pastel1") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(limits = c(0, max(total_labels$TotalLabel, na.rm = TRUE) * 1.15))
  })

  # --- AI Assistant Server Logic ---
  assistant_response <- reactiveVal("Awaiting your question...")
  observeEvent(input$ask_button, {
    showNotification("Asking the assistant...", type = "message", duration = 3)
    req(input$api_key_input, input$user_question)
    req(nrow(projected_data()) > 0)
    data_context <- capture.output(write.csv(projected_data(), row.names = FALSE))
    data_context_string <- paste(data_context, collapse = "\n")
    
    # Generate a more descriptive filter summary for the prompt
    auto_filter_text <- if (input$auto_select == "All") "All" else if (input$auto_select == 1) "Yes" else "No"
    
    full_prompt <- paste(
      "You are a helpful data analyst assistant for a Shiny dashboard...",
      "The user is viewing data filtered by Region: '", input$region_select, "' and by Auto: '", auto_filter_text, "'.",
      "Here is the data table they are seeing:",
      "--- DATA START ---", data_context_string, "--- DATA END ---",
      "The user has asked the following question:", input$user_question,
      "Please provide a clear and concise answer based *only* on the data provided."
    )
    tryCatch({
      api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=", input$api_key_input)
      body <- list(contents = list(list(parts = list(list(text = full_prompt)))))
      response <- POST(url = api_url, body = toJSON(body, auto_unbox = TRUE), add_headers("Content-Type" = "application/json"))
      if (status_code(response) == 200) {
        result <- content(response, "parsed")
        assistant_response(result$candidates[[1]]$content$parts[[1]]$text)
      } else {
        assistant_response(paste("API Error:", status_code(response), "-", content(response, "text")))
      }
    }, error = function(e) {
      assistant_response(paste("An error occurred:", e$message))
    })
  })
  output$assistant_response_ui <- renderUI({
    tags$div(
      style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 5px; padding: 10px; min-height: 100px; margin-top: 10px;",
      assistant_response()
    )
  })
}

# --- Run the Application ---
shinyApp(ui = ui, server = server)
