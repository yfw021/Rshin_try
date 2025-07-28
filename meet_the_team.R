library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(httr)      # For making API requests
library(jsonlite)  # For handling JSON data
library(tidyr)     # For reshaping data for the plot
library(leaflet)   # For the interactive map

# --- Sample Data ---
# Added latitude and longitude for each machine.
df <- data.frame(
  stringsAsFactors = FALSE,
  machine = c("Machine A", "Machine B", "Machine C", "Machine D", "Machine E", "Machine F", "Machine G", "Machine H"),
  type = c("Type A", "Type A", "Type B", "Type B", "Type C", "Type C", "Type A", "Type B"),
  region = c("TX", "NM", "TX", "NM+TX", "TX", "NM", "NM+TX", "TX"),
  auto = c(1, 0, 1, 1, 0, 1, 0, 1),
  lat = c(31.7619, 35.0844, 32.7767, 33.4484, 29.4241, 32.3199, 34.5199, 30.2672), # NEW: Latitude
  lon = c(-106.4850, -106.6504, -96.7970, -104.2288, -98.4936, -106.7637, -105.8701, -97.7431), # NEW: Longitude
  Capacity = c(100, 150, 120, 200, 90, 110, 180, 140),
  First_year = c(2010, 2015, 2020, 2027, 2012, 2018, 2022, 2028),
  Last_Year = c(2025, 2030, 2035, 2040, 2028, 2032, 2038, 2042)
)

# --- User Interface (UI) ---
ui <- navbarPage(
  "Machine Capacity Dashboard",
  
  # --- Introduction Page (MODIFIED) ---
  tabPanel("Introduction",
           fluidPage(
             h2("Welcome to the Capacity Projection Tool"),
             p("This interactive dashboard provides a comprehensive platform for visualizing and analyzing machine capacity data. You can explore projections of future capacity based on each machine's operational lifespan. Use the powerful filtering tools on the 'Dashboard' page to narrow down the data by region, machine type, and other attributes. The key features include an interactive map showing machine locations, a dynamic plot of total capacity over time, a detailed data table, and an integrated AI Assistant ready to answer your questions about the data."),
             hr(),
             h3("Vocabulary"),
             tags$dl(
               tags$dt("Capacity"), tags$dd("The maximum output or production ability of a machine."),
               tags$dt("Projection"), tags$dd("A forecast of future capacity based on the defined operational years (First_year to Last_Year) of the machines."),
               tags$dt("Region"), tags$dd("The geographical area where a machine is located, such as 'TX' (Texas) or 'NM' (New Mexico)."),
               tags$dt("Type"), tags$dd("The specific classification or model of a machine (e.g., 'Type A', 'Type B')."),
               tags$dt("Auto"), tags$dd("A binary feature indicating a specific machine attribute (1 for 'Yes', 0 for 'No')."),
               tags$dt("AI Assistant"), tags$dd("An integrated tool that uses a large language model to answer natural language questions about the data currently displayed on the dashboard.")
             ),
             
             # =====================================================
             # == NEW TEAM MEMBER SECTION START
             # =====================================================
             hr(),
             h3("Meet the Team"),
             p("Our team of dedicated analysts and developers brings this dashboard to life."),
             br(),
             
             # --- Team Row 1 ---
             fluidRow(
               column(4, align = "center",
                      tags$img(src = "member1.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Dr. Evelyn Reed"),
                      p(tags$em("Lead Data Scientist")),
                      p("Evelyn designed the projection models and leads the analytics strategy.")
               ),
               column(4, align = "center",
                      tags$img(src = "member2.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Mark Chen"),
                      p(tags$em("Senior R/Shiny Developer")),
                      p("Mark is the primary architect of this interactive Shiny application.")
               ),
               column(4, align = "center",
                      tags$img(src = "member3.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Sofia Rodriguez"),
                      p(tags$em("UI/UX Specialist")),
                      p("Sofia is responsible for the user-friendly design and layout of the dashboard.")
               )
             ),
             br(), # Adds vertical space between rows
             
             # --- Team Row 2 ---
             fluidRow(
               column(4, align = "center",
                      tags$img(src = "member4.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Ben Carter"),
                      p(tags$em("Data Engineer")),
                      p("Ben manages the data pipelines and ensures data integrity and performance.")
               ),
               column(4, align = "center",
                      tags$img(src = "member5.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Aisha Khan"),
                      p(tags$em("AI Integration Lead")),
                      p("Aisha implemented the AI Assistant feature and works on advanced analytics.")
               ),
               column(4, align = "center",
                      tags$img(src = "member6.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Carlos Gomez"),
                      p(tags$em("Geospatial Analyst")),
                      p("Carlos developed the interactive mapping components of the dashboard.")
               )
             ),
             br(), # Adds vertical space between rows
             
             # --- Team Row 3 ---
             fluidRow(
               column(4, align = "center",
                      tags$img(src = "member7.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Olivia White"),
                      p(tags$em("Project Manager")),
                      p("Olivia oversees the project timeline, resources, and stakeholder communication.")
               ),
               column(4, align = "center",
                      tags$img(src = "member8.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("David Lee"),
                      p(tags$em("Junior Analyst")),
                      p("David supports the team with data validation and scenario testing.")
               ),
               column(4, align = "center",
                      tags$img(src = "member9.png", height = "150px", width = "150px", style = "border-radius: 50%; object-fit: cover; border: 3px solid #ddd;"),
                      h4("Jasmine Kaur"),
                      p(tags$em("Quality Assurance")),
                      p("Jasmine ensures the application is bug-free and meets all requirements.")
               )
             )
             # =====================================================
             # == NEW TEAM MEMBER SECTION END
             # =====================================================
             
           )
  ),
  
  # --- Main Dashboard Page ---
  tabPanel("Dashboard",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Controls"),
               selectInput("region_select", "Filter by Region:", choices = c("All", "TX", "NM", "NM+TX")),
               selectInput("type_select", "Filter by Type:", choices = c("All", unique(df$type))),
               selectInput("auto_select", "Filter by Auto:", choices = c("All", "Yes" = 1, "No" = 0)),
               sliderInput("projection_years_slider", "Select Projection Year Range:", min = 2020, max = 2050, value = c(2026, 2040), sep = ""),
               hr(),
               h4("AI Assistant"),
               p("Ask a question about the data.", style = "font-size: 0.9em; color: grey;"),
               passwordInput("api_key_input", "Enter your Gemini API Key:"),
               textInput("user_question", "Your Question:", placeholder = "e.g., Which machine has most capacity?"),
               actionButton("ask_button", "Ask Assistant"),
               h5("Assistant's Response:"),
               uiOutput("assistant_response_ui")
             ),
             mainPanel(
               width = 9,
               fluidRow(
                 # UPDATED: Resized columns to fit three plots
                 column(4, h4("Machine Locations Map"), leafletOutput("capacity_map")),
                 column(4, h4("Capacity by Type Plot"), plotOutput("capacity_plot")),
                 # NEW: Added histogram plot
                 column(4, h4("Capacity Distribution"), plotOutput("histogram_plot"))
               ),
               hr(), h4("Detailed Projection Table"), DTOutput("capacity_table")
             )
           )
  ),
  
  # --- Scenario Comparison Page ---
  tabPanel("Scenario Comparison",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Define a Scenario"),
               p("Set the filters below and click 'Add Scenario' to add it to the comparison list."),
               selectInput("scen_region", "Region:", choices = c("All", "TX", "NM", "NM+TX")),
               selectInput("scen_type", "Type:", choices = c("All", unique(df$type))),
               selectInput("scen_auto", "Auto:", choices = c("All", "Yes" = 1, "No" = 0)),
               actionButton("add_scenario_btn", "Add Scenario", icon = icon("plus")),
               hr(),
               h4("Current Scenarios"),
               uiOutput("scenario_list_ui"),
               br(),
               actionButton("clear_scenarios_btn", "Clear All Scenarios", icon = icon("trash"))
             ),
             mainPanel(
               width = 9,
               h3("Capacity Projections by Scenario"),
               p("This plot shows the total projected capacity for each defined scenario over the selected time period."),
               plotOutput("scenario_comparison_plot", height = "600px")
             )
           )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # =====================================================
  # == Main Dashboard Logic
  # =====================================================
  
  # --- Reactive Data for Main Dashboard ---
  filtered_data <- reactive({
    filtered <- df
    if (input$region_select != "All") { filtered <- filtered %>% filter(region == input$region_select) }
    if (input$type_select != "All") { filtered <- filtered %>% filter(type == input$type_select) }
    if (input$auto_select != "All") { filtered <- filtered %>% filter(auto == input$auto_select) }
    return(filtered)
  })
  
  projected_data <- reactive({
    data_in <- filtered_data()
    projection_years <- input$projection_years_slider[1]:input$projection_years_slider[2]
    if (nrow(data_in) == 0) return(data.frame())
    for (year in projection_years) {
      col_name <- as.character(year)
      data_in <- data_in %>% mutate(!!col_name := ifelse(Last_Year >= year & First_year <= year, Capacity, 0))
    }
    data_in %>% select(machine, type, region, auto, lat, lon, everything())
  })
  
  output$capacity_table <- renderDT({
    req(nrow(projected_data()) > 0)
    display_data <- projected_data() %>% select(-lat, -lon)
    datatable(display_data, extensions = 'FixedColumns', options = list(scrollX = TRUE, pageLength = 10, fixedColumns = list(leftColumns = 4)), rownames = FALSE, class = 'cell-border stripe')
  })
  
  output$capacity_plot <- renderPlot({
    plot_data <- projected_data()
    req(nrow(plot_data) > 0)
    year_cols <- as.character(input$projection_years_slider[1]:input$projection_years_slider[2])
    summary_data <- plot_data %>% select(type, all_of(year_cols)) %>% pivot_longer(cols = -type, names_to = "Year", values_to = "Capacity") %>% group_by(Year, type) %>% summarise(TotalCapacity = sum(Capacity, na.rm = TRUE), .groups = 'drop') %>% mutate(Year = as.numeric(Year))
    total_labels <- summary_data %>% group_by(Year) %>% summarise(TotalLabel = sum(TotalCapacity, na.rm = TRUE))
    ggplot(summary_data, aes(x = Year, y = TotalCapacity, fill = type)) + geom_col(position = "stack") + geom_text(data = total_labels, aes(x = Year, y = TotalLabel, label = TotalLabel), inherit.aes = FALSE, vjust = -0.5, color = "black", size = 3) + labs(title = paste("Total Capacity for:", input$region_select, "Region"), x = "Year", y = "Total Capacity", fill = "Machine Type") + theme_minimal(base_size = 12) + scale_fill_brewer(palette = "Pastel1") + scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + scale_y_continuous(limits = c(0, max(total_labels$TotalLabel, na.rm = TRUE) * 1.15))
  })
  
  output$capacity_map <- renderLeaflet({
    map_data <- filtered_data()
    req(nrow(map_data) > 0)
    pal <- colorFactor(palette = "Pastel1", domain = map_data$type)
    leaflet(map_data) %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -100, lat = 32, zoom = 5) %>% addCircleMarkers(lng = ~lon, lat = ~lat, color = ~pal(type), stroke = FALSE, fillOpacity = 0.8, radius = 8, popup = ~paste("<b>Machine:</b>", machine, "<br><b>Type:</b>", type, "<br><b>Region:</b>", region, "<br><b>Capacity:</b>", Capacity)) %>% addLegend("bottomright", pal = pal, values = ~type, title = "Machine Type", opacity = 1)
  })
  
  # --- NEW: Histogram Plot Output ---
  output$histogram_plot <- renderPlot({
    plot_data <- filtered_data()
    req(nrow(plot_data) > 0)
    
    ggplot(plot_data, aes(x = Capacity, fill = type)) +
      geom_histogram(binwidth = 20, alpha = 0.7, position = "identity") +
      labs(
        title = "Distribution of Capacity by Type",
        x = "Capacity",
        y = "Number of Machines",
        fill = "Machine Type"
      ) +
      theme_minimal(base_size = 12) +
      scale_fill_brewer(palette = "Pastel1")
  })
  
  # =====================================================
  # == Scenario Comparison Logic
  # =====================================================
  
  scenarios <- reactiveVal(list())
  
  observeEvent(input$add_scenario_btn, {
    current_scenarios <- scenarios()
    scen_name <- paste("Scenario", length(current_scenarios) + 1)
    new_scen <- list(
      name = scen_name,
      region = input$scen_region,
      type = input$scen_type,
      auto = input$scen_auto
    )
    scenarios(append(current_scenarios, list(new_scen)))
  })
  
  observeEvent(input$clear_scenarios_btn, {
    scenarios(list())
  })
  
  output$scenario_list_ui <- renderUI({
    scen_list <- scenarios()
    if (length(scen_list) == 0) {
      return(p("No scenarios added yet.", style = "color: grey;"))
    }
    tags$ul(
      lapply(scen_list, function(s) {
        tags$li(paste0(s$name, ": Region=", s$region, ", Type=", s$type, ", Auto=", s$auto))
      })
    )
  })
  
  comparison_data <- reactive({
    req(length(scenarios()) > 0)
    
    all_scen_data <- list()
    projection_years <- input$projection_years_slider[1]:input$projection_years_slider[2]
    
    for (scen in scenarios()) {
      filtered <- df
      if (scen$region != "All") { filtered <- filtered %>% filter(region == scen$region) }
      if (scen$type != "All") { filtered <- filtered %>% filter(type == scen$type) }
      if (scen$auto != "All") { filtered <- filtered %>% filter(auto == scen$auto) }
      
      if (nrow(filtered) > 0) {
        for (year in projection_years) {
          col_name <- as.character(year)
          filtered <- filtered %>% mutate(!!col_name := ifelse(Last_Year >= year & First_year <= year, Capacity, 0))
        }
        
        yearly_totals <- filtered %>%
          select(all_of(as.character(projection_years))) %>%
          colSums() %>%
          as.data.frame() %>%
          rename(TotalCapacity = ".") %>%
          tibble::rownames_to_column("Year") %>%
          mutate(Year = as.numeric(Year), Scenario = scen$name)
        
        all_scen_data <- append(all_scen_data, list(yearly_totals))
      }
    }
    
    bind_rows(all_scen_data)
  })
  
  output$scenario_comparison_plot <- renderPlot({
    plot_data <- comparison_data()
    req(nrow(plot_data) > 0)
    
    ggplot(plot_data, aes(x = Year, y = TotalCapacity, color = Scenario, group = Scenario)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      labs(
        title = "Scenario Comparison: Total Capacity Over Time",
        x = "Year",
        y = "Total Projected Capacity",
        color = "Scenario"
      ) +
      theme_minimal(base_size = 16) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_color_brewer(palette = "Set1")
  })
  
  # =====================================================
  # == AI Assistant Logic
  # =====================================================
  assistant_response <- reactiveVal("Awaiting your question...")
  observeEvent(input$ask_button, {
    req(input$api_key_input, input$user_question, nrow(projected_data()) > 0)
    showNotification("Asking the assistant...", type = "message", duration = 3)
    data_context <- capture.output(write.csv(projected_data() %>% select(-lat, -lon), row.names = FALSE))
    data_context_string <- paste(data_context, collapse = "\n")
    auto_filter_text <- if (input$auto_select == "All") "All" else if (input$auto_select == 1) "Yes" else "No"
    full_prompt <- paste("You are a helpful data analyst. The user is seeing data filtered by Region: '", input$region_select, "', Type: '", input$type_select, "', and by Auto: '", auto_filter_text, "'.", "Here is the data table:", "--- DATA START ---", data_context_string, "--- DATA END ---", "The user asked: ", input$user_question, "Answer based *only* on the data provided.")
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
    tags$div(style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 5px; padding: 10px; min-height: 100px; margin-top: 10px;", assistant_response())
  })
}

# --- Run the Application ---
shinyApp(ui = ui, server = server)

