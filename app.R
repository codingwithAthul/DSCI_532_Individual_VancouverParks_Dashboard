library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)

# Load data
parks_df <- read.csv("data/raw/parks.csv", sep=";")

ui <- page_sidebar(
  title = "Vancouver Park Dashboard",
  fillable = TRUE,
  
  sidebar = sidebar(
    title = "Filters",
    textInput("search", "Search Parks", placeholder="Enter keywords..."),
    selectizeInput(
      "neighbourhood", 
      "Neighbourhood", 
      choices = sort(unique(na.omit(parks_df$NeighbourhoodName))),
      multiple = TRUE
    ),
    # Fixed: the slider range and defaults update to exactly max/min of the dataframe
    sliderInput(
      "size", 
      "Hectare", 
      min = min(parks_df$Hectare, na.rm=TRUE), 
      max = max(parks_df$Hectare, na.rm=TRUE), 
      value = c(min(parks_df$Hectare, na.rm=TRUE), max(parks_df$Hectare, na.rm=TRUE))
    ),
    checkboxGroupInput(
      "facilities",
      "Select Facilities",
      choices = c(
        "Washrooms" = "Washrooms",
        "Facilities" = "Facilities",
        "Special Features" = "SpecialFeatures" 
      )
    ),
    hr(),
    markdown("Adjust filters to update the charts.")
  ),
  
  card(
    card_header("Park Overview"),
    layout_column_wrap(
      width = 1/2,
      height = "350px",
      card(
         card_header("Table of data"),
         DTOutput("table_out")
      ),
      card(
         card_header("Washroom availability by Neighbourhood"),
         plotlyOutput("washroom_chart")
      )
    ),
    card(
      card_header("Map"),
      leafletOutput("park_map"),
      full_screen = TRUE
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive Data Filter
  filtered <- reactive({
    df <- parks_df
    
    if (isTruthy(input$search)) {
      df <- df %>% filter(grepl(input$search, Name, ignore.case = TRUE))
    }
    
    if (isTruthy(input$neighbourhood)) {
      df <- df %>% filter(NeighbourhoodName %in% input$neighbourhood)
    }
    
    df <- df %>% filter(Hectare >= input$size[1] & Hectare <= input$size[2])
    
    if (isTruthy(input$facilities)) {
      for (facility in input$facilities) {
        # Using `.data[[facility]]` handles the dynamic column string
        df <- df %>% filter(.data[[facility]] == "Y")
      }
    }
    
    df
  })
  
  # Outputs
  output$table_out <- renderDT({
    df <- filtered()
    display_df <- data.frame(
      Name = df$Name,
      Address = paste(df$StreetNumber, df$StreetName),
      Neighbourhood = df$NeighbourhoodName,
      # Convert URLs to clickable tags
      URL = ifelse(!is.na(df$NeighbourhoodURL) & df$NeighbourhoodURL != "", 
                   paste0('<a href="', df$NeighbourhoodURL, '" target="_blank">', df$NeighbourhoodURL, '</a>'), 
                   "")
    )
    # Important: escape=FALSE is needed to render the HTML tags in the table
    datatable(display_df, escape = FALSE, options = list(pageLength = 5, dom = 'ftp'))
  })
  
  output$park_map <- renderLeaflet({
    df <- filtered()
    
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = -123.1207, lat = 49.2827, zoom = 12)
      
    if (nrow(df) > 0) {
      # Extract lat/lon safely
      valid_coords <- df %>% 
        filter(!is.na(GoogleMapDest) & GoogleMapDest != "") %>%
        rowwise() %>%
        mutate(
          lat = as.numeric(strsplit(as.character(GoogleMapDest), ",")[[1]][1]),
          lon = as.numeric(strsplit(as.character(GoogleMapDest), ",")[[1]][2])
        ) %>%
        ungroup()
        
      if (nrow(valid_coords) > 0) {
        m <- m %>% addMarkers(
          data = valid_coords,
          lng = ~lon, lat = ~lat,
          popup = ~paste0("<b>", Name, "</b><br>Neighbourhood: ", NeighbourhoodName, "<br>Size: ", Hectare, " ha")
        )
      }
    }
    
    m
  })
  
  output$washroom_chart <- renderPlotly({
    # We want to show a bar chart of washrooms per neighbourhood based on ALL parks
    # But we want to highlight the neighbourhoods that are currently selected in the filter.
    
    # 1. Total washrooms per neighbourhood across ALL parks
    all_counts <- parks_df %>%
      filter(Washrooms == "Y") %>%
      group_by(NeighbourhoodName) %>%
      summarise(Count = n(), .groups = 'drop')
      
    # 2. Extract selected neighbourhoods from the dropdown input
    selected_neighs <- input$neighbourhood
    
    # 3. Determine colors. Light red if selected (or if nothing is selected), grey otherwise
    all_counts <- all_counts %>%
      mutate(
        Color = ifelse(is.null(selected_neighs) || length(selected_neighs) == 0 || NeighbourhoodName %in% selected_neighs,
                       "#285F2A", # Green for active selection
                       "#bdbdbd"  # Grey out otherwise
                      )
      )
      
    # 4. Average washroom count
    avg <- mean(all_counts$Count, na.rm=TRUE)
    
    # 5. Plotly Bar Chart
    p <- plot_ly(
      data = all_counts,
      x = ~NeighbourhoodName,
      y = ~Count,
      type = 'bar',
      marker = list(color = ~Color)
    ) %>%
    layout(
      xaxis = list(
        title = list(text = "Neighbourhood", font = list(size=14)), 
        tickangle = -45,
        tickfont = list(size=10)
      ),
      yaxis = list(title = list(text = "Total Washrooms", font = list(size=14))),
      showlegend = FALSE,
      # Add horizontal dotted average line
      shapes = list(
        list(
          type = "line",
          y0 = avg,
          y1 = avg,
          x0 = 0,
          x1 = 1,
          xref = "paper",
          line = list(color = "red", dash = "dot", width = 2)
        )
      ),
      margin = list(b = 100) # Give space for x-axis labels
    )
    
    p
  })
}

shinyApp(ui, server)
