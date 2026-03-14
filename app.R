library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)

# Load data
parks_df <- read.csv("data/raw/parks.csv", sep=";")

min_hectare <- min(parks_df$Hectare, na.rm=TRUE)
max_hectare <- max(parks_df$Hectare, na.rm=TRUE)

ui <- page_sidebar(
  title = div(
    class = "d-flex justify-content-between align-items-center w-100",
    "Vancouver Park Dashboard",
    actionButton("reset", "Reset Filters", class = "btn-danger btn-sm", style = "position: absolute; right: 20px; top: 15px; z-index: 1000;")
  ),
  fillable = TRUE,
  
  # Theme and Colors
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#285F2A",
    secondary = "#4d8a50"
  ) |> 
    bs_add_rules("
      /* Borders for each tile */
      .card { border: 2px solid #285F2A !important; border-radius: 8px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); }
      /* Background color for left side filter panel */
      .bslib-sidebar-layout > .sidebar { background-color: #e8f5e9 !important; border-right: 2px solid #285F2A !important; }
      /* Background color for right output side */
      body { background-color: #fcfcfc !important; }
    "),
  
  sidebar = sidebar(
    title = "Filters",
    textInput("search", "Search Parks", placeholder="Enter keywords..."),
    selectizeInput(
      "neighbourhood", 
      "Neighbourhood", 
      choices = sort(unique(na.omit(parks_df$NeighbourhoodName))),
      multiple = TRUE
    ),
    sliderInput(
        "size", 
        "Hectare", 
        # Limits dynamically match the data!
        min = min_hectare, 
        max = max_hectare, 
        value = c(min_hectare, max_hectare)
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
    card_header("Park Overview", class = "bg-primary text-white"),
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
  
  # Reset Button Logic
  observeEvent(input$reset, {
    updateTextInput(session, "search", value = "")
    updateSelectizeInput(session, "neighbourhood", selected = character(0))
    updateSliderInput(session, "size", value = c(min_hectare, max_hectare))
    updateCheckboxGroupInput(session, "facilities", selected = character(0))
  })
  
  # Reactive Data Filter
  filtered <- reactive({
    df <- parks_df
    
    if (isTruthy(input$search)) {
      df <- df |> filter(grepl(input$search, Name, ignore.case = TRUE))
    }
    
    if (isTruthy(input$neighbourhood)) {
      df <- df |> filter(NeighbourhoodName %in% input$neighbourhood)
    }
    
    df <- df |> filter(Hectare >= input$size[1] & Hectare <= input$size[2])
    
    if (isTruthy(input$facilities)) {
      for (facility in input$facilities) {
        df <- df |> filter(.data[[facility]] == "Y")
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
      URL = ifelse(!is.na(df$NeighbourhoodURL) & df$NeighbourhoodURL != "", 
                   paste0('<a href="', df$NeighbourhoodURL, '" target="_blank">', df$NeighbourhoodURL, '</a>'), 
                   "")
    )
    datatable(display_df, escape = FALSE, options = list(pageLength = 5, dom = 'ftp'))
  })
  
  output$park_map <- renderLeaflet({
    df <- filtered()
    
    m <- leaflet() |>
      addTiles()
      
    if (nrow(df) > 0) {
      valid_coords <- df |> 
        filter(!is.na(GoogleMapDest) & GoogleMapDest != "") |>
        rowwise() |>
        mutate(
          lat = as.numeric(strsplit(as.character(GoogleMapDest), ",")[[1]][1]),
          lon = as.numeric(strsplit(as.character(GoogleMapDest), ",")[[1]][2])
        ) |>
        ungroup()
        
      if (nrow(valid_coords) > 0) {
        m <- m |> addMarkers(
          data = valid_coords,
          lng = ~lon, lat = ~lat,
          popup = ~paste0("<b>", Name, "</b><br>Neighbourhood: ", NeighbourhoodName, "<br>Size: ", Hectare, " ha")
        )
        
        # Dynamically fit the bounds of the map to the active markers
        if (nrow(valid_coords) == 1) {
          m <- m |> setView(lng = valid_coords$lon[1], lat = valid_coords$lat[1], zoom = 15)
        } else {
          m <- m |> fitBounds(
            lng1 = min(valid_coords$lon), lat1 = min(valid_coords$lat),
            lng2 = max(valid_coords$lon), lat2 = max(valid_coords$lat)
          )
        }
      } else {
        # Fallback if no valid coords found but dataframe has rows
        m <- m |> setView(lng = -123.1207, lat = 49.2827, zoom = 14)
      }
    } else {
      # Fallback for empty dataframe (e.g. no results match criteria)
      m <- m |> setView(lng = -123.1207, lat = 49.2827, zoom = 14)
    }
    
    m
  })
  
  output$washroom_chart <- renderPlotly({
    all_counts <- parks_df |>
      filter(Washrooms == "Y") |>
      group_by(NeighbourhoodName) |>
      summarise(Count = n(), .groups = 'drop')
      
    selected_neighs <- input$neighbourhood
    
    all_counts <- all_counts |>
      mutate(
        Color = ifelse(is.null(selected_neighs) || length(selected_neighs) == 0 || NeighbourhoodName %in% selected_neighs,
                       "#285F2A", 
                       "#bdbdbd"  
                      )
      )
      
    avg <- mean(all_counts$Count, na.rm=TRUE)
    
    p <- plot_ly(
      data = all_counts,
      x = ~NeighbourhoodName,
      y = ~Count,
      type = 'bar',
      marker = list(color = ~Color)
    ) |>
    layout(
      xaxis = list(
        title = list(text = "Neighbourhood", font = list(size=14)), 
        tickangle = -45,
        tickfont = list(size=10)
      ),
      yaxis = list(title = list(text = "Total Washrooms", font = list(size=14))),
      showlegend = FALSE,
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
      margin = list(b = 100)
    )
    
    p
  })
}

shinyApp(ui, server)
