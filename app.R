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
         card_header("Washroom availability"),
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
      URL = df$NeighbourhoodURL
    )
    datatable(display_df, options = list(pageLength = 5, dom = 'ftp'))
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
    df <- filtered()
    
    counts <- df %>%
      count(Washrooms) %>%
      mutate(Washrooms = ifelse(Washrooms == "Y", "Yes", "No"))
      
    p <- plot_ly(counts, labels = ~Washrooms, values = ~n, type = 'pie',
                 textinfo = 'label+percent',
                 marker = list(colors = c("darkgreen", "lightgreen"))) %>%
         layout(showlegend = TRUE)
    p
  })
}

shinyApp(ui, server)
