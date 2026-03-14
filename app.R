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


