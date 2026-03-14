library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)

# Load data
parks_df <- read.csv("data/raw/parks.csv", sep=";")

