# Vancouver Park Dashboard (R Version)

## Purpose
This application is a dashboard designed to help users explore and discover parks in Vancouver, BC. It was originally built in Python as part of a group project and has been re-implemented in R using the Shiny framework for this individual assignment. Users can search for parks by name, filter by specific neighbourhoods, filter by park size (hectares), and specify required facilities (like washrooms or accessability). The dashboard provides an interactive data table, a dynamic bar chart of washroom availability per neighbourhood, and a dynamic map utilizing Leaflet to visualize the exact geographic locations of the filtered parks.

## Installation and Local Setup

To run this application locally on your machine, you must have R and RStudio installed.

1. Clone this repository to your local machine:
   ```bash
   git clone https://github.com/codingwithAthul/DSCI_532_Individual_VancouverParks_Dashboard.git
   cd DSCI_532_Individual_VancouverParks_Dashboard
   ```

2. Open R or RStudio and install the required package dependencies by running:
   ```R
   install.packages(c("shiny", "bslib", "dplyr", "DT", "leaflet", "plotly", "rsconnect"))
   ```

3. Run the application:
   ```R
   shiny::runApp("app.R")
   ```

## Deployment

The application is deployed on Posit Connect Cloud. You can view the live dashboard here: 
[https://connect.posit.cloud/codingwithathul/content/019cedc9-756f-427e-9971-b057dade9d62](https://connect.posit.cloud/codingwithathul/content/019cedc9-756f-427e-9971-b057dade9d62)
