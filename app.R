#
# Shiny Web Application for all_data_sf-490
#
library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(leaflet)
library(lubridate)
library(tmap)
# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
setwd("/cloud/project/Data")
all_data_sf <- st_read("all_data_sf.gpkg")
# ////////////////////////////////////////////////////////////////////////////
# UI 
# ////////////////////////////////////////////////////////////////////////////
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Housing and Equity Analysis in Clark County"),
  dashboardSidebar(
    selectInput("bedroomUnitSelect", "Select Bedroom Unit:",
                choices = unique(as.character(all_data_sf$BedroomUnits)),
                selected = unique(as.character(all_data_sf$BedroomUnits))[1]),
    selectInput("employmentStatus", "Select Employment Status:", 
                choices = c("Employed", "Unemployed", "Not in labor force")),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Data View", tabName = "tableView", icon = icon("table")),
      menuItem("Global Moran's I Analysis", tabName = "moransI", icon = icon("globe")),
      menuItem("Rent Prices", tabName = "rent", icon = icon("home")),
      menuItem("Income Analysis", tabName = "income", icon = icon("chart-bar")),
      menuItem("BIPOC", tabName = "bipoc", icon = icon("users")),
      menuItem("Employment Analysis", tabName = "employment", icon = icon("briefcase")),
      menuItem("SNAP Analysis", tabName = "snap", icon = icon("apple-alt")),
      menuItem("Rent Vacancy Analysis", tabName = "rentVacan", icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              box(title = "Overview", width = 12, 
                  HTML("<p>I will be analyzing rent prices in Clark County, WA by zip code using the Small Area Fair Market Rents (SAFMRs). 
                          I focused on market trends for renters from 2023, this data comes from the U.S. Department of Housing and Urban Development (HUD). 
                          I will be using different socio and economic indicators to take a closer look at the makeup of Clark County, WA. 
                          Median Household Income, Employment Status, Receipt of Food Stamps, Race, and Vacancy Status, this data comes from the American Community Survey (ACS).</p>
                        <p>The objective of this study is to conduct a comprehensive analysis that delves into the relationship between 
                       housing and equity. Ideally, Clark County should be envisioned as a place where residents have access to 
                       affordable housing.</p>
                         <p>Are regions with low poverty more likely to offer affordable housing? Is there a trend 
                       between escalating rent prices and a surge in poverty levels? Are BOPIC (Black, Indigenous, People of Color) 
                       individuals more likely to live in certain neighborhoods in Clark County?</p>
                         <p>First, I will employ the Global Moran’s I test, because it measures spatial autocorrelation, and will aid in 
                       determining whether the spatial pattern of a variable is clustered, dispersed, or random.</p>
                         ")
              )),
      tabItem(tabName = "moransI",
              box(title = "Global Moran's I Analysis", width = 12,
                  tableOutput("moransIOutput"))
      ),
      tabItem(tabName = "rent",
              box(title = "Rent Map", width = 12,
                  leafletOutput("rentMap"))),
      tabItem(tabName = "income",
              fluidRow(
                box(title = "Median Household Income Map", width = 6, plotOutput("medianIncomeMap")),
                box(title = "Income Tertile Map", width = 6, plotOutput("incomeTertileMap"))
              )),
      tabItem(tabName = "bipoc",
              fluidRow(
                box(title = "People of Color (%)", width = 6, plotOutput("bipocPercentageMap")),
                box(title = "BIPOC Tertile", width = 6, plotOutput("bipocTertileMap"))
              )),
      tabItem(tabName = "employment",
              fluidRow(
                box(title = "Employment Status Map", width = 6, leafletOutput("employmentStatusMap")),
                box(title = "Unemployment Tertile Map", width = 6, plotOutput("employmentTertileMap"))
              )),
      tabItem(tabName = "snap",
              fluidRow(
                box(title = "SNAP Map", width = 6, plotOutput("SnapMap")),
                box(title = "SNAP Tertile", width = 6, plotOutput("snapTertilePlot"))
              )),
      tabItem(tabName = "rentVacan",
              fluidRow(
                box(title = "Rent Vacancy Map", width = 6, plotOutput("VacancyMap")),
                box(title = "Rent Vacancy Tertile", width = 6, plotOutput("rentVacanTertilePlot"))
              )),
      tabItem(tabName = "tableView",
              box(title = "Data Table", width = 12, 
                  DTOutput("dataTable"))
      )
    )
  )
)
# Server
server <- function(input, output, session) {

  # Rent Plot
  output$rentMap <- renderLeaflet({
    # Check if the selected bedroom unit is in the all_data_sfset
    if(input$bedroomUnitSelect %in% all_data_sf$BedroomUnits) {
      filtered_all_data_sf <- all_data_sf %>%
        filter(BedroomUnits == input$bedroomUnitSelect) %>%
        st_transform(., crs = 4326)
      
      # Create and render the map using your custom function
      create_rent_map(filtered_all_data_sf, "Rent")
    } else {
      # Return an empty map if no valid selection is made
      leaflet() %>%
        addTiles()
    }
  })
  
  # Median Household Income Map
  output$medianIncomeMap <- renderPlot({
    tm_shape(Income_shapefile) + 
      tm_borders() + 
      tm_fill(col = "Median household income", 
              palette = "-viridis", 
              title = "Median Household Income",
              style = "quantile") + 
      tm_layout(legend.outside = TRUE, 
                legend.outside.position = "right", 
                legend.title.size = 1.2,
                legend.text.size = 1)
  })
            
 
  # Income Tertile Map
  output$incomeTertileMap <- renderPlot({
    all_data_sf$income_tertile <- factor(
      all_data_sf$income_tertile,
      levels = c(1, 2, 3),
      labels = c("Low Median Income", "Average Median Income", "High Median Income")
    )
    tm_shape(all_data_sf) +
      tm_borders() +
      tm_fill(
        col = "income_tertile",
        palette = "-viridis",
        title = "Income Tertile",
        style = "cat"
      ) +
      tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.2,
        legend.text.size = 1
      )
  })
  
  # People of Color (%) Map
  output$bipocPercentageMap <- renderPlot({
    tm_shape(all_data_sf) + 
      tm_borders() + 
      tm_fill(col = "People_of_Color", 
              palette = "-viridis", 
              title = "People of Color (%)",
              style = "quantile") + 
      tm_layout(legend.outside = TRUE, 
                legend.outside.position = "right", 
                legend.title.size = 1.2,
                legend.text.size = 1)
  })
  
  # BIPOC Tertile Map
  output$bipocTertileMap <- renderPlot({
    # Convert BIPOC_tertile to a factor with descriptive labels
    all_data_sf$BIPOC_tertile <- factor(
      all_data_sf$BIPOC_tertile,
      levels = c(1, 2, 3),
      labels = c("Low", "Average", "High")
    )
    
    tm_shape(all_data_sf) +
      tm_borders() +
      tm_fill(
        col = "BIPOC_tertile",
        palette = "-viridis",
        title = "BIPOC Tertile",
        style = "cat"  # Use categorical style for factor variable
      ) +
      tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.2,
        legend.text.size = 1
      )
  })
  
  # Employment Status Map
  output$employmentStatusMap <- renderLeaflet({
    # Call the create_map function based on the selected employment status
    selected_column <- switch(input$employmentStatus,
                              "Employed" = "Employed",
                              "Unemployed" = "Unemployed",
                              "Not in labor force" = "Not in labor force")
    
    create_map(Employment_shapefile, selected_column)
  })
  
  # Employment Tertile Map
  output$employmentTertileMap  <- renderPlot({
    # Convert Employment_tertile to a factor with descriptive labels
    all_data_sf$Employment_tertile <- factor(
      all_data_sf$Employment_tertile,
      levels = c(1, 2, 3),
      labels = c("Low", "Average", "High")
    )
    
    tm_shape(all_data_sf) +
      tm_borders() +
      tm_fill(
        col = "Employment_tertile",
        palette = "-viridis",
        title = "Unemployment Tertile",
        style = "cat"  # Use categorical style for factor variable
      ) +
      tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.2,
        legend.text.size = 1
      )
  })
  
  
  # SNAP Map
  output$SnapMap <- renderPlot({
    tm_shape(SNAP_shapefile) + 
      tm_borders() + 
      tm_fill(col = "Household received SNAP", 
              palette = "-viridis", 
              title = "Snap Distribution",
              style = "quantile") + 
      tm_layout(legend.outside = TRUE, 
                legend.outside.position = "right", 
                legend.title.size = 1.2,
                legend.text.size = 1)
  })
  
  # SNAP Tertile Map
  output$snapTertilePlot <- renderPlot({
    # Convert SNAP_tertile to a factor with descriptive labels
    all_data_sf$SNAP_tertile <- factor(
      all_data_sf$SNAP_tertile,
      levels = c(1, 2, 3),
      labels = c("Low", "Average", "High")
    )
    
    tm_shape(all_data_sf) +
      tm_borders() +
      tm_fill(
        col = "SNAP_tertile",
        palette = "-viridis",
        title = "SNAP Tertile",
        style = "cat"  # Use categorical style for factor variable
      ) +
      tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.2,
        legend.text.size = 1
      )
  })
  
  
  # Vacancy Map
  output$VacancyMap <- renderPlot({
    tm_shape(Vacan_shapefile) + 
      tm_borders() + 
      tm_fill(col = "For rent", 
              palette = "-viridis", 
              title = "Units available for Rent",
              style = "quantile") + 
      tm_layout(legend.outside = TRUE, 
                legend.outside.position = "right", 
                legend.title.size = 1.2,
                legend.text.size = 1)
  })
  
  
  output$rentVacanTertilePlot <- renderPlot({
    # Convert RentVacan_tertile to a factor with descriptive labels
    all_data_sf$RentVacan_tertile <- factor(
      all_data_sf$RentVacan_tertile,
      levels = c(1, 2, 3),
      labels = c("Low", "Average", "High")
    )
    
    tm_shape(all_data_sf) +
      tm_borders() +
      tm_fill(
        col = "RentVacan_tertile",
        palette = "-viridis",
        title = "Rent Vacancy Tertile",
        style = "cat"  # Use categorical style for factor variable
      ) +
      tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.title.size = 1.2,
        legend.text.size = 1
      )
  })
  
  
  # Calculate Global Moran's I for various variables
  output$moransIOutput <- renderText({
    neighbors <- poly2nb(all_data_sf)
    weights <- nb2listw(neighbors, style = "W")
    
    moran_test_rent <- moran.test(all_data_sf$Rent, weights)
    income_moran <- moran.test(all_data_sf$income_tertile, weights)
    moran_test_BIPOC <- moran.test(all_data_sf$BIPOC_tertile, weights)
    SNAP_moran <- moran.test(all_data_sf$SNAP_tertile, weights)
    unemployment_moran <- moran.test(all_data_sf$Employment_tertile, weights)
    vacancy_moran <- moran.test(all_data_sf$RentVacan_tertile, weights)
    
    paste(
      "Global Moran's I for Rent:\nI: ", moran_test_rent$I, "\n\nP-value: ", moran_test_rent$p.value,
      "\n\n------------------------------------------------\n\nGlobal Moran's I for Income Tertile:\nI: ", income_moran$I, "\n\nP-value: ", income_moran$p.value,
      "\n\n------------------------------------------------\n\nGlobal Moran's I for BIPOC Tertile:\nI: ", moran_test_BIPOC$I, "\n\nP-value: ", moran_test_BIPOC$p.value,
      "\n\n------------------------------------------------\n\nGlobal Moran's I for SNAP Tertile:\nI: ", SNAP_moran$I, "\n\nP-value: ", SNAP_moran$p.value,
      "\n\n------------------------------------------------\n\nGlobal Moran's I for Unemployment Tertile:\nI: ", unemployment_moran$I, "\n\nP-value: ", unemployment_moran$p.value,
      "\n\n------------------------------------------------\n\nGlobal Moran's I for Rent Vacancy Tertile:\nI: ", vacancy_moran$I, "\n\nP-value: ", vacancy_moran$p.value,
      sep = ""
    )
  })
  
  #  data table 
  output$dataTable <- renderDT({
    datatable(Income_dataframe, options = list(pageLength = 10, scrollX = TRUE))
  })

  
}


# Run the app
shinyApp(ui = ui, server = server)