#
# Shiny Web Application for all_data_sf-490
#
library(shiny)
library(sf)
library(spdep)
library(shinydashboard)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(viridis)
library(leaflet)
library(lubridate)
library(tmap)
library(rsconnect)
# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
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
                         <p>I will employ the Global Moran’s I test, because it measures spatial autocorrelation, and will aid in 
                       determining whether the spatial pattern of a variable is clustered, dispersed, or random. I employed a tertile-based categorization strategy for the zipcode data, delineating values as High, Medium,
and Low, with the exception of rent. This approach facilitates a nuanced analysis of spatial autocorrelation
across various demographic and economic variables</p>
                         ")
              )
      ),
      tabItem(tabName = "moransI",
              box(title = "Global Moran's I Analysis", width = 12,
                  tableOutput("moransIOutput"),
                  HTML("<div style='height: 20px;'></div>"),
                  HTML("<p><strong>The spatial distribution analysis in Clark County reveals distinct socioeconomic patterns.</strong> Notably, the downtown area and older neighborhoods, typically associated with higher living costs, show an unexpected concentration of lower-income levels. This pattern could suggest a legacy of affordable housing options in these central zones or a transition due to gentrification, where rising desirability among higher-income groups is altering the economic landscape.</p>

<p><strong>Surrounding this low-income core, there’s an apparent encroachment from more affluent areas, which could be indicative of socio-economic disparities widening as higher-income populations move closer to the city center.</strong> There’s a notable aggregation of high employment, BIPOC presence, and SNAP utilization within these low-income regions. This juxtaposition might reflect job availability in sectors that do not offer substantial wages, or it may highlight vibrant community networks that persist despite economic challenges.</p>

<p><strong>Further, the analysis identifies a scarcity of rental options in the periphery of Vancouver, WA, including zones not characterized by high incomes.</strong> This could point to a predominance of owner-occupied housing and a potential deficit in rental infrastructure, potentially affecting housing market dynamics and accessibility.</p>")
                  
                  
              )
      ),
      tabItem(tabName = "rent",
              box(title = "Rent Map", width = 12,
                  leafletOutput("rentMap"),
                  HTML("<p><strong>Rent Results:</strong> No significant spatial autocorrelation (Moran’s I: -0.0096, p-value: 0.4916). Rent values are randomly distributed, which doesn’t provide direct insight into the relationship between rent prices and poverty levels. This suggests the need for further analysis, possibly examining temporal trends in rent prices and their impact on poverty.</p>")
                  
              )
      ),
      # Income Analysis tab
      tabItem(tabName = "income",
              fluidRow(
                box(title = "Median Household Income Map", width = 6, plotOutput("medianIncomeMap")),
                box(title = "Income Tertile Map", width = 6, plotOutput("incomeTertileMap"))
              ),
              HTML("<p><strong>Income Tertiles Results:</strong> Strong spatial clustering (Moran’s I: 0.4578, p-value < 2.2e-16). This indicates geographically concentrated areas with similar income levels. This suggests that regions of Clark County are economically segregated. While it doesn’t directly answer the question about the link between poverty and affordable housing, it highlights the need to explore if these high or low-income clusters correlate with housing affordability.</p>")
      ),
      
      # BIPOC tab
      tabItem(tabName = "bipoc",
              fluidRow(
                box(title = "People of Color (%)", width = 6, plotOutput("bipocPercentageMap")),
                box(title = "BIPOC Tertile", width = 6, plotOutput("bipocTertileMap"))
              ),
              HTML("<p><strong>BIPOC Tertiles Results:</strong> Significant spatial concentration (Moran’s I: 0.5930, p-value < 2.2e-16). Shows that BIPOC populations are likely to be clustered in specific neighborhoods. This directly addresses your question about the spatial distribution of BIPOC individuals in Clark County, suggesting that they are more likely to reside in certain areas.</p>")
      ),
      
      # Employment Analysis tab
      tabItem(tabName = "employment",
              fluidRow(
                box(title = "Employment Status Map", width = 6, leafletOutput("employmentStatusMap")),
                box(title = "Unemployment Tertile Map", width = 6, plotOutput("employmentTertileMap"))
              ),
              HTML("<p><strong>Employment Tertiles Results:</strong> Substantial clustering (Moran’s I: 0.3154, p-value < 2.2e-16). Indicates non-random distribution of employment rates, which might correlate with economic conditions like poverty and housing affordability. However, a direct link to your research questions requires further analysis.</p>")
      ),
      
      # SNAP Analysis tab
      tabItem(tabName = "snap",
              fluidRow(
                box(title = "SNAP Map", width = 6, plotOutput("SnapMap")),
                box(title = "SNAP Tertile", width = 6, plotOutput("snapTertilePlot"))
              ),
              HTML("<p><strong>SNAP Tertiles Results:</strong> Notable spatial clustering (Moran’s I: 0.3956, p-value < 2.2e-16). The clustering of SNAP usage may indicate areas with higher poverty or lower income. This indirectly contributes to understanding the distribution of poverty and its potential relationship with housing affordability.</p>")
      ),
      
      # Rent Vacancy Analysis tab
      tabItem(tabName = "rentVacan",
              fluidRow(
                box(title = "Rent Vacancy Map", width = 6, plotOutput("VacancyMap")),
                box(title = "Rent Vacancy Tertile", width = 6, plotOutput("rentVacanTertilePlot"))
              ),
              HTML("<p><strong>Rent Vacancy Tertiles Results:</strong> Lower spatial autocorrelation (Moran’s I: 0.1951, p-value < 2.2e-16). Suggests some level of clustering in rental vacancies but not as pronounced as other variables. This might indirectly relate to housing affordability and poverty levels, but additional analysis is needed for a clearer understanding.</p>")
      ),
      tabItem(tabName = "tableView",
              box(title = "Data View", width = 12,
                  HTML("<p><strong>This is a sample snapshot of the income data utilized in the analysis.</strong> Due to the complexity of the full dataset, it's not included in its entirety. However, this excerpt gives an insight into how the dataframe is structured for analytical purposes.</p>"),
                  DTOutput("dataTable")
                  
              )
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
    selected_column <- switch(input$employmentStatus,
                              "Employed" = "Employed",
                              "Unemployed" = "Unemployed",
                              "Not in labor force" = "Not in labor force",
                              NULL)  # default case
    if (!is.null(selected_column)) {
      create_map(Employment_shapefile, selected_column)
    } else {
      leaflet() %>%
        addTiles()
    }
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
