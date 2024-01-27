# Clark-County-GIS-Analysis
## Introduction
This project explores the relationship between housing and equity in Clark County. It focuses on the accessibility of affordable housing and aims to unravel patterns in poverty distribution, housing affordability, and demographic dynamics, particularly among BIPOC communities.

## Data Sources
- **Clark County, WA Open Data - Shapefile**: Used for geospatial analysis. [Clark County Shapefile](https://hub-clarkcountywa.opendata.arcgis.com/pages/digital-gis-data-download)
- **HUD Data**: Includes data on [Fair Market Rents (FMRs)](https://www.huduser.gov/portal/datasets/fmr.html#year2024) and [Small Area FMRs](https://www.huduser.gov/portal/datasets/fmr/smallarea/index.html#query_2024).
- **Census Data**: Provides demographic, employment, and income data. [Census Data Link](#Census-Data-Link)


## GIS Skills and Methodology
- **Spatial Autocorrelation Analysis**: Application of Global Moran’s I tests to study spatial patterns.
- **Data Visualization**: Development of maps using R (`ggplot` and `geom_sf`) for spatial distribution visualization.
- **Tertile-Based Categorization**: Implemented in zipcode data analysis to understand spatial autocorrelation.
- **Advanced Data Manipulation**: Proficient in transforming, reshaping, and merging large datasets for GIS analysis.

## Key Findings
- **Economic Segregation**: Significant spatial clustering in income, indicating economic segregation.
- **BIPOC (Black, Indigenous, People of Color) Community Distribution**: Strong spatial concentration of BIPOC populations in certain neighborhoods.
- **SNAP Utilization and Employment**: Notable clustering indicating potential links to economic conditions and housing affordability.

## Interactive Analysis: ShinyApp
Explore the interactive analysis and visualizations on the ShinyApp: [GIS Analysis of Housing and Equity in Clark County](https://3ipavr-laura.shinyapps.io/Clark-County-GIS-Analysis/)

## Conclusion
This GIS-based analysis uncovers important socio-economic patterns in Clark County, highlighting the need for policy interventions in housing and equity. The project demonstrates the potential of GIS in informing urban planning and social equity decision-making.

## Repository Structure
- `src/`: Contains source codes.
- `data/`: Includes raw and processed data files.
- `ShinyApp/`: Interactive web application for data exploration.

## Future Directions
Further research could explore temporal trends in rent, the impact of gentrification on housing affordability, and the interplay between employment sectors and housing options.
