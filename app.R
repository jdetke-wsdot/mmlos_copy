# DRAFT place types - zoomable map

#load packages
#install.packages(c("shiny", "leaflet", "sf"))
#Sys.setenv(GITHUB_PAT = "your PAT")
#devtools::install_github("statnmap/HatchedPolygons")
library(shiny)
library(leaflet)
library(sf)
library(HatchedPolygons)

# DRAFT place types polygon
pt_sf <- st_read("polygon/AreaTypeATjobsTransit.shp")
pt_sf <- st_transform(pt_sf, crs = 4326)

# Freight intensive land use polygon
freight <- st_read("polygon/FreightIntensive.shp")
freight <- st_transform(freight, crs = 4326)

# Crosshatching for freight
freight.hatch <- hatched.SpatialPolygons(freight, density = c(120, 60), angle = c(45, 135))

# Highways of statewide significance
hss <- st_read("polygon/HighwaysStatewideSignificance.shp")
hss <- st_transform(hss, crs = 4326)

# Level of Service
los <- st_read("polygon/WSDOT_-_Level_of_Service_Standard_for_State_Routes.shp")
los <- st_transform(los, crs = 4326)

# Make the place types a factor instead of character so can color code it and sort in a sane way
pt_sf$EPA_WA_A_3 <- as.factor(pt_sf$EPA_WA_A_3)
forcats::fct_relevel(pt_sf$EPA_WA_A_3, c("rural", 
                                         "suburban",
                                         "urban",
                                         "urban core"))

# Create a color palate
# Levels: Close in Community, Low Density/Rural, Regional Center, Suburban/Town
# ordered and colors coded from WSDOT communications manual: https://www.wsdot.wa.gov/publications/manuals/fulltext/M3030/Communications.pdf?may-2021= 
factpal <- colorFactor(c("#007b5f", "#97d700","#00aec7", "DarkGray"), pt_sf$EPA_WA_A_3)

# Add an additional palate for colors in the existing level of service
# Based on basic color blind palate, could switch to any other
# Levels, C, D, E, E-mitigated 
factpal2 <- colorFactor(c("#F5793A","#A95AA1","#85C0F9","#0F2080"), los$LevelOfSer)

# Define UI 
ui <- fluidPage(
  titlePanel("DRAFT place types"),
  fluidRow(
    column(12, leafletOutput("map"),
           p(strong("DRAFT place types:")),
           p("Rural: 0-4 people and/or jobs per acre. No existing fixed-route transit service."),
           p("Suburban: 4-8 people and/or jobs per acre, OR, 1-4 people and/or jobs per acre IF there is existing fixed-route transit service."),
           p("Urban: 8-30 people and/or jobs per acre, OR, 4-8 people and jobs per acre IF the regional share of employment is in the top 5% of block groups for employment."),
           p("Urban core: More than 30 people and/or jobs per acre, OR, 8-30 people and jobs per acre IF there are more than 30 pedestrian links per square mile."),
           p("Freight dependent land use: More than 2,000 freight dependent jobs in the block group. Freight dependent jobs include NAICS sectors 11 (Agriculture, Forestry, Fishing and Hunting), 21 (Mining, Quarrying, and Oil and Gas Extraction), 22 (Utilities), 23 (Construction), 31-33 (Manufacturing), 42 (Wholesale Trade), and 48-49 (Transportation and Warehousing)."),
           p("Current Level of Service:"),
           p("* C: Speeds remain near free flow, but freedom to maneuver is noticeably restricted.", style = "color:#F5793A"),
           p("* D: Speed begins to decline with increasing volume.  Freedom to maneuver is further reduced, and the traffic stream has little space to absorb disruptions.", style = "color:#A95AA1"),
           p("* E: Unstable flow with volume at or near capacity. Freedom to maneuver is extremely limited, and level of comfort afforded to the driver is poor.", style = "color:#85C0F9"),
           p("* E mitigated: Congestion should be mitigated (such as transit) when p.m. peak hour LOS falls below LOS 'E.'", style = "color:#0F2080"),
           p("Level of Service definitions from: ", tags$a(href="https://wsdot.maps.arcgis.com/home/item.html?id=3f840aeeb1ba481c905270ca103cd1db","https://wsdot.maps.arcgis.com/home/item.html?id=3f840aeeb1ba481c905270ca103cd1db")),
           p(strong("Data sources:")),
           p("EPA Smart Location Mapping:", tags$a(href="https://www.epa.gov/smartgrowth/smart-location-mapping#SLD","https://www.epa.gov/smartgrowth/smart-location-mapping#SLD")),
           p("Highways of Statewide Significance:", tags$a(href="https://gisdata-wsdot.opendata.arcgis.com/datasets/754bfacdf14b4d0793a618bb1b5dfe2f_0/explore","https://gisdata-wsdot.opendata.arcgis.com/datasets/754bfacdf14b4d0793a618bb1b5dfe2f_0/explore")),
           p(strong("Potential Multimodal Level of Service:")),
           p("Cars: Modified level of service based on the current level of service as detailed in the", tags$a(href="https://wsdot.wa.gov/sites/default/files/2023-11/HSP-2024Update-PublicReviewDraft.pdf","Draft Highway System Plan.")),
           p("Active modes: ", tags$a(href="https://wsdot.wa.gov/sites/default/files/2023-06/PlanningStudyGuidance-AT-PlanForLevelOfTrafficStress.pdf", "Level of Traffic Stress"), "two or better across all place types."),
           p("Transit: A range of fixed-route and demand response services across place types. Regardless of transit-supportive land uses, there are always people in all place types who cannot drive and a demand for transit services."),
           p("Freight: There is demand for freight across all place types, whether for agricultural use in rural area or deliveries in the most urban areas. Special consideration should be given to freight dependent land uses, regardless of passenger vehicle level of service--e.g., truck parking, truck only lanes, dedicated loading zones, etc.")
           )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = pt_sf,
                  weight = 0,
                  #color = "white",
                  fillOpacity = 0.4,
                  fillColor = ~factpal(EPA_WA_A_3)) %>%
      addPolygons(data = freight, 
                  fillColor = c("transparent", "transparent"),
                  color = "#545454", 
                  opacity = 1, 
                  fillOpacity = 0.6, 
                  stroke = TRUE,
                  weight = 1.5, 
                  group = "Freight Dependent Land Use") %>%
      addPolylines(data = freight.hatch,
                   color = c("#545454", "blue")[as.numeric(freight.hatch$ID)],
                   group = "Freight Dependent Land Use") %>%
      addPolylines(data = los,
                   color = ~factpal2(LevelOfSer),
                   group = "Current Level Of Service") %>%
      addPolylines(data = hss,
                   color = "black",
                   group = "Highways of Statewide Significance") %>%
      addLayersControl( overlayGroups = c( "Freight Dependent Land Use",
                                           "Current Level Of Service", 
                                           "Highways of Statewide Significance")
                        , options = layersControlOptions( collapsed = FALSE ) ) %>%
      hideGroup( group = c( "Freight Dependent Land Use",
                            "Current Level Of Service", 
                            "Highways of Statewide Significance")) %>%
      addLegend("topright",
                colors = c("#007b5f", "#97d700", "#00aec7", "DarkGray"),
                labels = c("Rural", 
                           "Suburban", 
                           "Urban",
                           "Urban core"),
                title = "DRAFT place types")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
