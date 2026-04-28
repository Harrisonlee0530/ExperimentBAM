dashboardUI <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("species", "Species:",
                    choices = unique(raster_meta$species))
        
        uiOutput("region_ui")
        
        sliderInput("year", "Year:",
                    min = min(raster_meta$year),
                    max = max(raster_meta$year),
                    value = min(raster_meta$year),
                    step = 1,
                    sep = "")
      ),
      
      mainPanel(
        h4(textOutput("year_text")),
        leafletOutput("map", height = 600)
      )
    )
  )
}