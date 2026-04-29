dashboardUI <- function() {
  
  bslib::page_sidebar(
    
    title = "Species Distribution",
    
    sidebar = bslib::sidebar(
      width = 300,
      open = "open",
      
      uiOutput("species_ui"),
      uiOutput("region_ui"),
      
      sliderInput(
        "year", "Year:",
        min = min(raster_meta$year),
        max = max(raster_meta$year),
        value = min(raster_meta$year),
        step = 5,
        sep = ""
      )
    ),
    
    uiOutput("species_info_card"),
    
    bslib::card(
      full_screen = TRUE,
      leafletOutput("map", height = 600),
    )
  )
}