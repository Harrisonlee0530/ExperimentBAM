server <- function(input, output, session) {
  
  output$species_ui <- renderUI({
    selectInput(
      "species", "Species:",
      choices = unique(raster_meta$species)
    )
  })
  
  # ---- Dynamic region selector ----
  output$region_ui <- renderUI({
    
    req(input$species)
    
    selectInput(
      "region", "Region:",
      choices = raster_meta %>%
        filter(species == input$species) %>%
        pull(region) %>%
        unique()
    )
  })
  
  # ---- Select correct raster ----
  selected_raster <- reactive({
    
    req(input$species, input$region, input$year)
    
    key <- paste0(input$species, "_", input$region, "_", input$year)
    print(key)
    
    validate(need(key %in% names(rasters), "Raster not found"))
    
    r <- rasters[[key]]
    
    # ensure correct CRS for leaflet
    if (!terra::is.lonlat(r)) {
      r <- terra::project(r, "EPSG:4326")
    }
    
    # use first band
    if (terra::nlyr(r) > 1) {
      r <- r[[1]]
    }
    
    r
  })
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95, lat = 40, zoom = 4)
  })
  
  observe({
    
    r <- selected_raster()
    
    # safer value extraction
    vals <- terra::values(r, na.rm = TRUE)
    
    pal <- colorNumeric(
      palette = brewer.pal(9, "YlGn"),
      domain = vals,
      na.color = "transparent"
    )
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(
        r,
        colors = pal,
        opacity = 0.7
      )
  })
}