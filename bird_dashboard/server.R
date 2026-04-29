server <- function(input, output, session) {
  
  # UI Outputs for Species and Region
  output$species_ui <- renderUI({
    selectInput(
      "species", "Species:", 
      choices = unique(raster_meta$species)
    )
  })
  
  output$region_ui <- renderUI({
    req(input$species)
    
    choices <- raster_meta %>%
      filter(species == input$species) %>%
      pull(region) %>%
      unique()
    
    selectInput(
      "region", "Region:", 
      choices = choices
    )
  })
  
  # Reactive to get metadata
  selected_species_data <- reactive({
    req(input$species)
    species_info_df %>% 
      filter(id == input$species) %>% 
      slice(1)
  })
  
  # Render the Info Card
  output$species_info_card <- renderUI({
    info <- selected_species_data()
    req(nrow(info) > 0)
    
    img_url <- paste0("img/", info$id, ".jpg")
    
    bslib::card(
      card_body(
        fill = FALSE,
        div(style = "display: flex; align-items: flex-start;",
            
            # Left: 4:3 Image
            tags$img(
              src = img_url, 
              onerror = "this.onerror=null; this.src='https://via.placeholder.com/160x120?text=No+Photo';",
              style = "width: 160px; height: 120px; border-radius: 4px; margin-right: 25px; object-fit: cover; border: 1px solid #dee2e6;"
            ),
            
            # Right: Specific Formatting
            div(
              # Line 1: English Name
              tags$h2(info$english, style = "margin: 0; color: #153B40; font-weight: 700;"),
              
              # Line 2: French · Scientific · Family
              div(style = "font-size: 1.2rem; color: #555; margin-top: 5px;",
                  paste(info$french, " · ", info$scientific, " · Family ", info$family)
              )
            )
        )
      )
    )
  })
  
  raw_key <- reactive({
    req(input$species, input$region, input$year)
    paste0(input$species, "_", input$region, "_", input$year)
  })
  
  # This prevents the "selected_raster" from firing too often
  delayed_key <- raw_key %>% debounce(500)
  
  selected_raster <- reactive({
    req(delayed_key())
    tryCatch({
      get_raster(delayed_key())
    }, error = function(e) return(NULL))
  })
  
  # Initialize Base Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Using a cleaner base map
      setView(lng = -95, lat = 40, zoom = 4)
  })
  
  # Update Layer without reloading the whole map
  observe({
    r <- selected_raster()
    req(r)
    
    # Extract values for the palette
    vals <- terra::values(r, na.rm = TRUE)
    
    # If raster is empty, clear images and stop
    if(length(vals) == 0) {
      leafletProxy("map") %>% clearImages()
      return()
    }
    
    pal <- colorNumeric(
      palette = "YlGn", # Standard RColorBrewer name works directly
      domain = vals,
      na.color = "transparent"
    )
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>% # Remove old legends
      addRasterImage(
        r,
        colors = pal,
        opacity = 0.7,
        project = FALSE
      ) %>%
      addLegend(pal = pal, values = vals, title = "Value")
  })
}