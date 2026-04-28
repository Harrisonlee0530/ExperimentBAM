library(shiny)
library(dplyr)
library(bslib)
library(leaflet)
library(terra)
library(stringr)

setwd("bird_dashboard")
# ---- Load Tabs ----
source("tabs/welcome.R")
source("tabs/dashboard.R")
source("tabs/method.R")
source("tabs/model_access.R")
source("tabs/contact.R")

# # ---- Sample Data ----
# set.seed(123)
# 
# n <- 800
# species_list <- c("Sparrow", "Eagle", "Hawk", "Duck")
# species_probs <- c(0.5, 0.1, 0.2, 0.2)
# regions <- c("North", "South", "East", "West")
# 
# bird_data <- data.frame(
#     species = sample(species_list, n, replace = TRUE, prob = species_probs),
#     year = sample(2018:2023, n, replace = TRUE,
#                   prob = c(1,1,1.2,1.3,1.5,1.7))
# )
# 
# bird_data$region <- mapply(function(sp) {
#     if (sp == "Duck") {
#         sample(c("North", "East"), 1, prob = c(0.7, 0.3))
#     } else if (sp == "Eagle") {
#         sample(c("West", "North"), 1, prob = c(0.6, 0.4))
#     } else if (sp == "Hawk") {
#         sample(c("South", "West"), 1)
#     } else {
#         sample(regions, 1)
#     }
# }, bird_data$species)
# 
# region_centers <- data.frame(
#     region = regions,
#     lat_center = c(47, 38, 42, 43),
#     lon_center = c(-100, -100, -80, -120)
# )
# 
# bird_data <- bird_data %>%
#     left_join(region_centers, by = "region") %>%
#     mutate(
#         lat = rnorm(n(), lat_center, 1.5),
#         lon = rnorm(n(), lon_center, 2)
#     ) %>%
#     select(-lat_center, -lon_center)

tif_dir <- "sample_data/model_predictions"

tif_files <- list.files(
    tif_dir,
    pattern = "\\.tif$",
    full.names = TRUE
)

# Build metadata table
raster_meta <- data.frame(
    file = tif_files,
    name = basename(tif_files)
) %>%
    mutate(
        name = str_remove(name, "\\.tif$"),
        parts = str_split(name, "_", simplify = TRUE),
        
        species = parts[,1],
        region  = parts[,2],
        year    = as.integer(parts[,3])
    )

# Load rasters into a named list
rasters <- setNames(
    lapply(raster_meta$file, rast),
    raster_meta$name
)

head(raster_meta)

# ---- UI ----
ui <- navbarPage(
    title = "Bird Dashboard",
    id = "top_tabs",
    
    theme = bs_theme(
        version = 5,
        primary = "#153B40",   # rgb(21, 59, 64)
        secondary = "#1E5055", # rgb(30, 80, 85)
        base_font = font_google("Inter")
    ),
    
    header = tags$head(
        tags$style(HTML("
      /* Navbar background */
      .navbar {
        background-color: #153B40 !important;
        border: none;
      }

      /* Navbar text */
      .navbar .navbar-brand,
      .navbar .nav-link {
        color: #FFFFFF !important;
      }

      /* Active tab */
      .navbar .nav-link.active {
        background-color: #1E5055 !important;
        color: #FFFFFF !important;
      }

      /* Hover effect */
      .navbar .nav-link:hover {
        background-color: #1E5055 !important;
        color: #FFFFFF !important;
      }

      /* Sidebar panel styling */
      .well {
        background-color: #F4F7F7;
        border-radius: 10px;
        border: 1px solid #D0DADA;
      }

      /* Buttons / slider accents */
      .btn-primary {
        background-color: #153B40;
        border-color: #153B40;
      }

      .btn-primary:hover {
        background-color: #1E5055;
        border-color: #1E5055;
      }

      /* Slider color */
      .irs-bar, .irs-single {
        background: #153B40;
        border-color: #153B40;
      }

      .irs-from, .irs-to {
        background: #1E5055;
      }
    "))
    ),
    
    tabPanel("Welcome", welcomeUI()),
    tabPanel("Dashboard", dashboardUI()),
    tabPanel("Method", methodUI()),
    tabPanel("Model Access", modelAccessUI()),
    tabPanel("Contact Us", contactUI())
)

# ---- Server ----
server <- function(input, output, session) {
    
    # filtered_data <- reactive({
    #     req(input$year)
    #     bird_data %>%
    #         filter(
    #             year == input$year,
    #             region %in% input$region,
    #             species %in% input$species
    #         )
    # })
    selected_raster <- reactive({
        
        req(input$species, input$region, input$year)
        
        file_name <- paste0(input$species, "_", input$region, "_", input$year)
        
        req(file_name %in% names(rasters))
        
        rasters[[file_name]]
    })
    
    output$year_text <- renderText({
        paste("Year:", input$year)
    })
    
    # output$map <- renderLeaflet({
    #     leaflet() %>%
    #         addTiles() %>%
    #         setView(lng = -100, lat = 42, zoom = 4)
    # })
    
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
    
    # observe({
    #     req(input$top_tabs == "Dashboard")
    #     
    #     leafletProxy("map", data = filtered_data()) %>%
    #         clearMarkers() %>%
    #         addCircleMarkers(
    #             lng = ~lon,
    #             lat = ~lat,
    #             radius = 4,
    #             color = "#2E7D32",
    #             fillOpacity = 0.5,
    #             popup = ~paste(species, year, region)
    #         )
    # })
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -100, lat = 45, zoom = 4)
    })
    
    observe({
        
        r <- selected_raster()
        
        leafletProxy("map") %>%
            clearImages() %>%
            addRasterImage(
                r,
                opacity = 0.6,
                colors = colorNumeric("viridis", values(r), na.color = NA)
            )
    })
}

shinyApp(ui, server)