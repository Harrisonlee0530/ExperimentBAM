library(shiny)
library(leaflet)
library(terra)
library(dplyr)
library(stringr)
library(bslib)
library(bsicons)
library(RColorBrewer)
library(shinycssloaders)
library(shinydashboard)
library(readxl)

addResourcePath("img", "img")

# Load species metadata
species_info_df <- read_excel("sample_data/V5ModelSummary_CAWA.xlsx", sheet = "species") %>%
  select(id, scientific, english, french, family)

# ---- Directories ----
input_dir  <- "sample_data/model_predictions"
output_dir <- "sample_data/cog_predictions"

dir.create(output_dir, showWarnings = FALSE)

# ---- List original TIFFs ----
tif_files <- list.files(
  input_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# ---- Convert to COG if missing ----
for (f in tif_files) {
  
  out_file <- file.path(output_dir, basename(f))
  
  # Inside your for loop in global.R:
  if (!file.exists(out_file)) {
    message("Converting and Projecting to COG: ", basename(f))
    
    r <- rast(f)
    
    # Reproject to WGS84 (EPSG:4326)
    if (!terra::is.lonlat(r)) {
      r <- terra::project(r, "EPSG:3857")
    }
    
    writeRaster(
      r,
      out_file,
      filetype = "COG",
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "TILING=YES") # Standard COG optimizations
    )
  } else {
    message("File ", basename(f), " exists already")
  }
}

# ---- Now load COG files ----
cog_files <- list.files(
  output_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# ---- Parse metadata ----
raster_meta <- data.frame(
  file = cog_files,
  name = basename(cog_files)
) %>%
  mutate(
    name = str_remove(name, "\\.tif$"),
    parts = str_split(name, "_", simplify = TRUE),
    species = parts[,1],
    region  = parts[,2],
    year    = as.integer(parts[,3])
  )

# ---- Load rasters when needed ----
get_raster <- function(key) {

  f <- file.path(output_dir, paste0(key, ".tif"))
  
  if(!file.exists(f)) return(NULL)

  r <- rast(f)

  # first band only
  if (nlyr(r) > 1) r <- r[[1]]
  
  r <- terra::aggregate(r, fact = 2)
  
  # print(terra::crs(r))
  # print(terra::is.lonlat(r))

  r
}