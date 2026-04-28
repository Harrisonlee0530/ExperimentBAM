library(shiny)
library(leaflet)
library(terra)
library(dplyr)
library(stringr)
library(bslib)
library(RColorBrewer)

# ---- Load TIFF files ----
tif_dir <- "sample_data/model_predictions"

tif_files <- list.files(
  tif_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# ---- Parse metadata from filenames ----
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

# ---- Load rasters ----
rasters <- setNames(
  lapply(raster_meta$file, rast),
  raster_meta$name
)