#-------------------------------------------------------------------------------
# 3D Bivariate Map
#-------------------------------------------------------------------------------

# i. Required packages
# --------------------------------------------------
# This section ensures that all required packages are available for the workflow.
# It handles:
#   (1) installation of packages (if missing)
#   (2) loading packages into the session

# Install {rchelsa} from GitHub (required for CHELSA data access)
# Uses {remotes} since the package is not on CRAN
if(!requireNamespace("rchelsa", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("inSileco/rchelsa");
  library(rchelsa)
}

# Ensure {pacman} is available for streamlined package management
if(!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman");
  library(pacman)
}

# Install (if needed) and load all required packages
# {pacman::p_load()} checks installation status and loads packages in one step
pacman::p_load(
  tidyverse, sf, terra, rchelsa, glue, biscale,
  biscale, elevatr, cowplot, geodata, ggplotify,
  gridGraphics, rayshader, magick, colorspace
)

# ii. CHELSA Climate Data Acquisition and Preparation
# -----------------------------------------------------
# This section downloads bioclimatic variables from the CHELSA database,
# reads them into R, and prepares a stacked raster for analysis.

# Define the directory where downloaded data will be stored
chelsa <- "bioclim"

data_dir <- "data/{chelsa}/"

if(!dir.exists(glue::glue(data_dir))) {
  dir.create(glue::glue(data_dir))
}

# Specify CHELSA bioclimatic variable IDs:
# 1  = Mean Annual Near-Surface Air Temperature '°C' (BIO1)
# 12 = Annual Total Precipitation 'kg m-2 year-1'/'mm year-1' (BIO12)
ids <- c(1, 12)

# Define a helper function to download CHELSA data for a given variable ID
# Arguments:
#   id   - CHELSA bioclimatic variable ID
#   path - directory where files will be saved
download_chelsa_data <- function(id, path) {
  rchelsa::get_chelsea_data(
    categ = "clim",   # Climate data category
    type  = "bio",    # Bioclimatic variables
    id    = id,       # Variable ID (BIO1, BIO12, etc.)
    path  = path      # Output directory
  )
}

# Download selected CHELSA variables into the specified directory
lapply(ids, download_chelsa_data, path = data_dir)

# List downloaded files to verify successful download
list.files(
  glue::glue(data_dir)
)

# Read downloaded raster files using terra
# BIO1: Annual Mean Temperature
temp <- terra::rast(paste0(glue::glue(data_dir), "CHELSA_bio10_01.tif"))

# BIO12: Annual Precipitation
prec <- terra::rast(paste0(glue::glue(data_dir), "CHELSA_bio10_12.tif"))

# Temperature is stored as stored as °C × 10 which needs rescaling
temp_rescaled <- temp / 10

# Combine temperature and precipitation into a multi-layer raster stack
temp_prec <- c(temp_rescaled, prec)

# Assign meaningful layer names for easier interpretation in downstream analysis
names(temp_prec) <- c("temperature", "precipitation")

# iii. Retrieve country boundary (administrative level 0)
# ---------------------------------------------------------
# This is a generic step for downloading national boundaries.
# Malawi ("MWI") is used here as an example and can be replaced with
# any country of interest by changing the ISO3 code or country name.
#
# Arguments:
#   country - ISO3 country code or country name (e.g., "MWI" or "Malawi")
#   level   - administrative level (0 = country, 1 = regions, etc.)
#   path    - directory to store downloaded shapefiles
#
# Source: GADM (Global Administrative Areas database)
# Output is converted to an 'sf' object for spatial operations.

iso3 <- "MWI"  # Example: Malawi (replace as needed)

# Define country-specific storage path
boundary_dir <- file.path("data", "boundaries", iso3)

# Create directory if it doesn't exist
if(!dir.exists(boundary_dir)) {
  dir.create(boundary_dir, recursive = TRUE)
}

# Download and read boundary
country_sf <- geodata::gadm(
  country = iso3,    
  level = 0,          
  path = boundary_dir       
) |>
  sf::st_as_sf()

# Extract country centroid coordinates
country_cent <- st_centroid(country_sf) |>
  st_transform(crs = 4326)
#' 
c <- country_cent |>
  mutate(long = unlist(map(country_cent$geometry, 1)),
         lat = unlist(map(country_cent$geometry, 2))) |>
  as_tibble()
#' 
coords <- c(c[[1, "long"]], c[[1, "lat"]])

# iv. Crop, Extract Elevation, and Resample Climate Data
# --------------------------------------------------------
# This step aligns climate data (temperature and precipitation) with the
# spatial extent and resolution of the study area.
# The workflow includes:
#   (1) cropping to country boundary
#   (2) acquiring DEM data for spatial resolution reference
#   (3) resampling climate rasters to match DEM resolution
#   (4) reprojecting to a target coordinate reference system (CRS)

# Define target coordinate reference system (CRS)
# EPSG:32736 = WGS 84 / UTM zone 36S (check https://EPSG.io for alternatives)
target_crs <- "EPSG:32736"

# Crop climate data to the country boundary
# and apply a spatial mask to remove outside areas
temp_prec_country <- terra::crop(
  temp_prec,
  country_sf,
  mask = TRUE
)

# Download Digital Elevation Model (DEM) from AWS via elevatr
# This DEM is used as a spatial reference for resampling resolution
z <- 8

dem <- elevatr::get_elev_raster(
  locations = country_sf, 
  z = z,              # zoom level (controls spatial resolution)
  clip = "locations" # clip output to study area
) |> 
  terra::rast() |> 
  terra::crop(country_sf, mask = TRUE)

# Resample climate rasters to match DEM spatial resolution
# Bilinear interpolation is used for continuous variables
temp_prec_resampled <- terra::resample(
  x = temp_prec_country,  # input climate data
  y = dem,                # target resolution reference (DEM)
  method = "bilinear"    # appropriate for continuous data
) |> 
  terra::project(target_crs)  # reproject to analysis CRS

# Visual check of resampled raster layers
terra::plot(temp_prec_resampled, axes = FALSE, box = FALSE)

# Convert raster stack to data frame with spatial coordinates
# Useful for ggplot-based visualization 
temp_prec_df <- as.data.frame(
  temp_prec_resampled, xy = TRUE
)

# v. Create Bivariate Classes, Define Palette, and Map Theme
# ------------------------------------------------------------
# This section prepares inputs for bivariate mapping:
#   (1) classifies temperature and precipitation into bivariate bins
#   (2) defines a color palette for visualization
#   (3) sets a custom theme for consistent map styling

# Create bivariate classes using biscale
# This converts continuous variables (temperature and precipitation)
# into a 4x4 bivariate classification grid using Fisher-Jenks breaks
breaks <- biscale::bi_class(
  temp_prec_df, 
  x = temperature,
  y = precipitation, 
  style = "fisher",  # optimal natural break classification
  dim = 4            # defines a 4-by-4 bivariate grid
)

# Define bivariate color palette
# "BlueOr" = blue–orange diverging palette from {biscale}
pal <- "BlueOr"

# Generate the palette plot
pal_plot <- bi_pal(pal = pal, dim = 4)

# Extract the hex codes from the first layer’s data
colors <- layer_data(pal_plot, 1)$fill

colorspace::swatchplot(colors)

# Define custom map theme
# This theme removes axis elements and keeps a clean white background
custom_theme <- function(){
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(
        fill = "#ffffff", color = NA
      )
    )
}

# vi. 2D Bivariate Map 
# --------------------------------------------------
# This section creates the final 2D bivariate map and manually constructs
# a rotated legend for improved visual aesthetics.
#
# NOTE: The legend is generated separately using {biscale}, exported as an
# image, rotated using {magick}, and then reintroduced into the plot using
# {cowplot}. This approach allows fine control over legend orientation,
# which is not directly supported in standard ggplot2 workflows.

# Create main bivariate map (without legend)
map_plot <- ggplot(breaks) +
  geom_raster(
    aes(x = x, y = y, fill = bi_class),
    show.legend = FALSE
  ) +
  biscale::bi_scale_fill(
    pal = pal,
    dim = 4,
    flip_axes = FALSE,
    rotate_pal = FALSE
  ) +
  coord_sf(datum = NA) +
  custom_theme()

# Create bivariate legend 
legend <- biscale::bi_legend(
  pal = pal,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4,
  xlab = "Temperature (°C)",
  ylab = "Precipitation (mm)",
  size = 16
)

# Export legend as temporary PNG for post-processing
tmp <- tempfile(fileext = ".png")

ggsave(
  filename = tmp,
  plot = legend,
  width = 3,
  height = 3,
  dpi = 300
)

# Read legend image into memory
legend_img <- magick::image_read(tmp)

# Rotate legend for improved visual alignment (aesthetic choice)
legend_rotated <- magick::image_rotate(legend_img, 45)

# Convert rotated image into a ggplot object
legend_rotated_gg <- ggplotify::as.ggplot(legend_rotated)

# Combine map and rotated legend using cowplot layering system
full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(
    plot = map_plot,
    x = 0, y = 0,
    width = 1, height = 1
  ) +
  cowplot::draw_plot(
    plot = legend_rotated_gg,
    x = 0.05, y = 0.04,
    width = 0.35, height = 0.35
  )

# Display final composed map
print(full_map)

# Define map name for reproducible file naming
map <- "CONFIG_MAP"

# Save final 2D bivariate map as high-resolution PNG
ggsave(
  filename = glue::glue("images/{map}/{map}_temp-prec_{pal}_2d.png"),
  plot = full_map,
  width = 7,
  height = 7,
  dpi = 600,
  device = "png",
  bg = "#ffffff"
)

# vii. Prepare DEM data for terrain visualization layer
# -------------------------------------------------------
# This step converts the Digital Elevation Model (DEM) into a tidy
# data frame format for ggplot-based rendering.
# The DEM is used as a subtle background terrain layer to enhance
# spatial context in the final map.

# Convert DEM raster to data frame with coordinates
dem_df <- dem |>
  terra::project(target_crs) |> 
  as.data.frame(xy = TRUE, na.rm = TRUE)

# Rename elevation column for clarity
names(dem_df)[3] <- "dem"

# Create terrain (elevation) background layer
# This layer provides a soft grayscale representation of elevation
# to enhance spatial perception without distracting from the main data.
dem_map <- ggplot(
  dem_df,
  aes(x = x, y = y, fill = dem)
) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") +  # minimalist elevation shading
  guides(fill = "none") +                    # remove legend for cleaner layout
  coord_sf(
    crs = target_crs,  # ensure spatial alignment with main map
    datum = NA
  ) +
  custom_theme() +
  theme(
    legend.position = "none"
  )

# viii. Render 3D Scene using rayshader
# --------------------------------------------------
# This step generates a 3D visualization by combining:
#   - full_map (2D bivariate map as texture)
#   - dem_map (elevation surface for terrain structure)
#
# The result is a 3D geospatial scene suitable for high-quality rendering
# and visualization of spatial patterns.

rayshader::plot_gg(
  ggobj = full_map,        # 2D bivariate map (texture layer)
  ggobj_height = dem_map,  # elevation surface (terrain structure)
  width = 7,
  height = 7,
  windowsize = c(600, 600),
  
  scale = 100,             # vertical exaggeration of terrain (controls relief intensity)
  
  shadow = TRUE,           # enables terrain shadowing for depth perception
  shadow_intensity = 1,    # controls darkness/strength of shadows
  
  phi = 87,                # camera tilt (near top-down perspective)
  theta = 0,               # rotation around vertical axis
  zoom = 0.56,             # camera zoom level (smaller = more zoomed out)
  
  background = "white",    # background color of the scene
  multicore = TRUE         # speeds up rendering using multiple CPU cores
)

# Adjust camera zoom after initial render (fine tuning view framing)
rayshader::render_camera(zoom = 0.6)

# Safely close 3D rendering window (prevents overlapping rgl sessions)
try(rgl::close3d())

# ix. Render High-Quality 3D Scene
# --------------------------------------------------
# This stage produces a final high-resolution 3D render of the map using {rayshader}.
# It includes:
#   (1) defining output file paths
#   (2) saving configuration metadata for reproducibility
#   (3) executing a high-quality raytraced render with environment lighting

# Define output file path for the rendered image
# File naming includes map name, palette, zoom level, and render stage
outfile <- glue::glue(
  "images/{map}/{map}_temp-prec_{pal}_z{z}_3d.png"
)

# Save key parameters for reproducibility
# These objects can be reused later 
saveRDS(
  list(
    map = map,
    pal = pal,
    z = z,
    colors = colors,
    outfile = outfile,
    coords = coords
  ),
  glue("R/portraits/{map}/header.rds")
)

# Render block (wrapped for clarity and execution control)
{
  # Validate output path by writing a temporary test image
  # Ensures that directory structure exists and is writable
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), outfile)
  }
  
  # Track render start time (useful for performance monitoring)
  start_time <- Sys.time()
  cat(glue::glue("Start Time: {start_time}"), "\n")
  
  # High-quality raytraced rendering using rayshader
  rayshader::render_highquality(
    filename = outfile,
    preview = TRUE,   # display preview during rendering
    light = FALSE,    # disable default directional lighting
    
    # Use HDR environment lighting for realistic illumination
    # HDR file simulates studio lighting conditions
    environment_light = "assets/env/brown_photostudio_02_4k.hdr",
    intensity = 1,     # brightness of environment lighting
    rotate_env = 90,   # rotate lighting environment for optimal shading
    
    parallel = TRUE,   # enable multi-core rendering
    
    width = 5000, 
    height = 5000,     # output resolution (square format for consistency)
    
    interactive = FALSE  # non-interactive rendering (batch-friendly)
  )
  
  # Track render end time and total duration
  end_time <- Sys.time()
  cat(glue::glue("End Time: {end_time}"), "\n")
  cat(glue::glue("Total Time: {end_time - start_time}"), "\n")
}