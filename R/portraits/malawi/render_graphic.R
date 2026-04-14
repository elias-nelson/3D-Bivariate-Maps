# 1. Packages
# -------------------
# Install development versions, if missing and load them
if(!require(remotes)){
  install.packages("remotes")
}

if(!require(rchelsa)){
  remotes::install_github(
    "inSileco/rchelsa"
  )
}

if(!require(biscale)){
  remotes::install_github(
    "chris-prener/biscale"
  )
}

# install and load other required packages
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, sf, terra, rchelsa,
  biscale, elevatr, cowplot,
  gridGraphics, rayshader, magick
)

# CHELSA Climate Data
# -----------------------
# Set the data directory
data_dir <- "data/"

# Define a vector of IDs to download (1 = Average Temperature; 12 = Total Precipitation)
ids <- c(1, 12)

# Function to download CHELSA data
download_chelsa_data <- function(id, path){
  rchelsa::get_chelsea_data(
    categ = "clim", type = "bio",
    id = id, path = path
  )
}

# Download data for each id
lapply(ids, download_chelsa_data, path = data_dir)

list.files(data_dir)

# Read in downloaded rasters
temp <- terra::rast(paste0(data_dir, "CHELSA_bio10_01.tif"))
prec <- terra::rast(paste0(data_dir, "CHELSA_bio10_12.tif"))

# Compute average precipitation
prec_average <- prec/30

# Stack average temperature and precipitation
temp_prec <- c(temp, prec_average)

# Rename the raster layers in the raster stack
names(temp_prec) <- c("temperature", "precipitation")

# MALAWIs BOUNDARY POLYGON
# ----------------------------
country_sf <- sf::st_read(
  file.path(boundary_dir, "MW_conterminous_boundary.shp")
)

# CROP AND RESAMPLE
# -----------------------------
# Define the target CRS
target_crs <- "EPSG:32736"

# Crop input raster to the country's extent and apply a mask
temp_prec_country <- terra::crop(
  temp_prec, malawi_sf, mask = TRUE
)

# Obtain AWS DEM tiles from elevatr
# convert to terra SpatRaster and crop
dem <- elevatr::get_elev_raster(
  locations = malawi_sf, z = 8,
  clip = "locations"
) %>% 
  terra::rast() %>% 
  terra::crop(malawi_sf, mask = TRUE)

# Resample the climate rasters to match DEM resolution
# using bilinear iterpolation, then reproject
temp_prec_resampled <- terra::resample(
  x = temp_prec_country, 
  y = dem,
  method = "bilinear"
) 

# Plot resampled raster
terra::plot(temp_prec_resampled, box = FALSE, axes = FALSE)

# Convert the raster to dataframe with coordinates
temp_prec_df <- as.data.frame(
  temp_prec_resampled,
  xy = TRUE
)

# BREAKS, PALETTE AND PLOT THEME
# -----------------------------------
breaks <- biscale::bi_class(
  temp_prec_df, x = temperature,
  y = precipitation, style = "fisher",
  dim = 4
)

# Define the color palette
pal <- "BlueOr"

# Define a custom theme for the map
custom_theme <- function(){
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    plot.background = element_rect(
      fill = "#ffffff", color = NA,
    ),
    plot.title = element_text(
      color = "grey10", hjust = .5,
      face = "bold", vjust = -1
    ),
    plot.subtitle = element_text(
      hjust = .5, vjust = -1
    ),
    plot.caption = element_text(
      size = 9, color = "grey20",
      hjust = .5, vjust = 1
    ),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )
}

# 6. 2D BIVARIATE MAP
# ----------------------------------------
map <- ggplot(breaks) +
  geom_raster(
    aes(x = x, y = y, fill = bi_class),
    show.legend = FALSE
  ) +
  biscale::bi_scale_fill(
    pal = pal, dim = 4,
    flip_axes = FALSE, rotate_pal = FALSE
  ) +
  # labs(
  #   title = "MALAWI: Temperature and Precipitation",
  #   subtitle = "Average temperature and precipitation (1981-2010)",
  #   caption = "Source: CHELSA | Author: Elias Nelson Kaunda",
  #   x = "", y = ""
  # ) +
  coord_sf(
    # xlim = c(30, 36),
    datum = NA
  ) +
  custom_theme()

# create the legend for the bivariate map
legend <- biscale::bi_legend(
  pal = pal,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4,
  xlab = "Temperature (°C)",
  ylab = "Precipitation (mm)",
  size = 16
)

# Save legend to PNG with transparent background
tmp <- tempfile(fileext = ".png")

ggsave(tmp, legend, width = 3, height = 3, dpi = 300)

# Read PNG
legend_img <- image_read(tmp)

# Rotate
legend_rotated <- image_rotate(legend_img, 45)

# Convert to ggplot object for cowplot
legend_rotated_gg <- ggplotify::as.ggplot(legend_rotated)

# combine the map and legend using cowplot
full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(
    plot = map, x = 0, y = 0,
    width = 1, height = 1
  ) +
  cowplot::draw_plot(
    plot = legend_rotated_gg, x = .18, y = .04,
    width = .35, height = .35
  )

# display the final map with legend
print(full_map)

# Save as 2D PNG file
ggsave(
  plot = full_map,
  filename = "images/malawi_temp-prec_2d.png",
  width = 7,
  height = 7,
  dpi = 600,
  device = "png",
  bg = "#ffffff"
)

# project and convert to DEM to dataframe
dem_df <- dem |>
  # terra::project(target_crs) |>
  as.data.frame(xy = TRUE, na.rm = TRUE)

# rename the third column to "dem"
names(dem_df)[3] <- "dem"

# create the terrain layer map
dem_map <- ggplot(
  dem_df, aes(x = x, y = y, fill = dem)
) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") +
  guides(fill = "none") +
  # labs(
  #   title = "BELGIUM: Temperature and Precipitation",
  #   subtitle = "Average temperature and precipitation (1981-2010)",
  #   caption = "Source: CHELSA | Author: Milos makes maps"
  # ) +
  coord_sf(
    crs = target_crs,
    xlim = c(30, 36),
    datum = NA
  ) +
  custom_theme() +
  theme(legend.position = "none")

# 8. RENDER 3D SCENE
#-------------------

rayshader::plot_gg(
  ggobj = full_map,
  ggobj_height = dem_map,
  width = 7,
  height = 7,
  windowsize = c(600, 600),
  scale = 100,
  shadow = TRUE,
  shadow_intensity = 1,
  phi = 87, theta = 0, zoom = .56,
  background = "white",
  multicore = TRUE
)

# zoom out
rayshader::render_camera(zoom = .6)

try(rgl::close3d())

# 9. RENDER 3D OBJECT
#---------------------

rayshader::render_highquality(
  filename = "tracked_graphics/malawi_temp-prec_3d.png",
  preview = TRUE,
  light = FALSE,
  environment_light = "assets/env/brown_photostudio_02_4k.hdr",
  intensity = 1,
  rotate_env = 90,
  parallel = TRUE,
  width = 5000, height = 5000,
  interactive = FALSE
)

out_file <- "tracked_graphics/malawi_temp-prec_3d.png"

#'
{
  # Test write a PNG to ensure the file path is good.
  if(!file.exists(out_file)) {
    png::writePNG(matrix(1), out_file)
  }
  
  # I prefer tracking when the render starts and ends
  start_time <- Sys.time()
  cat(glue::glue("Start Time: {start_time}"), "\n")
  rayshader::render_highquality(
    filename = ,
    preview = TRUE,
    light = FALSE,
    environment_light = "assets/env/brown_photostudio_02_4k.hdr",
    intensity = 1,
    rotate_env = 90,
    parallel = TRUE,
    width = 5000, 
    height = 5000,
    interactive = FALSE
  )
  end_time <- Sys.time()
  cat(glue::glue("End time: {end_time}"), "\n")
  cat(glue::glue("Total time: {end_time - start_time}"), "\n")
}
