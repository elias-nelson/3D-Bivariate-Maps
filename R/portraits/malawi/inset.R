#-------------------------------------------------------------------------------
# Create Inset Map
#-------------------------------------------------------------------------------

# i. Required Packages
# --------------------------------------------------
# Ensure required packages are installed and loaded.
# {pacman} is used for streamlined package management.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, sf, glue, rnaturalearth, colorspace
)

# ii. Load Configuration from Main Workflow
# --------------------------------------------------
# Load previously saved objects (e.g., map name, palette, coordinates, colors)
# from the main script to ensure consistency across outputs

header <- readRDS(glue::glue("R/portraits/{map}/header.rds"))

# Extract centroid coordinates (longitude, latitude)
coords <- header$coords

# Extract color palette used in the main map
colors <- header$colors

# iii. Load Global Basemap Data
# --------------------------------------------------
# Retrieve country boundaries for global context

world <- rnaturalearth::ne_countries(
  scale = "small",
  returnclass = "sf"
)

# iv. Define Orthographic Projection (Globe View)
# --------------------------------------------------
# Create a globe-like projection centered on the study location

prj <- glue(
  "+proj=ortho +lat_0={coords[2]} +lon_0={coords[1]} ",
  "+x_0=0 +y_0=0 +a=6375000 +b=6375000 +units=m +no_defs"
)

# v. Create Circular "Globe" Mask
# --------------------------------------------------
# Generate a circular geometry representing the visible hemisphere

water <- st_sfc(st_point(c(0, 0)), crs = prj) |>
  st_buffer(6371000) |>        # approximate Earth radius (meters)
  st_transform(crs = 4326)

# Extract and clean boundary coordinates
circle_coords <- st_coordinates(water)[, c(1, 2)] 

circle_coords <- circle_coords[order(circle_coords[, 1]), ]
circle_coords <- circle_coords[!duplicated(circle_coords), ]

# vi. Construct Clipping Geometry
# --------------------------------------------------
# Build a polygon that combines the globe edge with a bounding box
# to properly clip global geometries for the orthographic projection

rectangle <- list(
  rbind(
    circle_coords,
    c(X = 180, circle_coords[nrow(circle_coords), "Y"]),
    c(X = 180, Y = -90),
    c(X = -180, Y = -90),
    c(X = -180, circle_coords[1, "Y"]),
    circle_coords[1, c("X", "Y")]
  )
) |>
  st_polygon() |>
  st_sfc(crs = 4326)

rectangle %>%
  ggplot()+
  geom_sf(data = world) +
  geom_sf(color = "red", fill = alpha("red", .5)) 

# vii. Clip World Data to Globe Extent
# --------------------------------------------------
# Disable spherical geometry (s2) for compatibility with intersection

sf::sf_use_s2(FALSE)

# Intersect world map with clipping polygon
w <- st_intersection(world, rectangle)

w %>%
  ggplot() +
  geom_sf()

# viii. Define Location Marker
# --------------------------------------------------
# Create a spatial point representing the study location

spot <- tibble(
  x = coords[1],
  y = coords[2]
) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

# ix. Define Color Scheme for Inset
# --------------------------------------------------
# Reuse colors from main map for visual consistency

colorspace::swatchplot(colors)

water_color <- colors[1]
land_color  <- colors[4]

# x. Create Globe Inset Map
# --------------------------------------------------
# Compose the orthographic globe with:
#   - water background
#   - clipped land polygons
#   - highlighted study location

loc_plot <- ggplot(data = w) +
  geom_sf(data = water, color = NA, fill = alpha(water_color, 0.75)) +
  geom_sf(fill = land_color, size = 0.1, color = "white") +
  
  # Layered point styling for emphasis (halo effect)
  geom_sf(data = spot, fill = colors[9], stroke = 0, size = 8, shape = 21) +
  geom_sf(data = spot, fill = colors[5], stroke = 0, size = 6, shape = 21) +
  geom_sf(data = spot, fill = colors[1], stroke = 0, size = 4, shape = 21) +
  
  coord_sf(crs = prj) +
  theme_void()

# xi. Display Inset Map
# --------------------------------------------------
print(loc_plot)

# xii. Save Inset Map
# --------------------------------------------------
# Export as PNG for integration into main figure

ggsave(
  plot = loc_plot,
  filename = glue("images/{header$map}/{header$map}_{header$pal}_inset.png"),
  width = 4 * 1.5,
  height = 3 * 1.5
)
