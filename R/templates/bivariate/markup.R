# Load packages and "add_main_annotations" function
for (f in list.files("R/utils")) {
  source(paste0("R/utils/", f, collapse = ""))
}
#' 
#' # Load 'header' list with needed data
header <- readRDS("R/portraits/CONFIG_MAP/header.rds")
#'
#' # Take original graphic from 'render_highquality' and add annotations
#' # Burning graphic
add_main_annotations(map = header$map, pal = header$pal,
                     text_color = header$colors[4],
                     align = "center",
                     base_coords = c(.25, .06),
                     offset = 450,
                     main_text = "Malawi",
                     main_size = 350,
                     main_font = "Chiller",
                     secondary_text = "Mean Annual Temperature\n \n&\n \nMean Total Precipitation\n \n(1981—2010)",
                     secondary_size = 120,
                     secondary_font = "Ink Free",
                     caption_size = 80,
                     caption_font = "Ink Free",
                     caption_coords = c(.5, .97),
                     caption_align = "center",
                     twitter_icon_coords = c(-650, 75),
                     twitter_icon_size = 65,
                     data_source = "CHELSA-bioclim and AWS Terrain Tiles",
                     original = header$outfile,
                     crop = c(3750, 4900),
                     crop_gravity = "center",
                     crop_start = NULL,
                     svg_file = NULL,
                     svg_coords = NULL,
                     svg_size = NULL,
                     inset = glue("images/{header$map}/{header$map}_{header$pal}_inset.png"),
                     inset_coords = c(.25, .35),
                     inset_size = 1100,
                     north_arrow = TRUE,
                     north_arrow_file = "assets/arrows/orange.png",
                     north_arrow_coords = c(0.85, 0.1),
                     north_arrow_size = 250,
                     north_arrow_alpha = 1)