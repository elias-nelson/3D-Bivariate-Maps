# Functions starting with '.' won't show up in the environment window
# but will be available in the GLOBALENV
.new_template <- function(map, template = "bivariate", open_editor = TRUE) {
  
  # Define template path
  template_path <- file.path("R", "templates", template)
  
  # Check template exists
  if (!dir.exists(template_path)) {
    stop(glue::glue("Template '{template}' does not exist in R/templates/"))
  }
  
  # List template files (exclude hidden files)
  files <- list.files(template_path, all.files = FALSE)
  
  if (length(files) == 0) {
    stop(glue::glue("No files found in template folder: {template_path}"))
  }
  
  # Define new output directory
  new_dir <- file.path("R", "portraits", map)
  
  # Prevent overwriting
  if (dir.exists(new_dir)) {
    stop(glue::glue("The directory '{new_dir}' already exists!"))
  }
  
  # Create directory (recursive ensures parent exists)
  dir.create(new_dir, recursive = TRUE)
  
  # Copy and modify template files
  for (f in files) {
    
    src_file <- file.path(template_path, f)
    dest_file <- file.path(new_dir, f)
    
    # Read, replace placeholder, and write
    content <- readLines(src_file, warn = FALSE)
    content <- gsub("CONFIG_MAP", map, content)
    
    writeLines(content, dest_file)
  }
  
  # Optionally open main script
  if (open_editor) {
    main_file <- file.path(new_dir, "render_graphic.R")
    
    if (file.exists(main_file)) {
      file.edit(main_file)
    } else {
      message("render_graphic.R not found in template.")
    }
  }
  
  # Friendly message
  message(glue::glue(
    crayon::bold(
      crayon::yellow(("New portrait created at: {new_dir}"))
    )
  ))
}

message(glue::glue(
  crayon::bold(
    crayon::cyan("Use `.new_template()` to start working on a new portrait.\n")
  )
))
