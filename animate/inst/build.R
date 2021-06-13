# Build and configure the application
build <- function(stack_size = 0, debug = FALSE) {
  curwd <- getwd()
  on.exit(setwd(curwd))
  setwd("./inst")
  lines <- readLines("plot.R")
  lines <- append(lines, "#! load_library('websocket')", 4)
  lines <- append(lines, "#! load_script('message.R')", 5)
  lines <- append(lines, glue::glue("JS_device <- plot2$new({stack_size})"))

  message("Writing to ", file.path("./inst", "dist.R"))
  write(lines, "dist.R")

  message("Writing to ", file.path("./inst/dist", "websocket_dist.html"))
  file.copy(sketch::source_r("dist.R", debug = debug, launch_browser = NULL),
            "./dist/websocket_dist.html", overwrite = T)
}

build(0, TRUE)


# Build detach mode
build_detach <- function() {
  curwd <- getwd()
  on.exit(setwd(curwd))
  setwd("./inst")
  lines <- readLines("plot.R")
  lines <- append(lines, glue::glue("#! load_data('{file}')"), 5)
  object_name <- paste0(tools::file_path_sans_ext(basename(file)), "_json")
  lines <- append(lines, paste0("JS_device$import(", object_name,")"))
  lines <- append(lines, "JS_device$loop()")
}


# ORIGINAL
# # Detach mode
# if (!missing(file)) {
#   import_line <- paste0("#! load_data('", file, "')")
#   lines <- append(lines, import_line, 5)
#   object_name <- paste0(tools::file_path_sans_ext(basename(file)), "_json")
#   lines <- append(lines, paste0("JS_device$import(", object_name,")"))
#   lines <- append(lines, "JS_device$loop()")
# }
