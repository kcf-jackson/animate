#' Build and configure the application
build <- function(stack_size = 0, debug = FALSE, keep = FALSE) {
  if (basename(getwd()) != "animate") {
    stop("You seem to be in the wrong folder to call this function.")
  }
  curwd <- getwd()
  on.exit(setwd(curwd))
  # Need to go to the main directory for compiling the R files into JS files
  message("Building the app...")
  setwd("./inst/src/")
  lines <- readLines("plot.R")
  lines <- append(lines, "#! load_library('websocket')", 4)
  lines <- append(lines, "#! load_script('message.R')", 5)
  lines <- append(lines, glue::glue("JS_device <- plot2$new({stack_size})"))

  message("Writing to ", file.path("./inst/src", "dist.R"))
  write(lines, "dist.R")

  message("Writing to ", file.path("./inst/dist", "animate.html"))
  res <- file.copy(
    from = sketch::source_r("dist.R", debug = debug, launch_browser = NULL),
    to = "../dist/animate.html",
    overwrite = T
  )

  if (!keep) {
    message("Cleaning up ", file.path("./inst/src", "dist.R"))
    file.remove("./dist.R")
  }

  res
}


#' Build detach mode
build_detach <- function(file) {
  if (basename(getwd()) != "animate") {
    stop("You seem to be in the wrong folder to call this function.")
  }
  curwd <- getwd()
  on.exit(setwd(curwd))
  setwd("./inst")

  lines <- paste0("#! load_data('", file, "')")
  object_name <- paste0(tools::file_path_sans_ext(basename(file)), "_json")
  lines <- append(lines, paste0("JS_device$import(", object_name,")"))
  lines <- append(lines, "JS_device$loop()")

  dir0 <- tempdir()
  temp <- file.path(dir0, "detach.R")
  write(lines, temp)

  temp2 <- file.path(dir0, "detach.js")
  compile_r(temp, temp2, rules = basic_rules())

  file.copy(temp2, "dist/detach.js")
}


#' Build shiny
build_shiny <- function(shiny_helper_file) {
  message("Writing to ./inst/dist/shiny.js")
  compile_r(shiny_helper_file, "./inst/dist/shiny.js")
}


#' Build a library
#'
#' @param main_file A character string; the file path to the app file.
#'
#' @export
build_library <- function(main_file) {
  # The bundled file should contain all the dependencies of the main file 'plot.R',
  # and then the main file itself.
  if (!file.exists(main_file)) {
    stop("File '", main_file, "' does not exist.")
  }
  dep <- sketch:::get_dependencies(main_file)

  message("Building the library...")
  curwd <- getwd()
  on.exit(setwd(curwd))
  src_path <- dirname(main_file)
  setwd(src_path)

  js <- bundle(c(sketch::src("dom"), sketch::src("io"), dep, basename(main_file)))
  message("Writing to ./inst/dist/animate.js")
  file.copy(js, "../dist/animate.js", overwrite = TRUE)
}


#' Bundle a list of files into a single JavaScript file
#'
#' @param fs A character vector; a list of files.
#'
#' @note This is needed for the shiny app.
#'
#' @export
bundle <- function(fs) {
  fs <- flatten_files(fs, "[.]((r)|(js))$", full.names = TRUE, recursive = TRUE,
                      ignore.case = TRUE)
  res_js <- file.path(tempdir(), "bundle.js")
  for (file in fs) {
    if (!file.exists(file) && !dir.exists(file)) {
      stop("Path '", file, "' does not exist.")
    }
    file_extension <- tolower(tools::file_ext(file))
    if (file_extension %in% c("r", "js")) {
      if (file_extension == "r") {
        temp <- tempfile()
        sketch::compile_r(file, temp)
      } else if (file_extension == "js") {
        temp <- file
      }
      lines <- readLines(temp)
      write(lines, file = res_js, append = TRUE)
      write("\n\n", file = res_js, append = TRUE)
    } else {
      warning("File '", file, "' is not processed.")
    }
  }
  res_js
}


#' Flatten a list of files / directories into a list of files
#'
#' @param fs A character vector; a list of files.
#' @param pattern An optional regular expression to pass to `list.files` for
#' filtering files while expanding a directory into a list of files.
#' @param ... Additional parameter to pass to `list.files`.
#'
#' @export
flatten_files <- function(fs, pattern = NULL, ...) {
  fs %>%
    purrr::map(function(path) {
      if (!file.exists(path) && !dir.exists(path)) {
        stop("Path '", path, "' does not exist.")
      }
      if (file.exists(path)) {
        return(path)
      }
      if (dir.exists(path)) {
        return(list.files(path, pattern, ...))
      }
    }) %>%
    unlist()
}
