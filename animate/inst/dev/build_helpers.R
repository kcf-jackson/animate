#' Build and configure the application
build <- function(stack_size = 0, debug = FALSE, keep = FALSE) {
  if (basename(getwd()) != "animate") {
    stop("You seem to be in the wrong folder to call this function.")
  }
  curwd <- getwd()
  on.exit(setwd(curwd))
  # Need to go to the main directory for compiling the R files into JS files
  setwd("./inst/src/")
  lines <- readLines("plot.R")
  lines <- append(lines, "#! load_library('websocket')", 4)
  lines <- append(lines, "#! load_script('message.R')", 5)
  lines <- append(lines, glue::glue("JS_device <- plot2$new({stack_size})"))

  message("Writing to ", file.path("./", "dist.R"))
  write(lines, "dist.R")

  message("Writing to ", file.path("../dist", "animate.html"))
  res <- file.copy(
    from = sketch::source_r("dist.R", debug = debug, launch_browser = NULL),
    tp = "./dist/animate.html",
    overwrite = T
  )
  if (!keep) file.remove("./dist.R")

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


#' Bundle a list of files into
#'
#' @param fs A character vector; a list of files
#'
#' @export
bundle <- function(fs) {
  res_js <- file.path(tempdir(), "bundle.js")
  for (file in fs) {
    if (!file.exists(file)) stop("File '", file, "' does not exist.")
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
