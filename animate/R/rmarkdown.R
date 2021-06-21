#' Insert an animated plot into an R Markdown document
#'
#' @param file The exported plot.
# #' @param ... Optional arguments to pass to the `iframe` call from `shiny::tags`.
#'
#' @export
insert_animate <- function(file) {
  dir0 <- tempdir()
  asset <- function(x) system.file(x, package = "animate")
  script <- function(x) as.character(to_shiny_tag(x))

  # args <- list(...)
  # if (!"style" %in% names(args)) {
  device <- jsonlite::fromJSON(file)$device
  style <- paste0("border: none;",
                  sprintf("width: %spx;", device$width + 50),
                  sprintf("height: %spx;", device$height + 50))
    # args$style <- style
  # }

  # Create data script
  data_str <- read_file(compile_data(file))

  # Create initialisation script
  object_name <- filename_to_variable(basename(file))
  init_str <- paste(
    "JS_device = new plot2(-1);",
    sprintf("JS_device.import(%s);", object_name),
    "ctrl = new controller(JS_device);",
    "ctrl.loop();",
    sep = "\n"
  )

  # Create html app
  html_str <- paste(
    "<!DOCTYPE html><html><head></head><body>",
    script(asset("assets/d3.v5.min.js")),
    script(asset("assets/ramda.min.js")),
    script(asset("assets/broadcast.js")),
    script(asset("assets/d3-symbol-extra.min.js")),
    script(asset("dist/animate.js")),
    sprintf("<script>%s</script>", data_str),
    sprintf("<script>%s</script>", init_str),
    "</body></html>",
    sep = "\n"
  )

  shiny::tags$iframe(srcdoc = html_str, style = style)
}


# Convert JSON to JS ----------------------------------------------------------
compile_data <- function(input, output = tempfile(), ...) {
  if (file.exists(output)) {
    message(sprintf("The output data file '%s' already exists. I'll load it instead of re-compiling the input file.",
                    output))
    return(output)
  }
  write(to_json(input, ...), file = output)
  output
}

to_json <- function(input) {
  fname <- basename(input)
  sym <- filename_to_variable(fname)
  json <- paste(readLines(input), collapse = "\n")
  sprintf("const %s = JSON.parse(%s)", sym, sQuote(json, "'"))
}

filename_to_variable <- function(x) {
  gsub(x = x, pattern = "[.]|-", replacement = "_")
}


# Read a file into a string ---------------------------------------------------
read_file <- function(x) {
  paste(readLines(x), collapse = "\n")
}


# Load JavaScript -------------------------------------------------------------
to_shiny_tag <- function(src, ...) {
  if (is_web_link(src)) {
    return(shiny::tags$script(src = src, ...))
  }

  if (is_local(src)) {
    data_uri <- base64enc::dataURI(file = src, mime = "text/javascript")
    return(shiny::tags$script(src = data_uri, ...))
  }

  stop("File '", basename(src), "' is not supported.")
}

is_web_link <- function(x) has_prefix(x, "http://") || has_prefix(x, "https://")

is_local <- function(x) !is_web_link(x)

has_prefix <- function(x, y) substr(x, 1, nchar(y)) == y
