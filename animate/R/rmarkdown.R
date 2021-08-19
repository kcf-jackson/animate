#' Insert an animated plot into an R Markdown document
#'
#' @param file The exported plot.
#' @param options A character string; the JavaScript to customise the playback options.
#' Two basic options `click_to_play()` and `click_to_loop()` have been implemented
#' for general usage.
#' @param style Optional style for the iframe that hosts the visualisation.
#'
#' @export
insert_animate <- function(file, options = click_to_play(), style) {
  dir0 <- tempdir()
  asset <- function(x) system.file(x, package = "animate")
  script <- function(x) as.character(to_shiny_tag(x))

  # args <- list(...)
  # if (!"style" %in% names(args)) {
  device <- jsonlite::fromJSON(file)$device
  if (missing(style)) {
    style <- paste0("border: none;",
                    sprintf("width: %spx;", device$width + 50),
                    sprintf("height: %spx;", device$height + 50))
  }
    # args$style <- style
  # }

  # Create data script
  data_str <- read_file(compile_data(file))

  # Create initialisation script
  object_name <- filename_to_variable(basename(file))
  init_str <- paste(
    "JS_device = new plot2(-1);",
    sprintf("JS_device.import(%s);", object_name),
    options,
    sep = "\n"
  )

  # Create html app
  html_str <- paste(
    "<!DOCTYPE html><html><head></head><body>",
    script(asset("dist/animate.js")),
    sprintf("<script>%s</script>", data_str),
    sprintf("<script>%s</script>", init_str),
    "</body></html>",
    sep = "\n"
  )

  shiny::tags$iframe(srcdoc = html_str, style = style)
}

#' Click an element to play a frame
#' @param selector The ID of the DOM element.
#' @param start An integer; the number of frames to execute upon the beginning
#' of the visualisation. This is useful when one wants to start with some set-up
#' instead of an empty canvas.
#' @export
click_to_play <- function(selector = "#SVG_1", start = 2) {
  sprintf(
    'ctrl = new controller(JS_device);
    ctrl.play_until(%s, 1, () => {
      ctrl.watch("%s", "click", function() {
        if (ctrl.player_pointer == 0) {
          ctrl.play_until(%s, 1);
        } else {
          ctrl.play();
        }
      });
    })',
    start, selector, start
  )
}

#' Click an element to play all frames
#' @param selector The ID of the DOM element.
#' @param start An integer; the number of frames to execute upon the beginning
#' of the visualisation. This is useful when one wants to start with some set-up
#' instead of an empty canvas.
#' @param wait A number; the number of milliseconds to wait for before the
#' next frame is drawn.
#' @export
click_to_loop <- function(selector = "#SVG_1", start = 2, wait = 20) {
  sprintf(
    'ctrl = new controller(JS_device);
    ctrl.play_until(%s, 1, () => {
      ctrl.watch("%s", "click", function() { ctrl.play_until(0, %s) });
    })',
    start, selector, wait
  )
}

#' Loop through the available frames n times
#' @param times An integer; the number of times to loop.
#' @param wait A number; the number of milliseconds to wait for before the
#' next frame is drawn.
#' @export
loop <- function(times = 1, wait = 20) {
  sprintf(
    "ctrl = new controller(JS_device);
    ctrl.loop(%s, %s);",
    times, wait
  )
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
