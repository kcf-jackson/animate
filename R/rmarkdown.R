#' In-line rendering of an animated plot in an R Markdown document
#'
#' @param device The \link{animate} object.
#' @param ... Optional parameters to pass to \link{insert_animate}.
#'
#' @note This function should only be used in a code chunk of an R Markdown document.
#'
#' @examples
#' \donttest{
#' input <- tempfile(fileext = ".Rmd")
#' output <- tempfile(fileext = ".html")
#' writeLines('
#' ```{r, echo = FALSE, message = FALSE}
#' # Run / include the following in a code chunk of an R Markdown document
#' library(animate)
#' device <- animate$new(500, 500, virtual = TRUE)  # set `virtual = TRUE` for R Markdown document
#' attach(device)
#'
#' # Data
#' id <- new_id(1:10)
#' s <- 1:10 * 2 * pi / 10
#' s2 <- sample(s)
#'
#' # Plot
#' par(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
#' plot(2*sin(s), 2*cos(s), id = id)
#' points(sin(s2), cos(s2), id = id, transition = list(duration = 2000))
#'
#' # Render in-line in an R Markdown document
#' rmd_animate(device, click_to_play(start = 3))  # begin the plot at the third frame
#' ```
#' ```{r, echo = FALSE, message = FALSE}
#' par(xlim = NULL, ylim = NULL)  # Reset `xlim` and `ylim` in `par`
#' # Do some other plots
#' off()
#' detach(device)
#' ```
#' ', input)
#' knitr::knit(input, output)
#' # browseURL(output)
#' }
#'
#' @export
rmd_animate <- function(device, ...) {
  if (!device$virtual_meta$virtual) {
    stop("Device is not virtual. Virtual device should be used for inline RMarkdown usage. Did you forget to set 'virtural = TRUE' in your `animate$new` function call?")
  }
  json_file <- tempfile(pattern = "test", fileext = ".json")
  json_obj <- glue::glue('{
    "plot_commands": <device$virtual_session$get()>,
    "device": {
      "selection": "", "id": "", "par": {},
      "height": <device$virtual_meta$height>,
      "width": <device$virtual_meta$width>
    },
    "max_num_commands": -1
  }', .open = "<", .close = ">")
  write(json_obj, json_file)
  R.utils::gzip(json_file)
  insert_animate(paste0(json_file, ".gz"), ...)
}


#' Insert an animated plot into an R Markdown document
#'
#' @param file The exported plot.
#' @param options A character string; the JavaScript to customise the playback options.
#' Two basic options `click_to_play()` and `click_to_loop()` have been implemented
#' for general usage.
#' @param style Optional style for the iframe that hosts the visualisation.
#' @param use_cdn TRUE / FALSE; if TRUE, serve the assets from a CDN, otherwise
#' embed the assets into the HTML.
#'
#' @examples
#' \donttest{
#' input <- tempfile(fileext = ".Rmd")
#' output <- tempfile(fileext = ".html")
#' writeLines('
#' ```{r, echo = FALSE, message = FALSE}
#' # Run / include the following in a code chunk of an R Markdown document
#' library(animate)
#' insert_animate(system.file("tests/Lorenz_system.json.gz", package = "animate"),
#'                options = click_to_loop())
#' ```
#' ', input)
#' knitr::knit(input, output)
#' # browseURL(output)
#' }
#'
#' @export
insert_animate <- function(file, options = click_to_play(), style,
                           use_cdn = TRUE) {
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
  asset_script <- if (use_cdn) {
    script("https://cdn.jsdelivr.net/gh/kcf-jackson/animate@latest/inst/dist/animate.js")
  } else {
    script(asset("dist/animate.js"))
  }

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
    asset_script,
    sprintf("<script>%s</script>", data_str),
    sprintf("<script>%s</script>", init_str),
    "</body></html>",
    sep = "\n"
  )

  shiny::tags$iframe(srcdoc = html_str, style = style)
}

#' Click an element to play a frame
#'
#' @description Playback option for the functions \link{rmd_animate}
#' and \link{insert_animate}.
#'
#' @param selector The ID of the DOM element.
#' @param start An integer; the number of frames to execute upon the beginning
#' of the visualisation. This is useful when one wants to start with some set-up
#' instead of an empty canvas.
#'
#' @examples
#' \donttest{
#' input <- tempfile(fileext = ".Rmd")
#' output <- tempfile(fileext = ".html")
#' writeLines('
#' ```{r, echo = FALSE, message = FALSE}
#' # Run / include the following in a code chunk of an R Markdown document
#' library(animate)
#' insert_animate(system.file("tests/basic_points.json", package = "animate"),
#'                options = click_to_play())
#' ```
#' ', input)
#' knitr::knit(input, output)
#' # browseURL(output)
#' }
#'
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
#'
#' @description Playback option for the functions \link{rmd_animate}
#' and \link{insert_animate}.
#'
#' @param selector The ID of the DOM element.
#' @param start An integer; the number of frames to execute upon the beginning
#' of the visualisation. This is useful when one wants to start with some set-up
#' instead of an empty canvas.
#' @param wait A number; the number of milliseconds to wait for before the
#' next frame is drawn.
#'
#' @examples
#' \donttest{
#' input <- tempfile(fileext = ".Rmd")
#' output <- tempfile(fileext = ".html")
#' writeLines('
#' ```{r, echo = FALSE, message = FALSE}
#' # Run / include the following in a code chunk of an R Markdown document
#' library(animate)
#' insert_animate(system.file("tests/Lorenz_system.json.gz", package = "animate"),
#'                options = click_to_loop())
#' ```
#' ', input)
#' knitr::knit(input, output)
#' # browseURL(output)
#' }
#'
#' @export
click_to_loop <- function(selector = "#SVG_1", start = 2, wait = 20) {
  sprintf(
    'ctrl = new controller(JS_device);
    ctrl.play_until(%s, 1, function() {
      ctrl.watch("%s", "click", function callback() {
        ctrl.unwatch("%s", "click", callback);
        ctrl.play_until(0, %s, () => ctrl.watch("%s", "click", callback));
      });
    })',
    start, selector, selector, wait, selector
  )
}

#' Loop through the available frames n times
#'
#' @description Playback option for the functions \link{rmd_animate}
#' and \link{insert_animate}.
#'
#' @param times An integer; the number of times to loop.
#' @param wait A number; the number of milliseconds to wait for before the
#' next frame is drawn.
#'
#' @examples
#' \donttest{
#' input <- tempfile(fileext = ".Rmd")
#' output <- tempfile(fileext = ".html")
#' writeLines('
#' ```{r, echo = FALSE, message = FALSE}
#' # Run / include the following in a code chunk of an R Markdown document
#' library(animate)
#' insert_animate(system.file("tests/Lorenz_system.json.gz", package = "animate"),
#'                options = loop(times = 2, wait = 15))
#' ```
#' ', input)
#' knitr::knit(input, output)
#' # browseURL(output)
#' }
#'
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
  if (tolower(tools::file_ext(fname)) == "gz") {
    json <- jsonlite::toJSON(readBin(input, "int", n = 1e6, size = 1, signed = FALSE))
    sprintf("const %s = JSON.parse(pako.inflate(%s, {to: 'string'}))", sym, json)
  } else {
    json <- paste(readLines(input), collapse = "\n")
    glue::glue("const {sym} = {json}")
  }
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
