Message <- function(type, message) {
  list(type = type, message = message)
}

#' A web-based graphics device
#' @export
plot2 <- R6::R6Class(
  "plot2",
  list(
    #' @field connection A handle for the WebSocket connection.
    connection = NULL,

    #' @field plot_stack A stack to store the processed commands. This can be
    #' exported to generate a standalone plot.
    plot_stack = list(),

    #' @field stack_size The number of commands `plot_stack` can hold. Use
    #' -1 for unlimited number of commands.
    stack_size = -1,

    # WebSocket ---------------------------------------------------------------
    #' @description
    #' Constructor of the device
    #' @param file A file path; needed when one wants to detach a plot from R.
    #' @param use_websocket TRUE / FALSE; whether to enable websocket connection.
    #' @param stack_size An integer; the number of commands `plot_stack` can hold.
    #' Use -1 for unlimited number of commands.
    initialize = function(file, use_websocket = TRUE, stack_size = -1) {
      self$stack_size <- stack_size

      # Start WebSocket connection
      if (use_websocket) {
        conn <- sketch::websocket$new()
        conn$startServer()
        self$connection <- conn
      }

      # Configure the application
      lines <- readLines(system.file("plot.R", package = "animate"))
      if (use_websocket) {
        lines <- append(lines, "#! load_library('websocket')", 4)
        lines <- append(lines, "#! load_script('message.R')", 5)
      }
      main <- paste0("JS_device <- plot2$new(", stack_size, ")")
      lines <- append(lines, main)

      # Detach mode
      if (!missing(file)) {
        import_line <- paste0("#! load_data('", file, "')")
        lines <- append(lines, import_line, 5)
        object_name <- paste0(tools::file_path_sans_ext(basename(file)), "_json")
        lines <- append(lines, paste0("JS_device$import(", object_name,")"))
        lines <- append(lines, "JS_device$loop()")
      }

      # Write to and serve from a temporary folder
      dir0 <- tempdir()
      message(paste("The App is served from:", dir0))
      file.copy(system.file("assets", package = "animate"), dir0, recursive = TRUE)
      file.copy(system.file("d3_helpers.R", package = "animate"), dir0)
      file.copy(system.file("import.R", package = "animate"), dir0)
      file.copy(system.file("message.R", package = "animate"), dir0)
      temp_file <- file.path(dir0, "plot.R")
      writeLines(lines, temp_file)

      cur_dir <- getwd()
      on.exit(setwd(cur_dir))

      setwd(dir0)
      sketch::source_r("plot.R")
    },

    #' @description
    #' Switch off the device; this function closes the WebSocket connection
    off = function() {
      self$connection$stopServer()
    },

    #' @description
    #' Send commands to device
    #' @param message The message to send to the device.
    send = function(message) {
      self$connection$ws$send(
        jsonlite::toJSON(message, auto_unbox = T)
      )
    },

    # Functions ---------------------------------------------------------------
    #' @description
    #' Initialise a SVG element
    #' @param width Width of the canvas in pixels.
    #' @param height Height of the canvas in pixels.
    #' @param ... Additional graphical parameters.
    # @param root Character; a selector path to the container of the canvas.
    # @param id Character; the id of the SVG element
    init = function(width = 800, height = 600, ...) {
      self$send(Message("fn_init_svg", list(width = width, height = height, ...)))
    },

    #' @description
    #' Add points to a plot
    #' @param x The x coordinates of the plot.
    #' @param y The y coordinates of the plot.
    #' @param ... Additional graphical parameters.
    points = function(x, y, ...) {
      self$send(Message("fn_points", list(x = x, y = y, ...)))
    },

    #' @description
    #' Add line segments / paths to a plot
    #' @param x The x coordinates of the plot.
    #' @param y The y coordinates of the plot.
    #' @param ... Additional graphical parameters.
    lines = function(x, y, ...) {
      self$send(Message("fn_lines", list(x = x, y = y, ...)))
    },

    #' @description
    #' Add text to a plot
    #' @param x The x coordinates of the plot.
    #' @param y The y coordinates of the plot.
    #' @param labels The text.
    #' @param ... Additional graphical parameters.
    text = function(x, y, labels, ...) {
      self$send(Message("fn_text", list(x = x, y = y, labels = labels, ...)))
    },

    #' @description
    #' Add background image to a plot
    #' @param href The link to the image.
    #' @param width The width of the image.
    #' @param height Th height of the image.
    #' @param ... Additional graphical parameters.
    image = function(href, width, height, ...) {
      self$send(Message("fn_image", list(href = href, width = width, height = height, ...)))
    },

    #' @description
    #' Set the active device to a SVG element
    #' @param selector A character vector; ID of the SVG.
    set = function(selector) {
      self$send(Message("fn_set", list(selector = selector)))
    },

    #' @description
    #' Remove a SVG element
    #' @param selector A character vector; ID of an element.
    remove = function(selector) {
      self$send(Message("fn_remove", list(selector = selector)))
    },

    #' @description
    #' Perform a group of graphical operations to a plot
    #' @param ... Any number of graphical operations.
    group = function(...) {
      self$send(Message("fn_group", c(...)))
    },

    #' @description
    #' Import an animated plot
    import = function() {
      self$send(Message("fn_import", list(skip_log = TRUE)))
    },

    #' @description
    #' Export an animated plot
    export = function() {
      self$send(Message("fn_export", list(skip_log = TRUE)))
    }
  ),
  list()
)
