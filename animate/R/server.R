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
    #' @param use_websocket TRUE / FALSE; whether to enable websocket connection.
    #' @param import A file path; needed when one wants to detach a plot from R.
    #' @param stack_size An integer; the number of commands `plot_stack` can hold.
    #' Use -1 for unlimited number of commands.
    initialize = function(use_websocket = TRUE, import, stack_size = -1) {
      # Start WebSocket connection
      conn <- sketch::websocket$new()
      conn$startServer()
      self$connection <- conn
      self$stack_size <- stack_size

      # Configure the application
      lines <- system.file("plot.R", package = "animate")
      if (use_websocket) {
        lines <- append(lines, "#! load_library('websocket')", 4)
      }
      if (!missing(import)) {
        import_line <- paste0("#! load_data('", import, "')")
        lines <- append(lines, import_line, 5)

      }
      main <- paste0("JS_device <- plot2$new(", stack_size, ")")
      lines <- append(lines, main)

      f <- tempfile(pattern = "temp_", fileext = ".R")
      writeLines(lines, f)
      sketch::source_r(f)
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
    #' @param cw Width of the canvas in pixels.
    #' @param ch Height of the canvas in pixels.
    #' @param ... Additional graphical parameters.
    # @param root Character; a selector path to the container of the canvas.
    # @param id Character; the id of the SVG element
    init = function(cw = 800, ch = 600, ...) {
      self$send(Message("fn_init_svg", list(cw = cw, ch = ch, ...)))
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
    #' Import an animated plot
    import = function() {
      self$send(Message("fn_import", list()))
    },

    #' @description
    #' Export an animated plot
    export = function() {
      self$send(Message("fn_export", list()))
    }
  ),
  list()
)
