message <- function(type, message) {
  list(type = type, message = message)
}


plot2 <- R6::R6Class(
  "plot2",
  list(
    connection = NULL,
    plot_stack = list(),

    # WebSocket ---------------------------------------------------------------
    #' Constructor of the device
    initialize = function() {
      # Start WebSocket connection
      conn <- httpuv::WebSocket$new()
      conn$startServer()
      self$connection <- conn
      sketch::source_r("plot.R")
    },

    #' Switch off the device
    #' @description This function closes the WebSocket connection
    off = function() {
      self$connection$stopServer()
    },

    #' Send commands to device
    #' @param message The message to send to the device.
    send = function(message) {
      self$connection$ws$send(
        jsonlite::toJSON(message, auto_unbox = T)
      )
    },


    # Functions ---------------------------------------------------------------
    #' Initialise a SVG element
    #' @param cw Width of the canvas in pixels.
    #' @param ch Height of the canvas in pixels.
    #' @param root Character; a selector path to the container of the canvas.
    init = function(cw = 800, ch = 600, root = "body", id) {
      self$send(message("fn_init_svg",
                        list(width = cw, height = ch, root = root, id = id)))
    },


    # Plot the points
    points = function(x, y, ...) {
      self$send(message("fn_points", list(x = x, y = y, ...)))
    }

    # lines / paths
    # text
    # export

  ),
  list()
)


# Main
device <- plot2$new()
device$init(800, 600)
x <- rnorm(10)
device$points(1:10, rnorm(10))
device$off()
