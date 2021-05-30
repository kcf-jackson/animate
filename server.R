Message <- function(type, message) {
  list(type = type, message = message)
}


plot2 <- R6::R6Class(
  "plot2",
  list(
    connection = NULL,
    plot_stack = list(),
    stack_size = -1,  # -1 refers to unlimited

    # WebSocket ---------------------------------------------------------------
    #' Constructor of the device
    initialize = function(stack_size = -1) {
      # Start WebSocket connection
      conn <- sketch::websocket$new()
      conn$startServer()
      self$connection <- conn
      self$stack_size <- stack_size
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

    #' Keep track of the processed commands / data
    #' @param data The data to be processed by the device.
    record = function(data) {
      if (self$stack_size == -1) {
        self$plot_stack$push(data)
        return(self$plot_stack)
      }

      if (self$plot_stack$length < self$stack_size) {
        self$plot_stack$push(data)
        return(self$plot_stack)
      }

      if (self$plot_stack$length == self$stack_size) {
        self$plot_stack$shift()
        self$plot_stack$push(data)
        return(self$plot_stack)
      }

      if (self$plot_stack$length > self$stack_size) {
        stop("Plot stack exceeded the allocated size. There may be racing issues.")
      }

      stop("This line should not be reached. Please raise an issue on Github. Your `plot_stack` has size " %+%
             self$plot_stack$length %+% ", and your allocated `stack_size` is " %+%
             self$stack_size %+% "(note that -1 corresponds to 'unlimited').")
    }


    # Functions ---------------------------------------------------------------
    #' Initialise a SVG element
    #' @param cw Width of the canvas in pixels.
    #' @param ch Height of the canvas in pixels.
    # Optional
    #' @param root Character; a selector path to the container of the canvas.
    #' @param id Character; the id of the SVG element
    init = function(cw = 800, ch = 600, ...) {
      self$send(Message("fn_init_svg", list(cw = cw, ch = ch, ...)))
    },

    # points
    points = function(x, y, ...) {
      self$send(Message("fn_points", list(x = x, y = y, ...)))
    },

    # lines / paths
    lines = function(x, y, ...) {
      self$send(Message("fn_lines", list(x = x, y = y, ...)))
    },

    # text
    text = function(x, y, text, ...) {
      self$send(Message("fn_text", list(x = x, y = y, text = text, ...)))
    }

    # export
  ),
  list()
)


# Main
device <- plot2$new()
device$init(600, 400)
device$points(1:10, rnorm(10))
device$points(1:10, rnorm(10), bg = "blue")
device$points(1:10, rnorm(10), bg = "black", col = "red", cex = 100, lwd = 2)
device$points(1:10, rnorm(10), style = list(fill = "pink"))
device$points(1:10, rnorm(10), bg = "blue", cex = 100,
              transition = list(duration = 2000))
device$off()
