Message <- function(type, message) {
  list(type = type, message = message)
}

#' A web-based graphics device
#' @export
animate <- R6::R6Class(
  "animate",
  list(
    #' @field connection A handle for the WebSocket connection.
    connection = NULL,

    #' @field ready_state The ready state of the connection.
    ready_state = 0,

    #' @field shiny Whether the device is used with shiny.
    shiny = FALSE,

    #' @field session A shiny session.
    session = NULL,

    # WebSocket ----------------------------------------------------------------
    #' @description
    #' Constructor of the device
    #' @param width An integer; the width in pixels.
    #' @param height An integer; the height in pixels.
    #' @param id A character string; the id assigned to the device.
    #' @param ... Additional arguments.
    initialize = function(width, height, id = "SVG_1", ...) {
      if (private$is_shiny(...)) {
        self$shiny <- TRUE
        args <- list(...)

        self$session <- args$session
        self$session$sendCustomMessage("animate", message)

        args$session <- NULL
        js_args <- append(list(width = width, height = height, id = id), args)
        return(do.call(self$init, js_args))
      }

      args <- append(list(width = width, height = height, id = id), list(...))
      in_handler <- function(x) {
        msg <- jsonlite::fromJSON(x)
        if (msg$type == "WebSocket.onopen") {
          self$ready_state <- 1
          message("Device is ready.")
          do.call(self$init, args)
        }
      }

      # Start WebSocket connection
      conn <- websocket$new(in_handler = in_handler)
      conn$startServer()
      self$connection <- conn

      # Serve the app
      app <- system.file("dist/animate.html", package = "animate")
      temp <- file.path(tempdir(), "index.html")
      file.copy(app, temp)
      viewer <- ifelse(rstudioapi::isAvailable(), rstudioapi::viewer, utils::browseURL)
      viewer(temp)
    },

    #' @description
    #' Switch off the device; this function closes the WebSocket connection
    off = function() {
      if (self$shiny) return(NULL)

      self$ready_state <- 0
      self$connection$stopServer()
    },

    #' @description
    #' Send commands to device
    #' @param message The message to send to the device.
    send = function(message) {
      if (self$shiny) return(private$shiny_send(message))

      if (self$ready_state == 0) {
        message("Device is not yet available.")
      } else {
        self$connection$ws$send(
          jsonlite::toJSON(message, auto_unbox = T)
        )
      }
    },

    #' @description
    #' Set the maximum size of the stack
    #' @param n The number of commands the plot stack can hold. Use
    #' -1 for unlimited number of commands.
    set_max_stacksize = function(n) {
      self$send(Message("fn_max_stacksize", list(n = n)))
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
    #' @param x The x coordinates of the bars.
    #' @param y The y coordinates of the bars.
    #' @param w The width of the bars.
    #' @param h The height of the bars.
    #' @param ... Additional graphical parameters.
    bars = function(x, y, w, h, ...) {
      self$send(Message("fn_bars", list(x = x, y = y, w = w, h = h, ...)))
    },

    #' @description
    #' Generic X-Y plotting
    #' @param x The x coordinates of the data.
    #' @param y The y coordinates of the data.
    #' @param type Type of the plot; one of 'p' and 'l'.
    #' @param ... Additional graphical parameters.
    plot = function(x, y, type = "p", ...) {
      self$send(Message("fn_plot", list(x = x, y = y, type = type, ...)))
    },

    #' @description
    #' Add points to a plot
    #' @param x The x coordinates of the points.
    #' @param y The y coordinates of the points.
    #' @param ... Additional graphical parameters.
    points = function(x, y, ...) {
      self$send(Message("fn_points", list(x = x, y = y, ...)))
    },

    #' @description
    #' Add line segments / paths to a plot
    #' @param x The x coordinates of the line.
    #' @param y The y coordinates of the line.
    #' @param ... Additional graphical parameters.
    lines = function(x, y, ...) {
      self$send(Message("fn_lines", list(x = x, y = y, ...)))
    },

    #' @description
    #' Add text to a plot
    #' @param x The x coordinates of the text.
    #' @param y The y coordinates of the text.
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
    #' Set the graphical parameters
    #' @param parameters The graphical parameters
    par = function(parameters) {
      self$send(Message("fn_par", parameters))
    },

    #' @description
    #' Remove a SVG element
    #' @param selector A character vector; the CSS selector.
    #' @param id A character string; ID of an element.
    remove = function(selector = "*", id) {
      self$send(Message("fn_remove", list(selector = selector, id = id)))
    },

    #' @description
    #' Remove all elements of the active SVG element
    clear = function() {
      self$send(Message("fn_clear", list()))
    },

    #' #' @description
    #' #' Perform a group of graphical operations to a plot
    #' #' @param ... Any number of graphical operations.
    #' group = function(...) {
    #'   self$send(Message("fn_group", c(...)))
    #' },

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
  # Private fields and methods -------------------------------------------------
  list(
    shiny_send = function(message) {
      self$session$sendCustomMessage("animate", message)
    },
    is_shiny = function(...) {
      args <- list(...)
      "session" %in% names(args)
    }
  )
)
