Message <- function(type, message) {
  list(type = type, message = message)
}

#' A web-based graphics device for animated visualisations
#' @description Extends the 'base' graphics functions to support frame-by-frame
#' animation and keyframes animation.
#' @export
animate <- R6::R6Class(
  "animate",
  list(
    #' @field connection A handle for the WebSocket connection.
    connection = NULL,

    #' @field ready_state The ready state of the connection.
    ready_state = 0,

    #' @field shiny TRUE or FALSE; whether the device is used with in a 'Shiny' app.
    shiny = FALSE,

    #' @field session A 'Shiny' session.
    session = NULL,

    #' @field virtual_meta A list of device metadata.
    virtual_meta = list(virtual = FALSE, width = "", height = ""),

    #' @field virtual_session A virtual session simulated with 'V8'.
    virtual_session = NULL,

    #' @field event_handlers A named list of user-defined functions for handling events.
    event_handlers = list(),

    # WebSocket ----------------------------------------------------------------
    #' @description
    #' Constructor of the device
    #' @param width An integer; the width in pixels.
    #' @param height An integer; the height in pixels.
    #' @param id A character string; the id assigned to the device.
    #' @param launch.browser A function to launch a viewer; two options are
    #' `rstudioapi::viewer` and `utils::browseURL`. It defaults to the first
    #' option if the user is using RStudio and to the second option otherwise.
    #' The default applies to interactive session only.
    #' @param ... Additional arguments. Use `virtual = TRUE` to use the virtual
    #' device, `shiny = TRUE` for shiny application; everything else will be
    #' passed to the SVG element that hosts the visualisation.
    #' @examples
    #' \donttest{
    #' library(animate)
    #' device <- animate$new(400, 400)  # Launch a WebSocket server
    #' attach(device)
    #' x <- 1:10
    #' y <- 1:10
    #' id <- new_id(x)   # Give each point an ID: c("ID-1", "ID-2", ..., "ID-10")
    #' plot(x, y, id = id)
    #'
    #' new_y <- 10:1
    #' plot(x, new_y, id = id, transition = TRUE)  # Use transition
    #' off()
    #' detach(device)
    #' }
    initialize = function(width, height, id = "SVG_1", launch.browser, ...) {
      if (private$is_virtual(...)) {
        self$virtual_meta <- list(virtual = TRUE, width = width, height = height)

        ct <- V8::v8()
        ct$source(system.file("dist/virtual_device.js", package = "animate"))
        ct$assign("p", V8::JS("virtual_device()"))

        self$virtual_session <- list(
          send = function(x) {
            ct$assign("data", x)
            ct$assign("result", V8::JS("p.send(data)"))
          },
          get = function() {
            ct$assign("result", V8::JS("p.get()"))
            ct$get(V8::JS("JSON.stringify(result)"))
          }
        )

        args <- list(...)
        args$virtual <- NULL
        js_args <- append(list(width = width, height = height, id = id), args)
        return(do.call(self$svg, js_args))
      }

      if (private$is_shiny(...)) {
        args <- list(...)
        self$shiny <- TRUE
        self$session <- args$session

        args$session <- NULL
        if (is.null(args$root)) {
          args$root <- "#animateOutput"
        }
        js_args <- append(list(width = width, height = height, id = id), args)
        return(do.call(self$svg, js_args))
      }

      args <- append(list(width = width, height = height, id = id), list(...))
      in_handler <- function(x) {
        msg <- jsonlite::fromJSON(x)
        if (msg$type == "WebSocket.onopen") {
          self$ready_state <- 1
          message("Device is ready.")
          do.call(self$svg, args)
        }
        if (msg$type == "user_event") {
          private$dispatch_event(msg$message)
        }
        if (msg$type == "export") {
          message("Exporting file to: ", msg$path)
          jsonlite::write_json(
            jsonlite::fromJSON(msg$data, simplifyVector = FALSE),
            msg$path,
            auto_unbox = TRUE,
            null = "null"
          )
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
      if (!missing(launch.browser)) {
        viewer <- launch.browser
      } else {
        if (interactive()) {
          viewer <- ifelse(rstudioapi::isAvailable(),
                           rstudioapi::viewer,
                           utils::browseURL)
        } else {
          viewer <- function(x) NULL
        }
      }
      viewer(temp)
    },

    #' @description
    #' Switch off the device; this function closes the WebSocket connection.
    off = function() {
      if (self$shiny) return(NULL)
      if (self$virtual_meta$virtual) return(NULL)

      self$ready_state <- 0
      self$connection$stopServer()
    },

    #' @description
    #' Send commands to device
    #' @param message The message to send to the device.
    send = function(message) {
      if (self$shiny) return(private$shiny_send(message))
      if (self$virtual_meta$virtual) return(private$virtual_send(message))

      if (self$ready_state == 0) {
        message("Device is not yet available.")
      } else {
        self$connection$ws$send(
          jsonlite::toJSON(message, auto_unbox = TRUE, null = "null")
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
    #' @param ... Additional parameters. Some commonly used parameters are `id`
    #' and `root`. `id` assigns an id to the SVG element for future reference;
    #' `root` specifies the DOM element to insert the SVG element into.
    # @param root Character; a selector path to the container of the canvas.
    # @param id Character; the id of the SVG element
    svg = function(width = 800, height = 600, ...) {
      self$send(Message("fn_init_svg", list(width = width, height = height, ...)))
    },

    #' @description
    #' Add bars to a plot
    #' @param x The x coordinates of the bars.
    #' @param y The y coordinates of the bars.
    #' @param w The width of the bars.
    #' @param h The height of the bars.
    #' @param ... Additional graphical parameters.
    bars = function(x, y, w, h, ...) {
      self$send(Message("fn_bars", list(x = x, y = y, w = w, h = h, ...)))
    },

    #' @description
    #' Add HTML objects to a plot
    #' @param x The x coordinates of the objects.
    #' @param y The y coordinates of the objects.
    #' @param w The width of the objects.
    #' @param h The height of the objects.
    #' @param content The content of the objects; the HTML string.
    #' @param ... Additional graphical parameters.
    objects = function(x, y, w, h, content, ...) {
      self$send(Message("fn_objects", list(x = x, y = y, w = w, h = h,
                                           content = content, ...)))
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
    #' @details
    #' Options for the "pch" parameter: "circle", "plus", "diamond", "square",
    #' "star", "triangle", "wye", "triangle_down", "triangle_left", "triangle_right",
    #' "diamond_alt", "diamond_square", "pentagon", "hexagon", "hexagon_alt",
    #' "octagon", "octagon_alt", "cross".
    #'
    #' The unit of the "cex" parameter is squared pixels, corresponding to how
    #' much pixel space the symbol would cover. The convention comes from the
    #' 'D3' library, and the choice is (believed) to make plots visually consistent
    #' across the different symbols.
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
    #' Add straight lines to a plot
    #' @param a The intercept.
    #' @param b The slope.
    #' @param h The y-value(s) for horizontal line(s).
    #' @param v The x-value(s) for vertical line(s).
    #' @param ... Additional graphical parameters.
    abline = function(a, b, h, v, ...) {
      if (xor(missing(a), missing(b))) {
        stop("Error in abline: invalid a=, b= specification. a and b must be present/absent together.")
      }
      # General lines specified with intercept and slope
      if (!missing(a) && !missing(b)) {
        args <- list(x = c(0, 1), y = c(a, a + b), xlim = c(0, 1), ...)
        self$send(Message("fn_lines", args))
      }
      # Horizontal lines
      if (!missing(h)) {
        for (y_intercept in h) {
          args <- list(x = c(0, 1), y = rep(y_intercept, 2), xlim = c(0, 1), ...)
          self$send(Message("fn_lines", args))
        }
      }
      # Vertical lines
      if (!missing(v)) {
        for (x_intercept in v) {
          args <- list(x = rep(x_intercept, 2), y = c(0, 1), ylim = c(0, 1), ...)
          self$send(Message("fn_lines", args))
        }
      }
    },

    #' @description
    #' Add an axis to a plot
    #' @param x The x coordinates of the text.
    #' @param y The y coordinates of the text.
    #' @param labels The text.
    #' @param ... Additional graphical parameters.
    axis = function(x, ...) {
      self$send(Message("fn_axis", list(data = x, ...)))
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
    #' Attach an interactive event to an element
    #' @param selector A character string; a CSS selector.
    #' @param event_type A character string; the event type. For example, "click", "mouseover",
    #' "mouseout". See more options at \url{https://www.w3schools.com/jsref/dom_obj_event.asp}.
    #' @param callback A function, to be called when the event is triggered.
    #' The function should take an argument to receive the data from the
    #' browser end.
    event = function(selector, event_type, callback) {
      event_name <- paste0(selector, ":", event_type)
      self$event_handlers[[event_name]] <- callback
      self$send(Message("fn_event", list(selector = selector,
                                         event = event_type,
                                         event_name = event_name,
                                         shiny = self$shiny)))
    },

    #' @description
    #' Chain a transition after another.
    #' @param callback A function, to be called when the event is triggered.
    #' The function should take an argument to receive the data from the
    #' browser end.
    #' @examples
    #' \donttest{
    #' library(animate)
    #' device <- animate$new(600, 600)  # Launch a WebSocket server
    #' attach(device)
    #' par(xlim = c(0, 10), ylim = c(0, 10))
    #' plot(1:10, 1:10, id = 1:10)
    #' points(1:10, sample(10, 10), id = 1:10,
    #'   transition = list(
    #'     duration = 1000,
    #'     on = chain(function(message) {
    #'       print(message)
    #'       points(1:10, sample(10, 10), id = 1:10, bg = "green",
    #'              transition = list(duration = 2000))
    #'       })
    #'   ))
    #' par(xlim = NULL, ylim = NULL)  # Reset `xlim` and `ylim` in `par`
    #' off()
    #' detach(device)
    #' }
    chain = function(callback) {
      event_name <- paste0("chained-transition-", private$event_counter)
      private$event_counter <- private$event_counter + 1

      self$event_handlers[[event_name]] <- callback
      param <- list(event = "end",
                    event_name = event_name,
                    shiny = self$shiny)
      param
    },

    #' @description
    #' Attach a captured event to an element
    #' @param selector A character string; a CSS selector.
    #' @param event_type A character string; the event type. For example, "click", "mouseover",
    #' "mouseout". See more options at \url{https://www.w3schools.com/jsref/dom_obj_event.asp}.
    #' @param method A character string; the name of a device function (e.g. "points").
    #' @param param A named list of arguments to be called with.
    #' @note This function differs from the `event` function in that events
    #' registered through `simple_event` do not require R at deployment to work.
    simple_event = function(selector, event_type, method, param) {
      self$send(Message("fn_simple_event", list(selector = selector,
                                                event = event_type,
                                                method = method,
                                                param = param)))
    },

    #' @description
    #' Set the active device to a SVG element
    #' @param device_id A character vector; ID of the device.
    set = function(device_id) {
      self$send(Message("fn_set", list(device_id = device_id)))
    },

    #' @description
    #' Set the graphical parameters
    #' @param ... The graphical parameters
    par = function(...) {
      args <- list(...)
      if (is.null(names(args)) || any(names(args) == "")) {
        stop("All graphical parameters must be named.")
      }
      self$send(Message("fn_par", args))
    },

    #' @description
    #' Remove elements from the active SVG element
    #' @param id A character vector; the ID of the elements.
    #' @param selector A character vector; a CSS selector.
    remove = function(id = NULL, selector = "*") {
      self$send(Message("fn_remove", list(selector = selector, id = id)))
    },

    #' @description
    #' Remove all elements from the active SVG element
    clear = function() {
      self$send(Message("fn_clear", list()))
    },


    #' @description
    #' Remove a SVG element
    #' @param id A character string; the ID of the SVG. If not provided, remove
    #' the active SVG element.
    delete = function(id = NULL) {
      self$send(Message("fn_delete", list(id = id)))
    },

    #' #' @description
    #' #' Perform a group of graphical operations to a plot
    #' #' @param ... Any number of graphical operations.
    #' group = function(...) {
    #'   self$send(Message("fn_group", c(...)))
    #' },

    #' @description
    #' Import an animated plot
    #' @param setting A JSON file exported from previous runs.
    import = function(setting) {
      self$send(Message("fn_import", list(setting = jsonlite::fromJSON(setting))))
    },

    #' @description
    #' Export an animated plot
    #' @param path A character string; the file path to export to.
    #' @param handler 'r' or 'browser'; the program to handle the export operation.
    export = function(path = "./animate.json", handler = "browser") {
      self$send(Message("fn_export", list(path = path, filename = basename(path),
                                          handler = handler, skip_log = TRUE)))
    },

    #' @description
    #' Record an animated plot as a MP4 video
    #'
    #' @details
    #' This function will prompt you to select a screen / window / tab to
    #' record. Once started, the recording can be stopped by using the stop button
    #' at the notification box, or clicking anywhere on the page near the device.
    #' Always confirm that the screen recording notification box is gone.
    #' The captured video will be downloaded right after the recording stops.
    #'
    #' This uses web browsers' Media Streams API to record the screen
    #' and return the captured frames as a video. The entire process runs locally.
    #' The source file that provides this functionality can be found at
    #' `system.file("addons/screen_record.js", package = "animate")`.
    #'
    #' This function is disabled for 'Shiny' app and R Markdown document.
    #'
    #' This function does not work in the RStudio viewer. Please use the
    #' "show in new window" button to launch the page with a web browser.
    #'
    #' See browser compatibility at: \url{https://developer.mozilla.org/en-US/docs/Web/API/MediaStream_Recording_API#browser_compatibility}
    #'
    #' See Media Streams API reference at: \url{https://developer.mozilla.org/en-US/docs/Web/API/Media_Streams_API}
    record = function() {
      ans <- readline("Confirm that you want to record your screen [y/n]:")
      if (tolower(trimws(ans)) == "y") {
        self$send(Message("fn_export_video", list()))
      }
    },

    #' @description Event handler
    #' @param input The input object in the `server` function of a 'Shiny' app.
    observeAnimateEvent = function(input) {
      private$dispatch_event(input$message)
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
    },
    virtual_send = function(message) {
      self$virtual_session$send(message)
    },
    is_virtual = function(...) {
      isTRUE(list(...)$virtual)
    },
    dispatch_event = function(message) {
      for (event_name in names(self$event_handlers)) {
        if (event_name == message$param$event_name) {
          event_fun <- self$event_handlers[[event_name]]
          return(event_fun(message))
        }
      }
      warning("No handler can handle the message:\n", message)
    },
    event_counter = 0
  )
)
