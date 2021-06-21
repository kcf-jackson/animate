#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"), asset_tags = basic_tags())

#! load_library("dom")
#! load_library("io")
#! load_library('websocket')
#! load_script('message.R')
#! load_script("assets/d3.v5.min.js")
#! load_script("assets/ramda.min.js")
#! load_script("assets/broadcast.js")
#! load_script("assets/d3-symbol-extra.min.js")

#! load_script("d3_helpers.R")
#! load_script("utils.R")
#! load_script("plot_helpers.R")
#! load_script("svg_to_png.R")
#! load_script("controller.R")

plot2 <- R6Class(
  "plot2",
  # Public fields and methods ==================================================
  list(
    # Fields -------------------------------------------------------------------
    #' @field plot_stack A stack to store plotting commands.
    plot_stack = c(),

    #' @field stack_size The maximum stack size. Use -1 for unlimited.
    stack_size = 0,

    #' @field active device
    device = list(selection = NULL, width = NULL, height = NULL,
                  par = list(mai = times(0.1, c(0.82, 0.82, 0.82, 0.82)))),

    # Methods-------------------------------------------------------------------
    #' Constructor
    initialize = function(stack_size = 0) {
      self$stack_size <- stack_size
      self
    },

    #' Add a SVG element
    add_svg = function(param) {
      root   <- param$root || "body"
      id     <- param$id || generate_id("svg")
      width  <- param$width
      height <- param$height
      attr   <- param$attr
      style  <- param$style

      base_attr <- list(id = id, width = width, height = height)

      svg0 <- d3::select(Id(id))
      if (svg0$empty()) {
        svg0 <- d3::select(root)$append("svg") %>%
          d3_attr(base_attr) %>%
          d3_cond(d3_attr(ARG, attr), attr) %>%
          d3_cond(d3_style(ARG, style), style)

        self$device <- Device(svg0, width, height)
      } else {
        console::warn("Element with ID '" %+% id %+% "' already exists. The element will not be created again.")
      }
      svg0
    },

    # Rendering functions ------------------------------------------------------
    #' Generic X-Y plotting
    plot = function(param) {
      x    <- param$x
      y    <- param$y
      id   <- param$id || generate_id("datum", length_data(x, y))
      type <- param$type || "p"
      xlim <- param$xlim || d3::extent(x)
      ylim <- param$ylim || d3::extent(y)
      main <- param$main
      xlab <- param$xlab
      ylab <- param$ylab

      # Add points ----
      selection <- self$device$selection

      # Create drawing area if not already existed
      draw_area <- selection$
        selectAll("g" %+% Id("drawing-area"))

      if (draw_area$empty()) {
        draw_area <- selection$
          append("g")$
          attr("id", "drawing-area")
      }

      self$device$selection <- draw_area
      if (type == "p") { self$points(param) }
      if (type == "l") { self$lines(param) }

      self$device$selection <- selection

      # Axes ----
      # x-axis
      Object::assign(param, list(data = x, lim = xlim, axis = "axisBottom", id = "x-axis"))
      self$axis(param)

      # y-axis
      Object::assign(param, list(data = y, lim = ylim, axis = "axisLeft", id = "y-axis"))
      self$axis(param)

      # Labels ----
      cw <- self$device$width
      ch <- self$device$height
      if (main) {
        text_param <- list(x = 0.5,
                           y = 0.5 * private$top(),
                           text = main,
                           id = "main-title",
                           xlim = c(0, 1), xrange = c(0, cw),
                           ylim = c(0, 1), yrange = c(0, ch),
                           attr = list("text-anchor" = "middle"))
        self$text(text_param)
      }
      if (xlab) {
        xlab_param <- list(x = 0.5,
                           y = 1 - 0.0625 * private$bottom(),
                           text = xlab,
                           id = "x-label",
                           xlim = c(0, 1), xrange = c(0, cw),
                           ylim = c(0, 1), yrange = c(0, ch),
                           attr = list("text-anchor" = "middle"))
        self$text(xlab_param)
      }
      if (ylab) {
        ylab_param <- list(x = 0.5 * private$left(),
                           y = 0.5,
                           text = ylab,
                           id = "y-label",
                           xlim = c(0, 1), xrange = c(0, cw),
                           ylim = c(0, 1), yrange = c(0, ch),
                           transform = "rotate(-90)",
                           attr = list("text-anchor" = "middle"))
        self$text(ylab_param)
      }

      TRUE
    },

    #' Add an axis to a plot
    axis = function(param) {
      data  <- param$data
      scale <- param$scale || "scaleLinear"
      axis  <- param$axis || "axisBottom"
      scale_orient <- ifelse(axis == "axisBottom" || axis == "axisTop", "x", "y")
      id    <- param$id || generate_id(type %+% "-axis")
      lim   <- param$lim || d3::extent(data)
      transition <- param$transition

      # Scale ----
      scale_fun <- d3_scale(domain = lim, range = self$range(scale_orient), scale)
      axis_fun <- d3[axis]()$scale(scale_fun)

      # Axes ----
      selection <- self$device$selection

      bottom <- (1 - private$bottom()) * self$device$height
      left   <- private$left() * self$device$width
      top    <- private$top() * self$device$height
      right  <- (1 - private$right()) * self$device$width
      if (axis == "axisLeft")   transform <- "translate(" %+% left %+% ", 0)"
      if (axis == "axisRight")  transform <- "translate(" %+% right %+% ", 0)"
      if (axis == "axisTop")    transform <- "translate(0, " %+% top %+% ")"
      if (axis == "axisBottom") transform <- "translate(0, " %+% bottom %+% ")"

      selected_axis <- selection$
        selectAll("g" %+% Id(id))

      if (selected_axis$empty()) {
        selected_axis <- selection$
          append("g")$
          attr("id", id)$
          classed("axis", TRUE)
      }

      selected_axis$
        attr("transform", transform) %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_call(axis_fun)
    },

    #' Add bars to a plot
    bars = function(param) {
      build_arg_list <- broadcast(
        function(x, y, w, h, id, fill, stroke, stroke_width, stroke_dasharray) {
          list(x = x, y = y, w = w, h = h, id = id,
               fill = fill, stroke = stroke,
               "stroke-width" = stroke_width,
               "stroke-dasharray" = stroke_dasharray)
        }
      )

      # Parameters destructuring ----
      x <- param$x
      y <- param$y
      w <- param$w
      h <- param$h
      id     <- param$id || generate_id("rect", length_data(x, y, w, h))
      fill   <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0
      stroke_dasharray <- param["stroke-dasharray"] || "none"

      attr <- param$attr
      style <- param$style
      transition <- param$transition

      # Set auxiliary variables ----
      data0 <- build_arg_list(x, y, w, h, id, fill, stroke, stroke_width, stroke_dasharray)

      # Scale ----
      scale <- param$scale || "scaleLinear"
      xlim <- param$xlim || d3::extent(add(x, w))
      ylim <- param$ylim || d3::extent(add(y, h))
      x_scale <- d3_scale(domain = xlim, range = self$range("x"), scale)
      y_scale <- d3_scale(domain = ylim, range = self$range("y"), scale)
      cw <- self$device$width
      ch <- self$device$height

      # Main update ----
      selection <- self$device$selection$
        selectAll("rect.bars")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("x", d %=>% x_scale(d$x))$
          attr("y", d %=>% y_scale(d$y + d$h))$
          attr("width", d %=>% x_scale(d$w) - cw * private$left())$
          attr("height", d %=>% ch * (1 - private$bottom()) - y_scale(d$h))$
          style("fill", d %=>% d$fill)$
          style("stroke-dasharray", d %=>% d["stroke-dasharray"])$
          style("stroke-width", d %=>% d["stroke-width"])$
          style("stroke", d %=>% d$stroke) %>%
          d3_cond(d3_attr(ARG, attr), attr) %>%
          d3_cond(d3_style(ARG, style), style)
      }

      # Enter ----
      selection$
        enter()$
        append("rect")$
        classed("bars", TRUE) %>%
        d3_update()

      # Update ----
      selection %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Add points to a plot
    points = function(param) {
      build_arg_list <- broadcast(
        function(x, y, id, size, shape, fill, stroke, stroke_width) {
          list(x = x, y = y, id = id,
               size = size, shape = shape, fill = fill,
               stroke = stroke, `stroke-width` = stroke_width)
        }
      )

      # Parameters destructuring ----
      x     <- param$x
      y     <- param$y
      id    <- param$id || generate_id("point", length_data(x, y))

      shape <- param$shape || "circle"
      size  <- param$size || 30
      fill  <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0
      attributes <- param$attr
      styles     <- param$style
      transition <- param$transition

      # Set auxiliary variables ----
      data0 <- build_arg_list(x, y, id, size, shape, fill, stroke, stroke_width)

      # Scale ----
      scale <- handle_scale(param)
      xlim <- param$xlim || d3_extent(x)
      ylim <- param$ylim || d3_extent(y)
      x_scale <- d3_scale(domain = xlim, range = self$range("x"), scale$x)
      y_scale <- d3_scale(domain = ylim, range = self$range("y"), scale$y)

      # Main update ----
      d3_update <- function(s) {
        s$attr("transform", d %=>% translate(x_scale(d$x), y_scale(d$y)))$
          attr("id", d %=>% d$id)$
          attr("d", d %=>% d3_symbol(d$size, d$shape))$
          style("fill", d %=>% d$fill)$
          style("stroke-width", d %=>% d["stroke-width"])$
          style("stroke", d %=>% d$stroke) %>%
          d3_cond(d3_attr(ARG, attributes), attributes) %>%
          d3_cond(d3_style(ARG, styles), styles)
      }

      # Select ----
      selection <- self$device$selection$
        selectAll("path.points")$
        filter(has_id(id))$
        data(data0)

      # Enter ----
      selection$
        enter()$
        append("path")$
        classed("points", TRUE) %>%
        d3_update()

      # Update ----
      selection %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Add an image to a plot
    image = function(param) {
      build_arg_list <- broadcast(
        function(href, width, height, id, x, y) {
          list(width = width, height = height, x = x, y = y, href = href, id = id)
        }
      )

      # Parameters destructuring ----
      x      <- param$x || 0
      y      <- param$y || 1
      width  <- param$width
      height <- param$height
      href   <- param$href
      id     <- param$id || generate_id("image", length_data(x, y))

      attributes <- param$attr
      styles <- param$style
      transition <- param$transition
      transform <- param$transform || ""

      # Set auxiliary variables ----
      data0 <- build_arg_list(href, width, height, id, x, y)

      # Scale ----
      scale <- param$scale || "scaleLinear"
      xlim <- param$xlim || ifelse(isArray(x), d3::extent(x), c(0, 1))
      ylim <- param$ylim || ifelse(isArray(y), d3::extent(y), c(0, 1))
      xrange <- param$xrange || self$range("x")
      yrange <- param$yrange || self$range("y")
      x_scale <- d3_scale(domain = xlim, range = xrange, scale)
      y_scale <- d3_scale(domain = ylim, range = yrange, scale)

      # Main update ----
      selection <- self$device$selection$
        selectAll("image")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("transform", d %=>% translate(x_scale(d$x), y_scale(d$y)) %+% transform)$
          attr("href", d %=>% d$href)$
          attr("width", d %=>% d$width)$
          attr("height", d %=>% d$height) %>%
          d3_cond(d3_attr(ARG, attributes), attributes) %>%
          d3_cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("image") %>%
        d3_update()

      # Update ----
      selection %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Add lines to a plot
    lines = function(param) {
      build_arg_list <- broadcast(
        function(id, fill, stroke, stroke_width, stroke_dasharray,
                 stroke_linejoin, stroke_linecap, stroke_miterlimit) {
          list(id = id, fill = fill,
               stroke = stroke,
               "stroke-width" = stroke_width,
               "stroke-dasharray" = stroke_dasharray,
               "stroke-linejoin" = stroke_linejoin,
               "stroke-linecap" = stroke_linecap,
               "stroke-miterlimit" = stroke_miterlimit)
        }
      )

      # Parameters destructuring ----
      x  <- param$x
      y  <- param$y
      id <- param$id || generate_id("lines")

      fill  <- param$fill || "none"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 1
      stroke_dasharray <- param["stroke-dasharray"] || "none"
      stroke_linejoin <- param["stroke-linejoin"] || "miter"
      stroke_linecap <- param["stroke-linecap"] || "butt"
      stroke_miterlimit <- param["miterlimit"] || 4

      attributes <- param$attr
      styles <- param$style
      transition <- param$transition

      # Set auxiliary variables ----
      data0 <- build_arg_list(id, fill, stroke, stroke_width,
                              stroke_dasharray, stroke_linejoin,
                              stroke_linecap, stroke_miterlimit)

      # Scale ----
      scale <- param$scale || "scaleLinear"
      xlim <- param$xlim || d3::extent(x)
      ylim <- param$ylim || d3::extent(y)
      x_scale <- d3_scale(domain = xlim, range = self$range("x"), scale)
      y_scale <- d3_scale(domain = ylim, range = self$range("y"), scale)
      line_data <- R::zip(x$map(d %=>% x_scale(d)),
                          y$map(d %=>% y_scale(d)))

      # Main update ----
      selection <- self$device$selection$
        selectAll("path.lines")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("d", d3::line()(line_data))$
          style("stroke-width", d %=>% d["stroke-width"])$
          style("stroke", d %=>% d$stroke)$
          style("fill", d %=>% d$fill) %>%
          d3_cond(d3_attr(ARG, attributes), attributes) %>%
          d3_cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("path")$
        classed("lines", TRUE) %>%
        d3_update()

      # Update ----
      selection %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Add text to a plot
    text = function(param) {
      build_arg_list <- broadcast(
        function(x, y, text, id) {
          list(x = x, y = y, text = text, id = id)
        }
      )

      # Parameters destructuring ----
      x     <- param$x
      y     <- param$y
      text  <- param$text
      id    <- param$id || generate_id("text", length_data(x, y))

      attributes <- param$attr
      styles <- param$style
      transition <- param$transition
      transform <- param$transform || ""

      # Set auxiliary variables ----
      data0 <- build_arg_list(x, y, text, id)

      # Scale ----
      scale <- param$scale || "scaleLinear"
      xlim <- param$xlim || d3::extent(x)
      ylim <- param$ylim || d3::extent(y)
      xrange <- param$xrange || self$range("x")
      yrange <- param$yrange || self$range("y")
      x_scale <- d3_scale(domain = xlim, range = xrange, scale)
      y_scale <- d3_scale(domain = ylim, range = yrange, scale)

      # Main update ----
      selection <- self$device$selection$
        selectAll("text")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("transform", d %=>% translate(x_scale(d$x), y_scale(d$y)) %+% transform)$
          text(d %=>% d$text) %>%
          d3_cond(d3_attr(ARG, attributes), attributes) %>%
          d3_cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("text") %>%
        d3_update()

      # Update ----
      selection %>%
        d3_cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Find the range of the drawing area
    #' @param type Either 'x' or 'y'.
    range = function(type) {
      bottom <- private$bottom()
      left   <- private$left()
      top    <- private$top()
      right  <- private$right()
      if (type == "x") {
        res <- times(c(left, 1 - right), private$width())
      }
      if (type == "y") {
        res <- times(c(top, 1 - bottom), private$height())$reverse()
      }
      res
    },

    #' Set maximum stacksize
    set_max_stacksize = function(n) {
      self$stack_size <- n
      n
    },

    #' Set graphical parameters
    set_par = function(parameters) {
      Object::assign(self$device$par, parameters)
    },

    #' Set active device
    set = function(selector) {
      selection <- d3::select(selector)
      if (!selection$empty()) {
        self$device$selection <- selection
        self$device$width <- parse_px(selection$style("width"))
        self$device$height <- parse_px(selection$style("height"))
      }
      selection
    },

    #' Remove an element
    remove = function(selector = "*", id) {
      filter_by_id <- selection %=>%
        selection$filter(d %=>% d && d$id && id$includes(d$id))

      (d3::selectAll(selector) %>%
        d3_cond(filter_by_id, id))$
        remove()
    },

    #' Remove all elements of the active device
    clear = function() {
      self$device$selection$
        selectAll("*")$
        remove()
    },

    # Control functions --------------------------------------------------------
    record = function(data) {
      if ((self$stack_size == 0) || (data$type == "fn_export")) {
        return(self$plot_stack)
      }

      if ((self$stack_size == -1) || (self$plot_stack$length < self$stack_size)) {
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
             self$stack_size %+% " (note that -1 corresponds to 'unlimited').")

      TRUE
    },

    dispatch = function(data) {
      for (dispatch_fn in private$dispatchers) {
        if (dispatch_fn$predicate(data$type)) {
          dispatch_fn$handler(data$message)
        }
      }
      TRUE
    },

    dispatch_by_idx = function(idx) {
      data <- self$plot_stack[idx]
      self$dispatch(data)
    },

    length = function() {
      self$plot_stack$length
    },

    #' Import the plot setting
    #' @param setting A JSON object
    import = function(setting) {
      self$plot_stack <- setting$plot_stack
      self$stack_size <- setting$stack_size
      self$device <- setting$device
      self
    },

    #' Export the plot setting
    export = function() {
      # Replace reference object by its ID
      temp <- self$device$selection
      self$device$selection <- temp$attr("id")
      # Export
      setting <- list(
        plot_stack = self$plot_stack,
        stack_size = self$stack_size,
        device = self$device
      ) %>%
        JSON::stringify() %>%
        end_string()
      write(setting, "animate.json")
      # Restore reference object
      self$device$selection <- temp
      TRUE
    }
  ),
  # Private fields and methods =================================================
  list(
    dispatchers = c(
      Decoder("fn_init_svg",
              x %=>% (x == "fn_init_svg"),
              message %=>% JS_device$add_svg(message)),
      Decoder("fn_bars",
              x %=>% (x == "fn_bars"),
              message %=>% JS_device$bars(message)),
      Decoder("fn_points",
              x %=>% (x == "fn_points"),
              message %=>% JS_device$points(message)),
      Decoder("fn_lines",
              x %=>% (x == "fn_lines"),
              message %=>% JS_device$lines(message)),
      Decoder("fn_image",
              x %=>% (x == "fn_image"),
              message %=>% JS_device$image(message)),
      Decoder("fn_text",
              x %=>% (x == "fn_text"),
              message %=>% JS_device$text(message)),
      Decoder("fn_export",
              x %=>% (x == "fn_export"),
              message %=>% JS_device$export()),
      Decoder("fn_set",
              x %=>% (x == "fn_set"),
              message %=>% JS_device$set(message$selector)),
      Decoder("fn_remove",
              x %=>% (x == "fn_remove"),
              message %=>% JS_device$remove(message$selector, message$id)),
      Decoder("fn_clear",
              x %=>% (x == "fn_clear"),
              message %=>% JS_device$clear()),
      Decoder("fn_plot",
              x %=>% (x == "fn_plot"),
              message %=>% JS_device$plot(message)),
      Decoder("fn_par",
              x %=>% (x == "fn_par"),
              message %=>% JS_device$set_par(message)),
      Decoder("fn_max_stacksize",
              x %=>% (x == "fn_max_stacksize"),
              message %=>% JS_device$set_max_stacksize(message$n))
    ),
    bottom = function() { self$device$par$mai[0] },
    left   = function() { self$device$par$mai[1] },
    top    = function() { self$device$par$mai[2] },
    right  = function() { self$device$par$mai[3] },
    width  = function() { self$device$width },
    height = function() { self$device$height }
  )
)
JS_device <- plot2$new(-1)
