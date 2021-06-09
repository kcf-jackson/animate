#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

#! load_library("dom")
#! load_library("io")
#! load_script("assets/d3.v5.min.js")

#! load_script("import.R")

#! load_script("assets/ramda.min.js")
#! load_script("d3_helpers.R")
#! load_script("assets/broadcast.js")
#! load_script("assets/d3-symbol-extra.min.js")

plot2 <- R6Class(
  "plot2",
  # Public fields and methods ==================================================
  list(
    # Fields -------------------------------------------------------------------
    #' @field plot_stack A stack to store plotting commands.
    plot_stack = Array(),

    #' @field stack_size The maximum stack size.
    stack_size = 0,

    #' @field active device
    device = list(selection = NULL, width = NULL, height = NULL),

    #' @field A variable for generating IDs.
    id_count = 0,


    # Methods-------------------------------------------------------------------
    #' Constructor
    initialize = function(stack_size = 0) {
      self$stack_size <- stack_size
      self
    },

    #' ID generator
    generate_id = function(prefix, n = 1) {
      res <- Array()
      for (i in seq(1, n)) {
        self$id_count <- self$id_count + 1
        id <- ifelse(prefix, prefix %+% "_" %+% self$id_count, self$id_count)
        res$push(id)
      }
      # Return scalar if the Array is of length 1
      if (res$length == 1) {
        return(res[0])
      }
      res
    },

    #' Add a SVG element
    add_svg = function(param) {
      width  <- param$width
      height <- param$height
      root   <- param$root || "body"
      id     <- param$id || self$generate_id("svg")
      attributes <- param$attr
      styles     <- param$style

      ARG <- R::`__`
      svg0 <- d3::select(Id(id))
      if (svg0$empty()) {
        svg0 <- d3::select(root)$
          append("svg")$
          attr("id", id)$
          attr("width", width)$
          attr("height", height) %>%
          cond(d3_attr(ARG, attributes), attributes) %>%
          cond(d3_style(ARG, styles), styles)

        self$device$selection <- svg0
        self$device$width <- width
        self$device$height <- height
      } else {
        stop("Element with ID '" %+% id %+% "' already exists. Please use another ID or remove the existing element.")
      }
      svg0
    },


    # Rendering functions ------------------------------------------------------
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
      id    <- param$id || self$generate_id("point", x$length)

      shape <- param$shape || "circle"
      size  <- param$size || 30
      fill  <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0

      attributes <- param$attr
      styles     <- param$style
      transition <- param$transition

      xlim <- param$xlim || range(x)
      ylim <- param$ylim || range(y)

      # Set auxiliary variables ----
      ARG <- R::`__`
      data0 <- build_arg_list(x, y, id, size, shape, fill, stroke, stroke_width)

      cw <- self$device$width
      ch <- self$device$height
      ext <- Array(0.05, 0.95)

      # Scale ----
      x_scale <- d3_scaleLinear(domain = xlim, range = ext$map(~.x * cw))
      y_scale <- d3_scaleLinear(domain = ylim, range = ext$map(~.x * ch))

      # Main update ----
      selection <- self$device$selection$
        selectAll("path")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("transform", d %=>% translate(x_scale(d$x), y_scale(d$y)))$
          attr("id", d %=>% d$id)$
          attr("d", d %=>% d3_symbol(d$size, d$shape))$
          style("fill", d %=>% d$fill)$
          style("stroke-width", d %=>% d["stroke-width"])$
          style("stroke", d %=>% d$stroke) %>%
          cond(d3_attr(ARG, attributes), attributes) %>%
          cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("path") %>%
        d3_update()

      # Update ----
      selection %>%
        cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
    },

    #' Add an image to a plot
    image = function(param) {
      selection <- self$device$selection$
        append("image") %>%
        d3_attr(param)
      selection
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
      x     <- param$x
      y     <- param$y
      id    <- param$id || self$generate_id("point", x$length)

      shape <- param$shape || "circle"
      size  <- param$size || 30
      fill  <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0

      attributes <- param$attr
      styles     <- param$style
      transition <- param$transition

      xlim <- param$xlim || range(x)
      ylim <- param$ylim || range(y)

      x  <- param$x
      y  <- param$y
      id <- param$id || self$generate_id("lines")

      fill  <- param$fill || "none"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 1
      stroke_dasharray <- param["stroke-dasharray"]
      stroke_linejoin <- param["stroke-linejoin"] || "miter"
      stroke_linecap <- param["stroke-linecap"] || "butt"
      stroke_miterlimit <- param["miterlimit"] || 4

      attributes <- param$attr
      styles <- param$style
      transition <- param$transition

      xlim <- param$xlim || range(x)
      ylim <- param$ylim || range(y)

      # Set auxiliary variables ----
      ARG <- R::`__`
      data0 <- build_arg_list(id, fill, stroke, stroke_width,
                              stroke_dasharray, stroke_linejoin,
                              stroke_linecap, stroke_miterlimit)
      cw <- self$device$width
      ch <- self$device$height
      ext <- Array(0.05, 0.95)

      # Scale ----
      x_scale <- d3_scaleLinear(domain = xlim, range = ext$map(~.x * cw))
      y_scale <- d3_scaleLinear(domain = ylim, range = ext$map(~.x * ch))
      line_data <- R::zip(x$map(d %=>% x_scale(d)),
                          y$map(d %=>% y_scale(d)))

      # Main update ----
      selection <- self$device$selection$
        selectAll("path")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("d", d3::line()(line_data))$
          style("stroke-width", d %=>% d["stroke-width"])$
          style("stroke", d %=>% d$stroke)$
          style("fill", d %=>% d$fill) %>%
          cond(d3_attr(ARG, attributes), attributes) %>%
          cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("path") %>%
        d3_update()

      # Update ----
      selection %>%
        cond(d3_transition(ARG, transition), transition) %>%
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
      id    <- param$id || self$generate_id("point", x$length)

      shape <- param$shape || "circle"
      size  <- param$size || 30
      fill  <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0

      attributes <- param$attr
      styles     <- param$style
      transition <- param$transition

      xlim <- param$xlim || range(x)
      ylim <- param$ylim || range(y)

      x     <- param$x
      y     <- param$y
      text  <- param$text
      id    <- param$id || self$generate_id("lines", x$length)

      attributes <- param$attr
      styles <- param$style
      transition <- param$transition

      xlim <- param$xlim || range(x)
      ylim <- param$ylim || range(y)

      # Set auxiliary variables ----
      ARG <- R::`__`
      data0 <- build_arg_list(x, y, text, id)
      cw <- self$device$width
      ch <- self$device$height
      ext <- Array(0.05, 0.95)

      # Scale ----
      x_scale <- d3_scaleLinear(domain = xlim, range = ext$map(~.x * cw))
      y_scale <- d3_scaleLinear(domain = ylim, range = ext$map(~.x * ch))

      # Main update ----
      selection <- self$device$selection$
        selectAll("text")$
        filter(d %=>% id$includes(d$id))$
        data(data0)

      d3_update <- function(s) {
        s$attr("id", d %=>% d$id)$
          attr("x", d %=>% x_scale(d$x))$
          attr("y", d %=>% y_scale(d$y))$
          text(d %=>% d$text) %>%
          cond(d3_attr(ARG, attributes), attributes) %>%
          cond(d3_style(ARG, styles), styles)
      }

      # Enter ----
      selection$
        enter()$
        append("text") %>%
        d3_update()

      # Update ----
      selection %>%
        cond(d3_transition(ARG, transition), transition) %>%
        d3_update()

      # Exit ----
      selection$
        exit()$
        remove()

      return(TRUE)
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
    remove = function(selector) {
      d3::select(selector)$remove()
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
      dispatch(data)
    },

    length = function() {
      self$plot_stack$length
    },

    #' Import the plot setting
    #' @param setting A JSON object
    import = function(setting) {
      self$plot_stack <- setting$plot_stack
      self$stack_size <- setting$stack_size
      self
    },

    #' Export the plot setting
    export = function() {
      setting <- list(
        plot_stack = self$plot_stack,
        stack_size = self$stack_size
      ) %>%
        JSON::stringify() %>%
        end_string()
      write(setting, "animate.json")
    }
  ),
  # Private fields and methods =================================================
  list(
    dispatchers = Array(
      Decoder("fn_init_svg",
              x %=>% (x == "fn_init_svg"),
              message %=>% JS_device$add_svg(message)),
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
              message %=>% JS_device$remove(message$selector))
    )
  )
)


# Helper functions -------------------------------------------------------------
Decoder <- function(name, predicate, handler) {
  list(name = name, predicate = predicate, handler = handler)
}

Id <- function(x) { "#" %+% x }

parse_px <- function(x) {
  parseInt(x$replace("px", ""))
}

end_string <- function(x) { x %+% "\n"}

cond <- function(selection, f, pred) {
  if (pred) return(selection %>% f())
  return(selection)
}

pch <- function(x) {
  # stock
  if (x == "plus")     return(d3::symbolCross)
  if (x == "diamond")  return(d3::symbolDiamond)
  if (x == "square")   return(d3::symbolSquare)
  if (x == "star")     return(d3::symbolStar)
  if (x == "triangle") return(d3::symbolTriangle)
  if (x == "wye")      return(d3::symbolWye)
  # transform
  if (x == "triangle_down")  return(d3::symbolTriangleDown)
  if (x == "triangle_left")  return(d3::symbolTriangleLeft)
  if (x == "triangle_right") return(d3::symbolTriangleRight)
  if (x == "diamond_alt")    return(d3::symbolDiamondAlt)
  if (x == "diamond_square") return(d3::symbolDiamondSquare)
  if (x == "pentagon")    return(d3::symbolPentagon)
  if (x == "hexagon")     return(d3::symbolHexagon)
  if (x == "hexagon_alt") return(d3::symbolHexagonAlt)
  if (x == "octagon")     return(d3::symbolOctagon)
  if (x == "octagon_alt") return(d3::symbolOctagonAlt)
  if (x == "cross") return(d3::symbolX)
  # default
  d3::symbolCircle
}


# Main -------------------------------------------------------------------------
# JS_device <- plot2$new()
# JS_device$add_svg(list(cw=600, ch=400))
# JS_device$points(list(x=seq(1,10), y=seq(1,10), shape="diamond_square"))
