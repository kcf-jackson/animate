#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

#! load_library("dom")
#! load_library("io")
#! load_script("assets/d3.v5.min.js")

#! load_script("message.R")
#! load_script("import.R")

#! load_script("assets/ramda.min.js")
#! load_script("d3_helpers.R")
#! load_script("assets/broadcast.js")
#! load_script("assets/d3-symbol-extra.min.js")

plot2 <- R6Class(
  "plot2",
  list(
    # Device -------------------------------------------------------------------
    plot_stack = Array(),

    stack_size = 0,

    device = list(width = NULL, height = NULL),

    id_count = 0,

    player_pointer = -1,

    player_handle = NULL,

    dispatchers = Array(
      decoder("fn_init_svg",
              x %=>% (x == "fn_init_svg"),
              message %=>% JS_device$add_svg(message)),
      decoder("fn_points",
              x %=>% (x == "fn_points"),
              message %=>% JS_device$points(message))
    ),

    initialize = function(stack_size = 0) {
      self$stack_size <- stack_size
      self$player_pointer <- ifelse(stack_size > 0, 0, -1)
      self
    },

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

    # Rendering functions ------------------------------------------------------
    add_svg = function(param) {
      cw   <- param$cw
      ch   <- param$ch
      root <- param$root || "body"
      id   <- param$id || self$generate_id("svg")

      svg0 <- select_dom("#" %+% id)
      if (!svg0) {
        svg0 <- svg("svg", list(width = cw, height = ch, id = id))
        select_dom(root)$appendChild(svg0)

        self$device$width <- cw
        self$device$height <- ch
      } else {
        console::log("Element with ID '" %+% id %+% "' already exists. Please use another ID or remove the existing element.")
      }
      svg0
    },

    points = function(param) {
      x     <- param$x
      y     <- param$y
      id    <- param$id || self$generate_id("point", x$length)

      shape <- param$shape || "circle"
      size  <- param$size || 30
      fill  <- param$fill || "black"
      stroke <- param$stroke || "black"
      stroke_width <- param["stroke-width"] || 0

      attributes <- param$attr
      styles <- param$style
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
      selection <- d3::select("svg")$
        selectAll("path")$
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

    record = function(data) {
      if (self$stack_size == 0) {
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
    },

    # Control functions --------------------------------------------------------
    dispatch = function(data) {
      for (dispatch_fn in self$dispatchers) {
        if (dispatch_fn$predicate(data$type)) {
          dispatch_fn$handler(data$message)
        }
      }
      TRUE
    },

    play = function() {
      if (self$player_pointer >= 0) {
        self$dispatch(plot_stack[self$player_pointer])
        self$player_pointer <- (self$player_pointer + 1) %% self$plot_stack$length
      }
      TRUE
    },

    loop = function() {
      if (self$player_pointer >= 0) {
        self$player_handle <- setInterval(function() {
          self$dispatch(plot_stack[self$player_pointer])
          self$player_pointer <- (self$player_pointer + 1) %% self$plot_stack$length
        }, 300)
        self$player_handle
      }
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
      setting <- JSON::stringify(list(
        plot_stack = self$plot_stack,
        stack_size = self$stack_size
      ))
      write(setting, "animate.json")
    }
  ),
  list()
)

build_arg_list <- broadcast(
  function(x, y, id, size, shape, fill, stroke, stroke_width) {
    list(x = x, y = y, id = id,
         size = size, shape = shape, fill = fill,
         stroke = stroke, `stroke-width` = stroke_width)
  }
)

cond <- function(selection, f, pred) {
  if (pred) return(selection %>% f)
  return(selection)
}

pch <- function(x) {
  # stock
  if (x == "plus") return(d3::symbolCross)
  if (x == "diamond") return(d3::symbolDiamond)
  if (x == "square") return(d3::symbolSquare)
  if (x == "star") return(d3::symbolStar)
  if (x == "triangle") return(d3::symbolTriangle)
  if (x == "wye") return(d3::symbolWye)
  # transform
  if (x == "triangle_down") return(d3::symbolTriangleDown)
  if (x == "triangle_left") return(d3::symbolTriangleLeft)
  if (x == "triangle_right") return(d3::symbolTriangleRight)
  if (x == "diamond_alt") return(d3::symbolDiamondAlt)
  if (x == "diamond_square") return(d3::symbolDiamondSquare)
  if (x == "pentagon") return(d3::symbolPentagon)
  if (x == "hexagon") return(d3::symbolHexagon)
  if (x == "hexagon_alt") return(d3::symbolHexagonAlt)
  if (x == "octagon") return(d3::symbolOctagon)
  if (x == "octagon_alt") return(d3::symbolOctagonAlt)
  if (x == "cross") return(d3::symbolX)
  # default
  d3::symbolCircle
}

decoder <- function(name, predicate, handler) {
  list(name = name, predicate = predicate, handler = handler)
}


# Main -------------------------------------------------------------------------
# JS_device <- plot2$new()
# JS_device$add_svg(list(cw=600, ch=400))
# JS_device$points(list(x=seq(1,10), y=seq(1,10), shape="diamond_square"))
