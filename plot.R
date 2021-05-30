#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

#! load_library("dom")
#! load_library("d3")
#! load_library("websocket")

#! load_script("message.R")
#! load_script("import.R")

#! load_library("ramda")
#! load_script("d3_helpers.R")
#! load_script("assets/broadcast.js")
#! load_script("assets/d3-symbol-extra.min.js")

build_arg_list <- broadcast(function(x, y, id, size, shape, fill, stroke, stroke_width) {
  list(x = x, y = y, id = id, size = size, shape = shape, fill = fill,
       stroke = stroke, `stroke-width` = stroke_width)
})


plot2 <- R6Class(
  "plot2",
  list(
    # Device
    plot_stack = Array(),
    device = list(width = NULL, height = NULL),

    # ID utility
    id_count = 0,
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

    # Rendering functions
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
    }

  ),
  list()
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


# # Main -------------------------------------------------------------------------
JS_device <- plot2$new()
# JS_device$add_svg(list(cw=600, ch=400))
# JS_device$points(list(x=seq(1,10), y=seq(1,10), shape="diamond_square"))
