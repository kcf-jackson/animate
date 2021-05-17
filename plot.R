#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "d3", "auto"))

#! load_library("dom")
#! load_library("d3")
#! load_library("websocket")

#! load_script("utils.R")
#! load_script("message.R")
#! load_script("import.R")

#! load_library("ramda")
#! load_script("broadcast.js")

build_arg_list <- broadcast(x %=>% y %=>% id %=>% size %=>% shape %=>%
  stroke %=>% stroke_width %=>% stroke_opacity %=>% stroke_dasharray %=>%
    stroke_linecap %=>% stroke_linejoin %=>% fill %=>% fill_opacity %=>% {
  return(list(x = x, y = y, id = id,
       size = size, shape = shape,
       stroke = stroke,
       stroke_width = stroke_width,
       stroke_opacity = stroke_opacity,
       stroke_dasharray = stroke_dasharray,
       stroke_linecap = stroke_linecap,
       stroke_linejoin = stroke_linejoin,
       fill = fill,
       fill_opacity = fill_opacity))
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
      if (n == 1) {
        return(res[0])
      }
      res
    },

    # Rendering functions
    add_svg = function(cw, ch, root = "body", id = "svg") {
      svg0 <- select_dom("#" %+% id)
      if (!svg0) {
        id <- id || self$generate_id("svg")
        svg0 <- svg("svg", list(width = cw, height = ch, id = id))
        select_dom(root)$appendChild(svg0)

        self$device$width <- cw
        self$device$height <- ch
      }
      svg0
    },

    points = function(x, y, id,
                      size = 3, shape = "circle",
                      stroke = "black", stroke_width = 1, stroke_opacity = 1,
                      stroke_dasharray = "none", stroke_linecap = "butt",
                      stroke_linejoin = "",
                      fill = "black",
                      fill_opacity = 1,
                      xlim, ylim, transition = list()) {

      # Set default for input variables
      id <- id || seq(1, x$length)
      xlim <- xlim || range(x)
      ylim <- ylim || range(y)

      # Set auxiliary variables
      data0 <- build_arg_list(
        x = x, y = y,
        id = self$generate_id("point", x$length),
        size = size, shape = shape,
        stroke = stroke, stroke_width = stroke_width,
        stroke_opacity = stroke_opacity, stroke_dasharray = stroke_dasharray,
        stroke_linecap = stroke_linecap, stroke_linejoin = stroke_linejoin,
        fill = fill, fill_opacity = fill_opacity
      )

      cw <- self$device$width
      ch <- self$device$height
      ext <- Array(0.05, 0.95)

      # Scale
      x_scale <- d3::scaleLinear()$
        domain(xlim)$
        range(ext$map(~.x * cw))
      y_scale <- d3::scaleLinear()$
        domain(ylim)$
        range(ext$map(~.x * ch))

      # Main update
      selection <- d3::select("svg")$
        selectAll("circle")$
        data(data0)

      selection$
        enter()$
        append("circle")$
        d3_attr(
          cx = function(d) { x_scale(d$x) },
          cy = function(d) { y_scale(d$y) },
          r  = function(d) { d$size },
          id = function(d) { d$id }
        )
      # $
      #   d3_style(
      #
      #   )

      selection$
        # transition()$
        d3_attr(
          cx = function(d) { x_scale(d$x) },
          cy = function(d) { y_scale(d$y) },
          r = 3,
          id = function(d) { d$id }
        )

      selection$
        exit()$
        remove()

      TRUE
    }

  ),
  list()
)




# Main -------------------------------------------------------------------------
JS_device <- plot2$new()
JS_device$add_svg(600, 800)
JS_device$points(seq(1,10), seq(1,10))
