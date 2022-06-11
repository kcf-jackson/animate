#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add foreignObject to a plot
objects = function(param, device) {
  param %<>% set_default(
    list(id = generate_id("objects", length_of_data(param$x, param$y, param$w, param$h,
                                                    param$content)))
  )
  keys <- c("x", "y", "w", "h", "id", "content")
  data0 <- as_data(param, keys)

  param$xlim <- param$xlim || device$par$xlim || d3_extent(concat(param$x, add(param$x, param$w)))
  param$ylim <- param$ylim || device$par$ylim || d3_extent(concat(param$y, add(param$y, param$h)))

  foreignObject_update <- function(selection, scale) {
    selection$
      attr("id", d %=>% d$id)$
      attr("x", d %=>% scale$x(d$x))$
      attr("y", d %=>% scale$y(d$y + d$h))$  # top-left corner of the 'rect'
      attr("width", d %=>% scale$x(d$w) - scale$x(0))$   # see note 1 below
      attr("height", d %=>% scale$y(0) - scale$y(d$h))$  # see note 2 below.
      html(d %=>% d$content)
  }
  d3_enter_update_exit(param, device, data0,
                       tag = "foreignObject", className = "objects",
                       foreignObject_update)
}

# Note 1: Suppose we map [0,1,2] to [50,100,150], note that 1 is map to 100,
# but the length of 1 should map to 50. This is done by mapping [0, 1] to
# [50, 100], then take the difference.
#
# Note 2: The direction of the subtraction is reversed because the y-scale is
# inverted, so 0 maps to the greater number.
