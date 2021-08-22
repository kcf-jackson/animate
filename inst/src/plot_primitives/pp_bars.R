#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add bars to a plot
bars = function(param, device) {
  param %<>% set_default(
    list(id = generate_id("rect", length_of_data(param$x, param$y, param$w, param$h)),
         fill = "black", stroke = "black", "stroke-width" = 0, "stroke-dasharray" = "none")
  )
  keys <- c("x", "y", "w", "h", "id", "fill", "stroke", "stroke-width", "stroke-dasharray")
  data0 <- as_data(param, keys)

  param$xlim <- param$xlim || device$xlim || d3_extent(param$x$concat(add(param$x, param$w)))
  param$ylim <- param$ylim || device$ylim || d3_extent(param$y$concat(add(param$y, param$h)))

  barplot_update <- function(selection, scale) {
    selection$
      attr("id", d %=>% d$id)$
      attr("x", d %=>% scale$x(d$x))$
      attr("y", d %=>% scale$y(d$y + d$h))$  # top-left corner of the 'rect'
      attr("width", d %=>% scale$x(d$w) - scale$x(0))$   # see note 1 below
      attr("height", d %=>% scale$y(0) - scale$y(d$h))$  # see note 2 below.
      style("fill", d %=>% d$fill)$
      style("stroke-dasharray", d %=>% d["stroke-dasharray"])$
      style("stroke-width", d %=>% d["stroke-width"])$
      style("stroke", d %=>% d$stroke)
  }
  d3_enter_update_exit(param, device, data0,
                       tag = "rect", className = "bars",
                       barplot_update)
}

# Note 1: Suppose we map [0,1,2] to [50,100,150], note that 1 is map to 100, 
# but the length of 1 should map to 50. This is done by mapping [0, 1] to 
# [50, 100], then take the difference.
#
# Note 2: The direction of the subtraction is reversed because the y-scale is
# inverted, so 0 maps to the greater number.
