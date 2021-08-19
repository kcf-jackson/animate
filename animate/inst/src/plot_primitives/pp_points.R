#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))
#! load_script("assets/ramda.min.js")
#! load_script("assets/broadcast.js")
#! load_script("utils.R")
#! load_script("pp_scales.R")

#' Add points to a plot
points = function(param, device) {
  param %<>% set_default(
    list(id = generate_id("point", length_of_data(param$x, param$y)),
         shape = "circle",
         size = device$par$size || 30,
         fill = "black",
         stroke = "black",
         `stroke-width` = 0)
  )
  keys <- c("x", "y", "id", "shape", "size", "fill", "stroke", "stroke-width")
  data0 <- as_data(param, keys)
  points_update <- function(selection, scale) {
    # assume that data have been binded
    selection$
      attr("transform", d %=>% translate(scale$x(d$x), scale$y(d$y)))$
      attr("id", d %=>% d$id)$
      attr("d", d %=>% d3_symbol(d$size, d$shape))$
      style("fill", d %=>% d$fill)$
      style("stroke-width", d %=>% d["stroke-width"])$
      style("stroke", d %=>% d$stroke)
  }

  d3_enter_update_exit(param, device, data0,
                       tag = "path", className = "points",
                       points_update)
}

# Notes
# (x, y) are necessary for users to provide
# (id, shape, size, fill, stroke, stroke-width) are optional for users to provide
# (attr, style, transition) are optional for users to provide
#
# (x, y, id, shape, size, fill, stroke, stroke-width) are necessary for update
# (attr, style, transition) are optional for update
