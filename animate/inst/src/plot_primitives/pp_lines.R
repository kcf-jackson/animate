#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))
#' Add lines to a plot
lines = function(param, device) {
  param %<>% set_default(
    list(id = generate_id("lines"),
         fill = "none",
         stroke = "black",
         "stroke-width" = 1,
         "stroke-dasharray" = "none",
         "stroke-linejoin" = "miter",
         "stroke-linecap" = "butt",
         "stroke-miterlimit" = 4)
  )
  keys <- c("id", "fill", "stroke", "stroke-width", "stroke-dasharray",
            "stroke-linejoin", "stroke-linecap", "stroke-miterlimit")
  data0 <- as_data(param, keys)
  lines_update <- function(x, y) {
    function(selection, scale) {
      line_data <- R::zip(x$map(d %=>% scale$x(d)),
                          y$map(d %=>% scale$y(d)))
      selection$
        attr("id", d %=>% d$id)$
        attr("d", d3::line()(line_data))$
        style("stroke-width", d %=>% d["stroke-width"])$
        style("stroke", d %=>% d$stroke)$
        style("fill", d %=>% d$fill)
    }
  }

  d3_enter_update_exit(param, device, data0,
                       tag = "path", className = "lines",
                       lines_update(param$x, param$y))
}
