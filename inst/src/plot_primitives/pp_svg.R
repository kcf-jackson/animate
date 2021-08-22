#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add a SVG element to a new device
svg_device <- function(param) {
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
      return(Device(svg0, width, height, id))
  }

  console::warn("Element with ID '" %+% id %+% "' already exists. The element will not be created again.")
  FALSE
}
