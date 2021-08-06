#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

# Interface --------------------------------------------------------------------
Decoder <- function(name, predicate, handler) {
  list(name = name, predicate = predicate, handler = handler)
}

Device <- function(selection, width, height, id,
                   par = list(mai = times(0.1, c(0.82, 0.82, 0.82, 0.82)))) {
  list(selection = selection, width = width, height = height, 
       id = id, par = par)
}

# Handler ----------------------------------------------------------------------
handle_scale <- function(param) {
  res <- ifelse(param$log, logScale(param$log),
                param$scale || list(x = "scaleLinear", y = "scaleLinear"))
  res
}

# Conversion -------------------------------------------------------------------
logScale <- function(log) {
  if (log == "x") return(list(x = "scaleLog", y = "scaleLinear"))
  if (log == "y") return(list(x = "scaleLinear", y = "scaleLog"))
  if (log == "xy") return(list(x = "scaleLog", y = "scaleLog"))
  if (log == "yx") return(list(x = "scaleLog", y = "scaleLog"))
  stop("Wrong input for the parameter 'log'. It must be 'x', 'y', 'xy' or 'yx'.")
  NULL
}

length_data <- function() {
  args <- Array::from(arguments)
  args_len <- args$map(x %=>% x$length || 1)
  Math::max(...args_len)
}

has_id <- function(id) {
  if (!isArray(id)) id <- Array(1)$fill(id)
  lambda(d, d && d$id && id$includes(d$id))
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
