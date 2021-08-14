#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Return the scale function for the X-Y plotting
new_scale <- function(param, device) {
  xlim <- param$xlim || device$par$xlim || d3_extent(param$x)
  ylim <- param$ylim || device$par$ylim || d3_extent(param$y)
  range <- device$range()
  xrange <- param$xrange || range$x
  yrange <- param$yrange || range$y
  scale <- get_scale_spec(param)
  list(x = d3_scale(domain = xlim, range = range$x, type = scale$x),
       y = d3_scale(domain = ylim, range = range$y, type = scale$y))
}

# get_scale_spec :: list -> list(x: character, y: character)
get_scale_spec <- function(param) {
  if (param$log) return(logScale(param$log))
  if (param$scale) return(Scale(param$scale))
  list(x = "scaleLinear", y = "scaleLinear")
}

# logScale :: character -> list(x: character, y: character)
logScale <- function(log) {
  if (log == "x") return(list(x = "scaleLog", y = "scaleLinear"))
  if (log == "y") return(list(x = "scaleLinear", y = "scaleLog"))
  if (log == "xy") return(list(x = "scaleLog", y = "scaleLog"))
  if (log == "yx") return(list(x = "scaleLog", y = "scaleLog"))
  stop("Wrong input for the parameter 'log'. It must be 'x', 'y', 'xy' or 'yx'.")
  NULL
}

Scale <- function(spec) {
  if (length(spec) == 1) {
    spec <- as_scalar(spec)
    return(list(x = spec, y = spec))
  }
  spec
}
