#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

#' One of 'linear', 'power', 'log', 'identity', 'time', 'radial',
#' 'sequential', 'diverging', ''quantize', 'quantile', 'threshold',
#' 'band', 'point'
d3_scale <- function(domain, range, type = "scaleLinear") {
  d3[type]()$domain(domain)$range(range)
}

d3_symbol <- function(size, shape) {
  d3::symbol()$size(size)$type(pch(shape))()
}

d3_attr <- R::curry(function(selection, attrs) {
  s <- selection
  for (x in Object::keys(attrs)) {
    s <- s$attr(x, attrs[x])
  }
  return(s)
})

d3_style <- R::curry(function(selection, styles) {
  s <- selection
  for (x in Object::keys(styles)) {
    s <- s$style(x, styles[x])
  }
  return(s)
})

d3_transition <- R::curry(function(selection, transition) {
  s <- selection$transition()
  for (x in Object::keys(transition)) {
    s <- s[x](transition[x])
  }
  return(s)
})

d3_call <- function(selection, f) {
  selection$call(f)
}

d3_extent <- function(x) {
  if (Array::isArray(x) && x$length > 1) {
    return(d3::extent(x))
  }
  # Scalar or Array of length 1
  y <- ifelse(Array::isArray(x), x[0], x)
  if (y == 0) return(Array(-1, 1))
  d3::extent(Array(0.6 * y, 1.4 * y))
}

d3_cond <- function(selection, f, pred) {
  if (pred) return(selection %>% f())
  return(selection)
}
