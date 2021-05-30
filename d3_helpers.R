#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "auto"))

d3_scaleLinear <- function(domain, range) {
  d3::scaleLinear()$domain(domain)$range(range)
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
    if (x == "style") {
      s <- d3_style(s, transition[x])
    } else if (x == "attr") {
      s <- d3_attr(s, transition[x])
    } else {
      s <- s[x](transition[x])
    }
  }
  return(s)
})

translate <- function(x, y) {
  "translate(" %+% x %+% "," %+% y %+% ")"
}
