#! config(debug = F, rules = animate_rules(), deparsers = dp("basic", "auto"))
#! load_script("assets/ramda.min.js")

#' One of 'linear', 'power', 'log', 'identity', 'time', 'radial',
#' 'sequential', 'diverging', ''quantize', 'quantile', 'threshold',
#' 'band', 'point'
d3_scale <- function(domain, range, type = "scaleLinear") {
  d3[type]()$domain(domain)$range(range)
}

d3_symbol <- function(size, shape) {
  d3::symbol()$size(size)$type(pch(shape))()
}

d3_attr <- curry(function(selection, attrs) {
  s <- selection
  for (x in Object::keys(attrs)) {
    s <- s$attr(x, attrs[x])
  }
  return(s)
})

d3_style <- curry(function(selection, styles) {
  s <- selection
  for (x in Object::keys(styles)) {
    s <- s$style(x, styles[x])
  }
  return(s)
})

d3_transition <- curry(function(selection, transition) {
  s <- selection$transition()
  for (x in Object::keys(transition)) {
    if (x == "on") {
      # chained transition
      param <- transition[x]
      s <- s[x](param$event, function(d) {
        message <- list(type = "user_event",
                        message = list(param = param,
                                       data = d,
                                       event = d3$event))
        if (param$shiny) {
          Shiny::setInputValue("animate_event", message)
        } else {
          ws$send(JSON::stringify(message))
        }
        return(TRUE)
      })
    } else {
      s <- s[x](transition[x])
    }
  }
  return(s)
})

d3_call <- function(selection, f) {
  selection$call(f)
}

d3_extent <- function(x) {
  if (isArray(x) && x$length > 1) {
    return(d3::extent(x))
  }
  # Scalar or Array of length 1
  y <- as_scalar(x)
  if (y == 0) {
    return(c(-1, 1))
  }
  # Calling `d3::extent` to also handle the negative case
  d3::extent(c(0.6 * y, 1.4 * y))
}

d3_cond <- function(selection, f, pred) {
  if (pred) return(selection %>% f())
  return(selection)
}

as_scalar <- function(x) {
  ifelse(isArray(x), x[0], x)
}
