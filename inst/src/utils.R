#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))
#! load_script("assets/ramda.min.js")
#! load_script("assets/broadcast.js")

# Alias ----
isNull  <- lambda(x, x == NULL)
isUndefined <- lambda(x, x == JS_UNDEFINED)
isEmpty <- lambda(x, isNull(x) || isUndefined(x))
isObject <- lambda(x, typeof(x) == "object")

# Text related functions ----
Id         <- lambda(x, "#" %+% x)
parse_px   <- lambda(x, parseInt(x$replace("px", "")))
end_string <- lambda(x, x %+% "\n")
translate  <- lambda(x, y, "translate(" %+% x %+% "," %+% y %+% ")")

# Helpers ----
add    <- broadcast(lambda(x, y, x + y))
minus  <- broadcast(lambda(x, y, x - y))
times  <- broadcast(lambda(x, y, x * y))
divide <- broadcast(lambda(x, y, x / y))
rep    <- lambda(x, n, Array(n)$fill(x))

#' Find the length of a scalar, Array or Object.
length <- function(x) {
  if (isNull(x))   return(0)  # order is strict because `null` is an object.
  if (isObject(x)) return(names(x)$length)
  if (isArray(x))  return(x$length)
  return(1)
}

#' Take a maximum of an Array of numbers
max <- function(xs) {
  Math::max(...xs)
}

#' Generate a sequence of numbers
seq <- function(from, to, by = 1) {
  res <- Array()
  res$push(from)

  if (from == to) {
    return(res)
  }

  if (Math::sign(to - from) != Math::sign(by)) {
    stop("Cannot go from the first argument to the second arguemnt using a step size given in the third argument")
  }

  cur <- from + by
  while (cur <= to) {
    res$push(cur)
    cur <- cur + by
  }
  res
}

#' Set the default for a list of parameters
#' The default will be set only for attributes that do not already have a value
set_default <- function(param, default_param) {
  for (item in names(default_param)) {
    if (isEmpty(param[item])) {
      param[item] <- default_param[item]
    }
  }
  param
}

#' Subset a list using a set of keys
subset <- function(xs, keys) {
  ys <- Object::assign(list(), xs)
  res <- list()
  for (key in keys) {
    res[key] <- ys[key]
  }
  return(res)
}
