#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))
#! load_script("assets/ramda.min.js")
#! load_script("assets/broadcast.js")
#! load_script("utils.R")

length_of_data <- function() {
  max(Array::from(arguments)$map(length))
}

has_id <- function(id) {
  if (!isArray(id)) id <- as_array(id)
  lambda(d, d && d$id && id$includes(d$id))
}

as_array <- function(x) {
  Array(1)$fill(x)
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


#-------------------------------------------------------------------------------
#' ID generator
generate_id <- (function(id_count) {
  res_fun <- function(prefix, n = 1) {
    res <- Array()
    i <- 0
    while (i < n) {
      id_count <<- id_count + 1
      id <- ifelse(prefix, prefix %+% "_" %+% id_count, id_count)
      res$push(id)
      i <- i + 1
    }
    if (res$length == 0) return("")
    if (res$length == 1) return(res[0])
    res
  }
  res_fun
})(0)


#' Turn a list of parameters of unequal length into a list of attributes of
#' equal length
as_data <- function(param, keys) {
  inside_out(as_data_(param, keys))
}

as_data_ <- function(param, keys) {
  if (!keys) keys <- names(param)
  sub_param <- subset(param, keys)
  max_length <- max(length_of_list(sub_param))
  res <- list()
  for (key in keys) {
    res[key] <- cycle(param[key], max_length)
  }
  res
}

#' Turns a list of array into an array of list
inside_out <- function(xs) {
  res <- c()
  first_key <- names(xs)[0]
  n <- length(xs[first_key])
  i <- 0
  while (i < n) {
    new_obj <- list()
    for (key in names(xs)) {
      new_obj[key] <- xs[key][i]
    }
    res$push(new_obj)
    i <- i + 1
  }
  res
}

#' Get the length of all the attributes
length_of_list <- function(x) {
  lens <- Array()
  for (key in names(x)) {
    lens$push(length(x[key]))
  }
  lens
}

is_scalar <- function(x) {
  c("string", "boolean", "number")$includes(typeof(x))
}

#' Recycle a list of items to reach a desired length
cycle <- function(xs, n) {
  if (is_scalar(xs)) xs <- as_array(xs)

  res <- Array(n)
  if (n == 0) return(res)

  len_xs <- length(xs)
  i <- 0
  while (i < n) {
    res[i] <- xs[i %% len_xs]
    i <- i + 1
  }
  res
}


#' Concatenate two Arrays / values
concat <- function(x, y) {
  if (isArray(x) && isArray(y)) {
    return(x$concat(y))
  }

  if (is_scalar(x) && isArray(y)) {
    return(as_array(x)$concat(y))
  }

  if (isArray(x) && is_scalar(y)) {
    return(x$concat(as_array(y)))
  }

  if (is_scalar(x) && is_scalar(y)) {
    return(Array(x, y))
  }

  return(JS_UNDEFINED)
}
