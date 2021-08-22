#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))
#! load_library("d3")
#! load_script("assets/ramda.min.js")
#! load_script("assets/broadcast.js")
#! load_script("utils.R")

# Interface --------------------------------------------------------------------
Decoder <- function(name, handler) {
  predicate <- x %=>% (x == name)
  list(name = name, predicate = predicate, handler = handler)
}


#' The Device interface
#'
#' @param selection The DOM element to host the device.
#' @param width An integer; the width of the device.
#' @param height An integer; the height of the device.
#' @param id A character string; the id of the device.
#' @param par A list; other additional device parameters.
Device <- function(selection, width, height, id, par = list()) {
  par <- set_default(par, list(mai = rep(0.082, 4)))
  env <- list(selection = selection, width = width, height = height,
              id = id, par = par)

  #' Return the margin parameters
  bottom <- function() env$par$mai[0]
  left   <- function() env$par$mai[1]
  top    <- function() env$par$mai[2]
  right  <- function() env$par$mai[3]

  #' Return the x and y ranges of the plotting area
  range  <- function() {
    list(x = times(c(left(), 1 - right()), env$width),
         y = times(c(top(), 1 - bottom()), env$height)$reverse())
  }

  #' Set graphical parameters
  set_par <- function(parameters) {
    env$par <- Object::assign(env$par, parameters)
    env
  }

  #' Remove all elements from the device
  clear <- function() {
    env$selection$selectAll("*")$remove()
  }

  #' Remove a selected element from the device
  remove <- function(selector = "*", id) {
    filter_by_id <- selection %=>% selection$filter(has_id(id))
    selected <- env$selection$
      selectAll(selector) %>%
      d3_cond(filter_by_id, id)
    selected$remove()
  }

  export_ <- function() {
    # Replace the reference object by its ID
    new_env <- Object::assign(list(), env)
    new_env$selection <- new_env$selection$attr("id")
    new_env
  }

  import_ <- function(setting) {
    # Restore the reference object
    env$selection <- document$querySelector(Id(setting$selection))
    env$width <- setting$width
    env$height <- setting$height
    env$id <- setting$id
    env$par <- setting$par
    TRUE
  }

  Object::assign(
    env,
    list(# Methods
       bottom = bottom, left = left, top = top, right = right,
       range = range, set_par = set_par,
       clear = clear, remove = remove,
       export = export_, import = import_
    )
  )
}
