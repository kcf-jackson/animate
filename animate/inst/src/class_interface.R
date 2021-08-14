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
  par    <- set_default(par, list(mai = rep(0.082, 4)))

  #' Return the margin parameters
  bottom <- function() par$mai[0]
  left   <- function() par$mai[1]
  top    <- function() par$mai[2]
  right  <- function() par$mai[3]

  #' Return the x and y ranges of the plotting area
  range  <- function() {
    list(x = times(c(left(), 1 - right()), width),
         y = times(c(top(), 1 - bottom()), height)$reverse())
  }

  #' Set graphical parameters
  set_par <- function(parameters) Object::assign(par, parameters)

  #' Remove all elements from the device
  clear <- function() { selection$selectAll("*")$remove() }

  #' Remove a selected element from the device
  remove <- function(selector = "*", id) {
    filter_by_id <- selection %=>% selection$filter(has_id(id))
    selected <- selection$selectAll(selector) %>% d3_cond(filter_by_id, id)
    selected$remove()
  }

  export_ <- function() {
    # Replace reference object by its ID
    list(selection = selection$attr("id"),
         width = width, height = height,
         id = id, par = par)
  }

  import_ <- function(setting) {
    # Restore reference object
    selection <<- document$querySelector(setting$selection)
    width <<- setting$width
    height <<- setting$height
    id <<- setting$id
    par <<- setting$par
    TRUE
  }

  list(selection = selection, width = width, height = height, id = id,
       par = par,
       # Methods
       bottom = bottom, left = left, top = top, right = right,
       range = range, set_par = set_par,
       clear = clear, remove = remove,
       export = export_, import = import_)
}
