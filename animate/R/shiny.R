#' Load the assets for an animate plot
#'
#' @param n The number of commands the plot stack can hold. Use -1 for unlimited
#' number of commands.
#'
#' @note This function will only work if the 'www' folder of the Shiny app contains
#' the relevant JavaScript files. The files can be created using the function
#' `setup_animate`.
#'
#' @export
load_animate <- function(n = 0) {
  tags <- shiny::tags
  tags$head(
    shiny::tags$script(src = "d3.v5.min.js"),
    tags$script(src = "ramda.min.js"),
    tags$script(src = "broadcast.js"),
    tags$script(src = "d3-symbol-extra.min.js"),
    tags$script(src = "animate_shiny.js"),
    tags$script(src = "shiny.js"),
    tags$script(paste0("JS_device = new plot2(", n, ")"))
  )
}


#' Copy the necessary JavaScript assets to the 'www' folder of a Shiny app
#'
#' @param path The path to the 'www' folder
#'
#' @export
setup_animate <- function(path) {
  asset <- function(x) system.file(x, package = "animate")

  file.copy(asset("assets/d3.v5.min.js"), path)
  file.copy(asset("assets/ramda.min.js"), path)
  file.copy(asset("assets/broadcast.js"), path)
  file.copy(asset("assets/d3-symbol-extra.min.js"), path)
  file.copy(asset("dist/animate_shiny.js"), path)
  file.copy(asset("dist/shiny.js"), path)
}
