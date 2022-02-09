#' Create an animate output (container) element
#'
#' @param outputId output variable to read the plot/image from.
#' @param width Width of the plot area. Must be a valid CSS unit (like "100%", "400px", "auto").
#' @param height Height of the plot area. Must be a valid CSS unit (like "100%", "400px", "auto").
#' @param ... Optional CSS styling for the container of the plot.
#'
#' @note (Advanced usage) A "stack_limit" parameter can be included in the optional parameters
#' to control how many directives the device should keep track of.
#'
#' @export
animateOutput <- function(outputId = "animateOutput", width = "100%", height = "400px", ...) {
  # advanced usage
  args <- list(...)
  if (!args$stack_limit) {
    stack_limit <- -1
  } else {
    stack_limit <- args$stack_limit
    args$stack_limit <- NULL
  }

  style <- htmltools::css(width = width, height = height, ...)
  htmltools::tagList(
    animateDependencies(),
    do.call(htmltools::div, list(id = outputId, style = style, class = "shiny-animate-output")),
    shiny::tags$script(paste0("JS_device = new plot2(", stack_limit, ")"))
  )
}


#' The HTML dependency of an 'animate' plot
animateDependencies <- function() {
  htmltools::htmlDependency(
    name = "animate-assets",
    version = "0.3.1",
    package = "animate",
    src = "dist",
    script = c("animate.js", "shiny.js")
  )
}
