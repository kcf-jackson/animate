#' Generic X-Y plotting
plot <- function(param, device) {
  x    <- param$x
  y    <- param$y
  id   <- param$id || generate_id("datum", length_of_data(x, y))
  type <- param$type || "p"
  xlim <- param$xlim || d3::extent(x)
  ylim <- param$ylim || d3::extent(y)
  main <- param$main
  xlab <- param$xlab
  ylab <- param$ylab

  # Add points ----
  selection <- device$selection

  # Create drawing area if not already existed
  draw_area <- selection$
    selectAll("g" %+% Id("drawing-area"))
  if (draw_area$empty()) {
    draw_area <- selection$
      append("g")$
      attr("id", "drawing-area")
  }
  device$selection <- draw_area


  if (type == "p") { points(param, device) }
  if (type == "l") { lines(param, device) }

  device$selection <- selection

  # Axes ----
  # x-axis
  Object::assign(param, list(data = x, lim = xlim, axis = "axisBottom", id = "x-axis"))
  self$axis(param)

  # y-axis
  Object::assign(param, list(data = y, lim = ylim, axis = "axisLeft", id = "y-axis"))
  self$axis(param)

  # Labels ----
  cw <- device$width
  ch <- device$height
  if (main) {
    text_param <- list(x = 0.5,
                       y = 0.5 * device$top(),
                       text = main,
                       id = "main-title",
                       xlim = c(0, 1), xrange = c(0, cw),
                       ylim = c(0, 1), yrange = c(0, ch),
                       attr = list("text-anchor" = "middle"))
    self$text(text_param)
  }
  if (xlab) {
    xlab_param <- list(x = 0.5,
                       y = 1 - 0.0625 * device$bottom(),
                       text = xlab,
                       id = "x-label",
                       xlim = c(0, 1), xrange = c(0, cw),
                       ylim = c(0, 1), yrange = c(0, ch),
                       attr = list("text-anchor" = "middle"))
    self$text(xlab_param)
  }
  if (ylab) {
    ylab_param <- list(x = 0.5 * device$left(),
                       y = 0.5,
                       text = ylab,
                       id = "y-label",
                       xlim = c(0, 1), xrange = c(0, cw),
                       ylim = c(0, 1), yrange = c(0, ch),
                       transform = "rotate(-90)",
                       attr = list("text-anchor" = "middle"))
    self$text(ylab_param)
  }

  TRUE
}
