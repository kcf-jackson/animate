#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Generic X-Y plotting
plot <- function(param, device) {
  param %<>% set_default(
    list(id = generate_id("datum", length_of_data(param$x, param$y)),
         type = "p")
  )

  # Create drawing area if not already existed
  draw_area <- device$selection$
    select("g" %+% Id("drawing-area"))
  if (draw_area$empty()) {
    device$selection$
      append("g")$
      attr("id", "drawing-area")
  }

  # Plot data points
  if (param$type == "p") { points(Object::assign(list(), param), device) }
  if (param$type == "l") { lines(Object::assign(list(), param), device) }

  # Axes ----
  # x-axis
  axis_param <- Object::assign(
    list(), param, list(data = param$x, lim = param$xlim, side = 1, id = "x-axis"))
  axis(axis_param , device)

  # y-axis
  axis_param <- Object::assign(
    list(), param, list(data = param$y, lim = param$ylim, side = 2, id = "y-axis"))
  axis(axis_param, device)

  # Labels ----
  cw <- device$width
  ch <- device$height
  base_param <- list(xlim = c(0, 1), xrange = c(0, cw),
                     ylim = c(0, 1), yrange = c(0, ch),
                     attr = list("text-anchor" = "middle"),
                     style = list("font-family" = "Arial, Helvetica, sans-serif"))
  if (param$main) {
    text_param <- Object::assign(
      list(x = 0.5,
           y = 0.5 * device$top(),
           text = param$main,
           id = "main-title"),
      base_param
    )
    text(text_param, device)
  }
  if (param$xlab) {
    xlab_param <- Object::assign(
      list(x = 0.5,
           y = 1 - 0.0625 * device$bottom(),
           text = param$xlab,
           id = "x-label"),
      base_param
    )
    text(xlab_param, device)
  }
  if (param$ylab) {
    ylab_param <- Object::assign(
      list(x = 0.5 * device$left(),
           y = 0.5,
           text = param$ylab,
           id = "y-label",
           transform = "rotate(-90)"),
      base_param
    )
    text(ylab_param, device)
  }
  TRUE
}

# labels <- function(position, label, id) {
#   lab_param <- list(text = label, xlim = c(0, 1), ylim = c(0, 1), attr = list("text-anchor" = "middle"))
#   if (position == "left")        { opt <- list(x = 0.5 * device$left(), y = 0.5, id = "y-label", transform = "rotate(-90)") }
#   else if (position == "right")  { opt <- list(x = device$right() + 0.5 * (1 - device$right()), y = 0.5, id = "y-label"}
#   else if (position == "top")    { opt <- list(x = 0.5, y = 0.5 * device$top(), id = "x-label") }
#   else if (position == "bottom") { opt <- list(x = 0.5, y = 1 - 0.0625 * device$bottom(), id = "x-label") }
#   else { stop("Wrong input for the first argument 'position'.") }
#   Object::assign(lab_param, opt)
# }
