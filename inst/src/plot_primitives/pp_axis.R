#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add an axis to a plot
axis = function(param, device) {
  lim   <- param$lim || d3_extent(param$data)
  side  <- axis_code(param$side || 1)
  xy_axis <- ifelse(side == "axisBottom" || side == "axisTop", "x", "y")
  position <- param$position || side
  id    <- param$id || generate_id(xy_axis %+% "-axis")
  scale <- param$scale || "scaleLinear"

  scale_fun <- d3_scale(domain = lim, range = device$range()[xy_axis], scale)
  axis_fun  <- d3[position]()$scale(scale_fun)

  # Axes ----
  selected_axis <- device$selection$
    select("g" %+% Id(id))

  if (selected_axis$empty()) {
    selected_axis <- device$selection$
      append("g")$
      attr("id", id)$
      classed("axis", TRUE)
  }

  selected_axis$
    attr("transform", axis_transform(side, device)) %>%
    d3_cond(d3_transition(ARG, param$transition), param$transition) %>%
    d3_call(axis_fun)
}

# Convert an axis code into the d3 axis option
axis_code <- function(x) {
  if (x == 1) return("axisBottom")
  if (x == 2) return("axisLeft")
  if (x == 3) return("axisTop")
  if (x == 4) return("axisRight")
  x
}

# Compute the transform attribute given the axis position
axis_transform <- function(axis_type, device) {
  left   <- device$left() * device$width
  right  <- (1 - device$right()) * device$width
  top    <- device$top() * device$height
  bottom <- (1 - device$bottom()) * device$height
  if (axis_type == "axisLeft")   transform <- "translate(" %+% left %+% ", 0)"
  if (axis_type == "axisRight")  transform <- "translate(" %+% right %+% ", 0)"
  if (axis_type == "axisTop")    transform <- "translate(0, " %+% top %+% ")"
  if (axis_type == "axisBottom") transform <- "translate(0, " %+% bottom %+% ")"
  transform
}
