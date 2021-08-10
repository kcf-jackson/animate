#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add an axis to a plot
axis = function(param, device) {
  data  <- param$data
  axis  <- param$axis || "axisBottom"

  lim   <- param$lim || d3::extent(data)
  orientation <- ifelse(axis == "axisBottom" || axis == "axisTop", "x", "y")
  scale <- param$scale || "scaleLinear"

  scale_fun <- d3_scale(domain = lim, range = device$range()[orientation], scale)
  axis_fun  <- d3[axis]()$scale(scale_fun)

  id <- param$id || generate_id(param$orientation %+% "-axis")
  transition <- param$transition

  # Axes ----
  selected_axis <- device$selection$
    selectAll("g" %+% Id(id))

  if (selected_axis$empty()) {
    selected_axis <- selection$
      append("g")$
      attr("id", id)$
      classed("axis", TRUE)
  }

  selected_axis$
    attr("transform", axis_transform(param$axis, device)) %>%
    d3_cond(d3_transition(ARG, transition), transition) %>%
    d3_call(axis_fun)
}

# Compute the transform attribute given the axis position
axis_transform <- function(axis_type, device) {
  left   <- device$left() * device$width
  right  <- (1 - device$right()) * device$width
  top    <- device$top() * device$height
  bottom <- (1 - device$bottom()) * device$height
  if (axis == "axisLeft")   transform <- "translate(" %+% left %+% ", 0)"
  if (axis == "axisRight")  transform <- "translate(" %+% right %+% ", 0)"
  if (axis == "axisTop")    transform <- "translate(0, " %+% top %+% ")"
  if (axis == "axisBottom") transform <- "translate(0, " %+% bottom %+% ")"
  transform
}
