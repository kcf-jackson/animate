#' #' Add bars to a plot
#' bars = function(param) {
#'   build_arg_list <- broadcast(
#'     function(x, y, w, h, id, fill, stroke, stroke_width, stroke_dasharray) {
#'       list(x = x, y = y, w = w, h = h, id = id,
#'            fill = fill, stroke = stroke,
#'            "stroke-width" = stroke_width,
#'            "stroke-dasharray" = stroke_dasharray)
#'     }
#'   )
#'
#'   # Parameters destructuring ----
#'   x <- param$x
#'   y <- param$y
#'   w <- param$w
#'   h <- param$h
#'   id     <- param$id || generate_id("rect", length_data(x, y, w, h))
#'   fill   <- param$fill || "black"
#'   stroke <- param$stroke || "black"
#'   stroke_width <- param["stroke-width"] || 0
#'   stroke_dasharray <- param["stroke-dasharray"] || "none"
#'
#'   attr <- param$attr
#'   style <- param$style
#'   transition <- param$transition
#'
#'   # Set auxiliary variables ----
#'   data0 <- build_arg_list(x, y, w, h, id, fill, stroke, stroke_width, stroke_dasharray)
#'
#'   # Scale ----
#'   scale <- param$scale || "scaleLinear"
#'   xlim <- param$xlim || d3::extent(add(x, w))
#'   ylim <- param$ylim || d3::extent(add(y, h))
#'   x_scale <- d3_scale(domain = xlim, range = self$range("x"), scale)
#'   y_scale <- d3_scale(domain = ylim, range = self$range("y"), scale)
#'   cw <- self$device$width
#'   ch <- self$device$height
#'
#'   # Main update ----
#'   selection <- self$device$selection$
#'     selectAll("rect.bars")$
#'     filter(d %=>% id$includes(d$id))$
#'     data(data0)
#'
#'   d3_update <- function(s) {
#'     s$attr("id", d %=>% d$id)$
#'       attr("x", d %=>% x_scale(d$x))$
#'       attr("y", d %=>% y_scale(d$y + d$h))$
#'       attr("width", d %=>% x_scale(d$w) - cw * private$left())$
#'       attr("height", d %=>% ch * (1 - private$bottom()) - y_scale(d$h))$
#'       style("fill", d %=>% d$fill)$
#'       style("stroke-dasharray", d %=>% d["stroke-dasharray"])$
#'       style("stroke-width", d %=>% d["stroke-width"])$
#'       style("stroke", d %=>% d$stroke) %>%
#'       d3_cond(d3_attr(ARG, attr), attr) %>%
#'       d3_cond(d3_style(ARG, style), style)
#'   }
#'
#'   # Enter ----
#'   selection$
#'     enter()$
#'     append("rect")$
#'     classed("bars", TRUE) %>%
#'     d3_update()
#'
#'   # Update ----
#'   selection %>%
#'     d3_cond(d3_transition(ARG, transition), transition) %>%
#'     d3_update()
#'
#'   # Exit ----
#'   selection$
#'     exit()$
#'     remove()
#'
#'   return(TRUE)
#' },
