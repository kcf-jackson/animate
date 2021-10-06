#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))
#' Add an image to a plot
image <- function(param, device) {
  param %<>% set_default(
    list(x = 0, y = 1,
         id = generate_id("image", length_of_data(param$x, param$y)),
         transform = "")
  )

  keys <- c("x", "y", "href", "width", "height", "id")
  data0 <- as_data(param, keys)

  param$xlim <- param$xlim || device$par$xlim || d3_extent(c(param$x, add(param$x, param$width)))
  param$ylim <- param$ylim || device$par$ylim || d3_extent(c(param$y, add(param$y, param$height)))

  image_update <- function(transform) {
    function(selection, scale) {
      selection$
        attr("id", d %=>% d$id)$
        attr("transform", d %=>% translate(scale$x(d$x), scale$y(d$y + d$height)) %+% transform)$
        attr("href", d %=>% d$href)$
        attr("width", d %=>% scale$x(d$width) - scale$x(0))$
        attr("height", d %=>% scale$y(0) - scale$y(d$height))
    }
  }

  d3_enter_update_exit(param, device, data0,
                       tag = "image", className = "images",
                       image_update(param$transform))
}
