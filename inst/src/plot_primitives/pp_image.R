#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))
#' Add an image to a plot
image <- function(param, device) {
  param %<>% set_default(
    list(x = 0, y = 1,
         id = generate_id("image", length_of_data(param$x, param$y)),
         transform = "",
         xlim = d3_extent(param$x),
         ylim = d3_extent(param$y))
  )
  keys <- c("x", "y", "href", "width", "height", "id")
  data0 <- as_data(param, keys)
  image_update <- function(transform) {
    function(selection, scale) {
      selection$
        attr("id", d %=>% d$id)$
        attr("transform", d %=>% translate(scale$x(d$x), scale$y(d$y)) %+% transform)$
        attr("href", d %=>% d$href)$
        attr("width", d %=>% d$width)$
        attr("height", d %=>% d$height)
    }
  }

  d3_enter_update_exit(param, device, data0,
                       tag = "image", className = "images",
                       image_update(param$transform))
}
