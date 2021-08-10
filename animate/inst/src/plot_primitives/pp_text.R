#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))
#' Add text to a plot
text <- function(param, device) {
  param %<>% set_default(
    list(id = generate_id("text", length_of_data(param$x, param$y)),
         transform = "")
  )
  data0 <- as_data(param, c("x", "y", "text", "id"))
  text_update <- function(transform) {
    function(selection, scale) {
      selection$
        attr("id", d %=>% d$id)$
        attr("transform", d %=>% translate(scale$x(d$x), scale$y(d$y)) %+% transform)$
        text(d %=>% d$text)
    }
  }

  d3_enter_update_exit(param, device, data0,
                       tag = "text", className = "labels",
                       text_update(param$transform))
}
