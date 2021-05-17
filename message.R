#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "d3", "auto"))

# Receive data from R and dispatch according to the data type ------------------
decoder <- function(name, predicate, handler) {
  list(name = name, predicate = predicate, handler = handler)
}

dispatchers <- Array(
  decoder("fn_init_svg",
          x %=>% (x == "fn_init_svg"),
          function(message) { add_svg(message$width, message$height) }),
  decoder("fn_points",
          x %=>% (x == "fn_points"),
          function(message) { points(message$x, message$y) })
)

ws$onmessage <- function(msg) {
  data <- JSON::parse(msg$data)
  console::log(data)
  plot_stack$push(data)

  message <- data$message
  for (dispatch_fn in dispatchers) {
    if (dispatch_fn$predicate(data$type)) {
      dispatch_fn$handler(message)
    }
  }
  TRUE
}
