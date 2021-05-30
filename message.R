#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "d3", "auto"))

# Receive data from R and dispatch according to the data type ------------------
decoder <- function(name, predicate, handler) {
  list(name = name, predicate = predicate, handler = handler)
}

dispatchers <- Array(
  decoder("fn_init_svg",
          x %=>% (x == "fn_init_svg"),
          message %=>% JS_device$add_svg(message)),
  decoder("fn_points",
          x %=>% (x == "fn_points"),
          message %=>% JS_device$points(message))
)

ws$onmessage <- function(msg) {
  data <- JSON::parse(msg$data, remap_args)
  # console::log(JSON::stringify(data))
  JS_device$plot_stack$push(data)

  for (dispatch_fn in dispatchers) {
    if (dispatch_fn$predicate(data$type)) {
      dispatch_fn$handler(data$message)
    }
  }
  TRUE
}


# Map R base plot arguments to JavaScript d3 arguments
# Reference:
# https://stackoverflow.com/questions/60789391/rename-nested-key-in-array-of-objects-js
# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
remap_args <- function(k, v) {
  if (k == "pch") { this["shape"] <- v }
  else if (k == "col") { this["stroke"] <- v }
  else if (k == "bg")  { this["fill"] <- v }
  else if (k == "cex") { this["size"] <- v }
  else if (k == "lwd") { this["stroke-width"] <- v }
  else return(v)
  JS_UNDEFINED
}
