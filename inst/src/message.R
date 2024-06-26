#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "d3", "auto"))


# Receive data from R and dispatch according to the data type
ws$onmessage <- function(msg) {
  data <- JSON::parse(msg$data, remap_args)
  # console::log(JSON::stringify(data))
  if (typeof(JS_device) != 'undefined') {
    JS_device$record(data)
    JS_device$dispatch(data)
  }
  JS_UNDEFINED
}


# Map R base plot arguments to JavaScript d3 arguments
# Reference:
# https://stackoverflow.com/questions/60789391/rename-nested-key-in-array-of-objects-js
# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
remap_args <- function(k, v) {
  if (k == "pch") { this["shape"] <- v }
  else if (k == "col")    { this["stroke"] <- v }
  else if (k == "bg")     { this["fill"] <- v }
  else if (k == "cex")    { this["size"] <- v }
  else if (k == "lwd")    { this["stroke-width"] <- v }
  else if (k == "lty")    { this["stroke-dasharray"] <- v }
  else if (k == "lend")   { this["stroke-linecap"] <- v }
  else if (k == "ljoin")  { this["stroke-linejoin"] <- v }
  else if (k == "lmitre") { this["stroke-miterlimit"] <- v }
  else if (k == "labels") { this["text"] <- v }
  else { return(v) }
  JS_UNDEFINED
}
