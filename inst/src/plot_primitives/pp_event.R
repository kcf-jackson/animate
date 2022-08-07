#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add event listener
event = function(param, device) {
  d3$selectAll(param$selector)$
    on(param$event, function(d) {
      message <- list(
        type = "user_event",
        message = list(
          param = param,
          data = d,
          event = d3$event,
          keyCode = d3$event$keyCode,
          mouse = d3$mouse(this)
        )
      )

      if (param$shiny) {
        Shiny::setInputValue("animate_event", message)
      } else {
        ws$send(JSON::stringify(message))
      }
      return(TRUE)
    })
}

#' Add simple event listener
simple_event = function(param, plot2_object) {
  map_name = function(x) {
    if (x == "svg") return("fn_init_svg")
    if (c("bars", "objects", "plot", "points", "lines", #"abline",
          "axis", "text", "image", #"event", "simple_event"
          "set", "par", "remove", "clear",
          "delete", "import", "export")$includes(x)) {
      return("fn_" + x)
    }
    if (x == "record") return("fn_export_video")
    return(FALSE)
  }
  d3$selectAll(param$selector)$
    on(param$event, function(d) {
      console::log(param$event)
      fun_type = map_name(param$method)
      if (fun_type) {
        plot2_object$dispatch(
          list(type = fun_type, message = param$param)
        )
      }
      return(TRUE)
    })
}
