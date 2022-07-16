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
