#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

#' Add event listener
event = function(param, device) {
  d3$selectAll(param$selector)$
    on(param$event, function(d) {
      ws$send(JSON::stringify(
        list(
          type = "user_event",
          message = list(
            param = param,
            data = d,
            event = d3$event,
            mouse = d3$mouse(this)
          )
        )
      ))
    })
}
