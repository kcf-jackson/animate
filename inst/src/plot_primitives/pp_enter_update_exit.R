#! config(rules = animate_rules(), deparsers = dp("basic", "auto"))

d3_enter_update_exit <- function(param, device, data0, tag, className, d3_update) {
  scale <- new_scale(param, device)
  attr  <- param$attr
  style <- param$style
  transition <- param$transition

  selection <- device$selection$
    selectAll(tag + "." + className)$
    filter(has_id(param$id))$
    data(data0)

  # Enter ----
  selection$
    enter()$
    append(tag)$
    classed(className, TRUE) %>%
    d3_update(scale) %>%
    d3_cond(d3_attr(ARG, attr), attr) %>%
    d3_cond(d3_style(ARG, style), style)

  # Update ----
  selection %>%
    d3_cond(d3_transition(ARG, transition), transition) %>%
    d3_update(scale) %>%
    d3_cond(d3_attr(ARG, attr), attr) %>%
    d3_cond(d3_style(ARG, style), style)

  # Exit ----
  selection$
    exit()$
    remove()
}
