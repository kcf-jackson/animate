# Project specific rules
library(sketch)
animate_rules <- function() {
  new_rules <- combine_rules(list(
    make_rule("isArray", "Array.isArray"),
    make_rule("names", "Object.keys"),
    make_rule("c", "Array"),
    make_rule("ARG", "R.__"),
    make_rule("curry", "R.curry")
  ))
  append(basic_rules(), new_rules)
}


# Testing
sketch::test_sketch("utils.R", "tests/test_utils.R")
sketch::test_sketch("plot_helpers.R", "tests/test_plot_helpers.R")
sketch::test_sketch("class_interface.R", "tests/test_class_interface.R")
