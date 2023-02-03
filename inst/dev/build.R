# # Project specific rules
# library(sketch)
# animate_rules <- function() {
#   new_rules <- combine_rules(list(
#     make_rule("isArray", "Array.isArray"),
#     make_rule("names", "Object.keys"),
#     make_rule("c", "Array"),
#     make_rule("ARG", "R.__"),
#     make_rule("curry", "R.curry")
#   ))
#   append(basic_rules(), new_rules)
# }
#
#
# # Build ------------------------------------------------------------------------
# source("inst/dev/build_helpers.R")
#
# # Build the app
# build(-1, FALSE)
#
# # Build the library (for usage with shiny and R Markdown Document)
# build_library("inst/src/plot.R")
# build_shiny("inst/src/addons/shiny.R")
# build_virtual_device("inst/src/addons/virtual_device.R")
#
#
# # Testing ----------------------------------------------------------------------
# if (FALSE) {
#   sketch::test_sketch("utils.R", "tests/test_utils.R")
#   sketch::test_sketch("plot_helpers.R", "tests/test_plot_helpers.R")
#   sketch::test_sketch("class_interface.R", "tests/test_class_interface.R")
# }
