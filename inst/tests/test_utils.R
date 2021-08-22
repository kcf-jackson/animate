#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))


# Test `set_default` ----
param <- list(x = 1, y = 2)
param_def <- list(y = 3, z = 3)

output <- set_default(param, param_def)
testthat::expect_equal(output$x, 1)
testthat::expect_equal(output$y, 2)
testthat::expect_equal(output$z, 3)
# console::log(output)


# Test `length` ----
testthat::expect_equal(length(c(1,2,3)), 3)
testthat::expect_equal(length(999), 1)
testthat::expect_equal(length(NULL), 0)


# Test `rep` ----
input <- rep(5, 3)
output <- c(5, 5, 5)
testthat::expect_equal(input, output)


# Test `subset` ----
input <- list(x = 1, y = 2, z = 3)
output_1 <- list(y = 2)
output_2 <- list(x = 1, z = 3)
testthat::expect_equal(subset(input, c("y")), output_1)
testthat::expect_equal(subset(input, c("x", "z")), output_2)
