#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))


# Test `cycle` ----
input <- c(1, 2, 3)
output_1 <- c(1, 2, 3, 1, 2)
output_2 <- c(1, 2, 3, 1, 2, 3, 1)
output_3 <- c(1, 2)
output_4 <- c()
input_2 <- c("ab", "cd")
output_5 <- c("ab", "cd", "ab")
testthat::expect_equal(cycle(input, 5), output_1)
testthat::expect_equal(cycle(input, 7), output_2)
testthat::expect_equal(cycle(input, 2), output_3)
testthat::expect_equal(cycle(input, 0), output_4)
testthat::expect_equal(cycle(input_2, 3), output_5)

# Test `as_data_` ----
input <- list(x = c(1,2), y = 'a', z = c(3,2,1))
output <- list(x = c(1,2,1), y = c('a','a','a'), z = c(3,2,1))
testthat::expect_equal(as_data_(input), output)


# Test `length_of_data` ----
testthat::expect_equal(length_of_data(c(1,2,3), c(1,2)), 3)
testthat::expect_equal(length_of_data(999, c(1,2)), 2)


# Test `length_of_list` ----
input <- list(x = c(1,2,3), y = 3)
input_2 <- list(x = c(1,2), y = c(1,2,3))
testthat::expect_equal(length_of_list(input), c(3,1))
testthat::expect_equal(length_of_list(input_2), c(2,3))


# Test `inside_out` ----
input <- list(x = c(1,2), y = c('a', 'b'))
output <- c(list(x = 1, y = 'a'), list(x = 2, y = 'b'))
testthat::expect_equal(inside_out(input), output)
