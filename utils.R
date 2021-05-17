#! config(debug = T, rules = basic_rules(), deparsers = dp("basic", "d3", "auto"))

px <- function(n) { n %+% "px" }

# Convert named list of arrays into Array of named lists------------------------
as_row_data <- function(named_list) {
  names <- Object$keys(named_list)
  if (names$length == 0) return(Array())

  res <- Array()
  n <- named_list[names[0]]$length
  for (i in R::seq(0, n-1)) {
    datum <- list()
    for (variable in names) {
      datum[variable] <- named_list[variable][i]
    }
    res$push(datum)
  }

  res
}


# # Unit test
# input <- list(x = Array(1,2,3), y = Array(1,2,3))
# output <- as_row_data(input)
# expected <- Array(list(x = 1, y = 1), list(x = 2, y = 2), list(x = 3, y = 3))
# console::log(input)
# console::log(output)
# console::log(expected)
# console::log(eqArray(output, expected))
