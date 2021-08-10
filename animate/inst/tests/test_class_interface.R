#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"))

device_1 <- Device("placeholder-1", 600, 800, "id-1")
testthat::expect_equal(device_1$width, 600)
testthat::expect_equal(device_1$height, 800)

testthat::expect_equal(device_1$bottom(), 0.082)
testthat::expect_equal(device_1$top(), 0.082)
testthat::expect_equal(device_1$left(), 0.082)
testthat::expect_equal(device_1$right(), 0.082)

device_1$par$mai <- rep(0.1, 4)
testthat::expect_equal(device_1$bottom(), 0.1)
testthat::expect_equal(device_1$top(), 0.1)
testthat::expect_equal(device_1$left(), 0.1)
testthat::expect_equal(device_1$right(), 0.1)


device_2 <- Device("placeholder-2", 1000, 500, "id-2")
testthat::expect_equal(device_2$range()$x, c(82, 918))
testthat::expect_equal(device_2$range()$y, c(459, 41))

device_2$set_par(list(mai = rep(0, 4)))
testthat::expect_equal(device_2$range()$x, c(0, 1000))
testthat::expect_equal(device_2$range()$y, c(500, 0))
