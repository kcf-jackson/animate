# Helpers
rose <- function(theta, a = 1, k = 3) {
  # `a` controls the size, and `k` controls the shape of the rose
  list(x = a * cos(k * theta) * cos(theta), y = a * cos(k * theta) * sin(theta))
}

is_odd <- function(x) x %% 2 == 1
period <- ifelse(is_odd(p) && is_odd(q), pi * q, 2 * pi * q)


# Part 1 -----------------------------------------------------------------------
# 1 rose
library(animate)
device <- animate$new(600, 600)
attach(device)


# Data
p <- 2
q <- 7
k <- p / q
time <- seq(from = 0, to = period, length.out = 400)  # 400 time steps
pts <- rose(time, a = 1, k = k)


# Plot
par(xlim = extendrange(pts$x), ylim = extendrange(pts$y))
for (step in seq_along(time)) {
  lines(pts$x[1:step], pts$y[1:step], id = "line-1")
  points(pts$x[step], pts$y[step], id = "point-1")
  Sys.sleep(0.01)
}


# Clean up for the next part
clear()
off()


# Part 2 -----------------------------------------------------------------------
# 4 x 4 roses
# Data
ps <- 1:4
qs <- c(5, 7, 11, 13)
roses <- list()
for (p in ps) {
  for (q in qs) {
    period <- ifelse(is_odd(p) && is_odd(q), pi * q, 2 * pi * q)
    time <- seq(from = 0, to = period, length.out = 400)
    roses[[length(roses) + 1]] <- rose(time, a = 1, k = p / q)
  }
}


# Create 15 additional plot areas. The id of the first plot area is by default "SVG_1";
# we follow the convention and name the rest as "SVG_i", where i = 2, ..., 16.
device <- animate$new(150, 150)
attach(device)
for (i in 2:16) {
  svg(150, 150, id = paste0("SVG_", i))
}


# Loop over the plot area and draw the corresponding rose.
for (step in 1:400) {
  for (i in 1:16) {
    set(paste0("SVG_", i))  # Make plot area named "SVG_i" active
    pts <- roses[[i]]
    par(xlim = extendrange(pts$x), ylim = extendrange(pts$y))
    lines(pts$x[1:step], pts$y[1:step], id = "line-1")
    points(pts$x[step], pts$y[step], id = "point-1")
  }
}
