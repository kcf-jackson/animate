# "Hand-drawn" style plot

# Helpers ----------------------------------------------------------------------
finite_diff <- \(f, h = 1e-8) \(x) (f(x + h) - f(x)) / h

root <- \(f, interval) uniroot(f, interval)$root

# Custom animation
custom_transition <- function(f, from, to) {
  function(...) {
    attr <- names(from)
    # From
    args <- list(...)
    args$transition <- NULL
    args[[attr]] <- append(from[[attr]], args[[attr]])
    do.call(f, args)
    # To
    args <- list(...)
    if (is.null(args$transition)) {
      args$transition <- TRUE
    }
    args[[attr]] <- append(to[[attr]], args[[attr]])
    do.call(f, args)
  }
}

fade <- function(f) {
  custom_transition(f,
                    from = list(style = list(opacity = 0)),
                    to = list(style = list(opacity = 1)))
}

grow <- function(f, n) {
  custom_transition(f, from = list(cex = 1), to = list(cex = n))
}

animated_lines <- function(x, y, id, ...) {
  for (i in seq_along(x)) {
    lines(x[1:i], y[1:i], id = id, ...)
    Sys.sleep(0.01)
  }
}

connected_lines <- function(x, y, id, ...) {
  stopifnot(length(x) == 2 && length(y) == 2)
  py <- seq(y[1], y[2], 0.1 * sign(diff(y)))
  px <- seq(x[1], x[2], length.out = length(py))
  animated_lines(px, py, id, ...)
}


# Main -------------------------------------------------------------------------
# Set up device
library(animate)
device <- animate$new(800, 600)
attach(device)
lim <- -2:12
par(xlim = range(lim), ylim = range(lim))


# Data
x <- c(seq(-2, -1, 0.04), seq(-1, 11, 0.1), seq(11, 12, 0.04))
f <- \(x) 2*(1 + 1.5 *((x-3)/3.5)^4 - 4*((x-3)/3.5)^3 + 2*((x-2)/4)^2 + 1)
data0 <- data.frame(x = x, y = f(x))
data0 <- data0[data0$y >= -2 & data0$y <= 12, ]

# Find stationary points of f
df <- finite_diff(f)
x_stationary <- c(root(df, c(0,3)), root(df, c(3,6)), root(df, c(8, 10)))

# Derivative
data1 <- data.frame(x = x, y = df(x))
data1 <- data1[data1$y >= -2 & data1$y <= 12, ]


# Make grid
grid <- lim
abline(v = grid, style = list(opacity = 0.3))
abline(h = grid, style = list(opacity = 0.3))
Sys.sleep(3)

# Make x-axis and y-axis
fade(abline)(h = 0, id = "abline-1", transition = list(duration = 1000))
fade(abline)(v = 0, id = "abline-2", transition = list(duration = 1000))
Sys.sleep(1)

# Make text labels
fade(text)(0:11 + 0.2, -0.6, 0:11, id = new_id(0:11, prefix = "x-axis-label"),
               transition = list(delay = 200, duration = 1000))
fade(text)(-0.6, 1:9 - 0.2, 1:9, id = paste0("y-axis-label-", 1:9),
                transition = list(delay = 200, duration = 1000))
fade(text)(-0.7, 10:11 - 0.2, 10:11, id = paste0("y-axis-label-", 10:11),
                transition = list(delay = 200, duration = 1000))
Sys.sleep(1)

# Plot the original function f(x)
animated_lines(data0$x, data0$y, "line-1")
fade(text)(1.5, 7, "f(x)", id = "fx",
           style = list("font-size" = "32px", "font-style" = "italic"))

# Plot the derivative function f'(x)
animated_lines(data1$x, data1$y, "line-2", col = "blue")
fade(text)(2.5, -1.5, "f'(x)", id = "dfx",
                style = list("font-size" = "32px", "font-style" = "italic",
                             "fill" = "blue"))
Sys.sleep(1)

# Plot the roots on the derivative curve
grow(points, 10^2)(x_stationary, df(x_stationary), id = new_id(1:3, "d-root"),
                   bg = "blue")
Sys.sleep(1)

# Connect to the stationary points on the original curve
for (j in 1:length(x_stationary)) {
  r <- x_stationary[j]
  connected_lines(rep(r, 2), c(df(r), f(r)), id = paste0("root-line-", j))
  grow(points, 10^2)(r, f(r), id = paste0("root-", j))
  Sys.sleep(0.3)
}

off()
detach(device)
