library(animate)

# Example 1 ----
device <- animate$new(500, 300)
attach(device)
x <- 1:10
y <- 1:10
id <- new_id(x)   # Give each point an ID
plot(x, y, id = id)

new_y <- 10:1
plot(x, new_y, id = id, transition = TRUE)
export(path = "basic_plot.json", handler = 'r')


# Improved example 1 ----
device <- animate$new(400, 400)
attach(device)
x <- c(0.5, 1, 0.5, 0, -0.5, -1, -0.5, 0)
y <- c(0.5, 0, -0.5, -1, -0.5, 0, 0.5, 1)
id <- new_id(x)
plot(x, y, id = id)

# Transition (basic)
shuffle <- c(8, 1:7)
plot(x[shuffle], y[shuffle], id = id, transition = TRUE)  # Use transition

# Transition (with multiple attributes and timing option)
shuffle <- c(7:8, 1:6)
plot(x[shuffle], y[shuffle], id = id,
     cex = 1:8 * 20,                      # another attribute for transition
     transition = list(duration = 2000))  # 2000ms transition
export(path = "basic_plot.json", handler = 'r')
off()


# Example 2 ----
clear()  # Clear the canvas

set.seed(123)
x <- 1:10
y <- 10 * runif(10)
id <- new_id(y, prefix = "points")   # Give each point an ID
plot(x, y, id = id, bg = "red")

new_y <- 10 * runif(10)
points(x, new_y, id = id, bg = "lightgreen", cex = 1:10 * 30,
       transition = list(duration = 2000))
export(path = "basic_points.json", handler = 'r')


# Example 3 ----
clear()  # Clear the canvas

x <- 1:100
y <- sin(x / 5 * pi / 2)
id <- "line-1"   # a line needs 1 ID only (despite containing multiple points)
plot(x, y, id = id, type = 'l')

for (n in 101:200) {
  new_x <- 1:n
  new_y <- sin(new_x / 5 * pi / 2)
  plot(new_x, new_y, id = id, type = 'l')
  Sys.sleep(0.02)
}
export(path = "basic_lines.json", handler = 'r')

off()
detach(device)


# Improved example 3 ----
device <- animate$new(400, 400)
attach(device)

par(xlim = c(-17, 16), ylim = c(-18, 13))
t <- seq(0, 2*pi, length.out = 150)
x <- 16 * sin(t)^3
y <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)
id <- "line-1"   # a line needs 1 ID only (despite containing multiple points)
for (n in seq_along(t)) {
  plot(x[1:n], y[1:n], id = id, type = 'l')
  Sys.sleep(0.02)
}
export(path = "basic_lines.json", handler = 'r')
R.utils::gzip("basic_lines.json", overwrite = TRUE)

off()
detach(device)

clear()
