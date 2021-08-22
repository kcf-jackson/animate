library(animate)
device <- animate$new(600, 400)
attach(device)

# Example 1
x <- 1:10
y <- 1:10
id <- new_id(x)   # Give each point an ID
plot(x, y, id = id)

new_y <- 10:1
plot(x, new_y, id = id, transition = TRUE)
export(path = "vignettes/source/basic_plot.json", handler = 'r')


# Example 2
clear()  # Clear the canvas

set.seed(123)
x <- 1:10
y <- 10 * runif(10)
id <- new_id(y, prefix = "points")   # Give each point an ID
plot(x, y, id = id, bg = "red")

new_y <- 10 * runif(10)
points(x, new_y, id = id, bg = "lightgreen", cex = 1:10 * 30,
       transition = list(duration = 2000))
export(path = "vignettes/source/basic_points.json", handler = 'r')


# Example 3
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
export(path = "vignettes/source/basic_lines.json", handler = 'r')

off()
detach(device)
