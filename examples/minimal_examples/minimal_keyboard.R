# A minimal example showing how keyboard event works

library(animate)
device <- animate$new(600, 600)
attach(device)

par(xlim = c(0, 10), ylim = c(0, 10))
x <- 5
y <- 5
plot(x, y, id = "id-1")

event("body", "keypress", function(evt) {
  if (evt$keyCode == 119) y <<- y + 1  # W
  if (evt$keyCode == 97)  x <<- x - 1  # A
  if (evt$keyCode == 115) y <<- y - 1  # S
  if (evt$keyCode == 100) x <<- x + 1  # D
  plot(x, y, id = "id-1")
})
