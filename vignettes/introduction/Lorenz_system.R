# device <- animate$new(500, 300)
# attach(device)

# Lorenz system parameters
sigma <- 10
beta <- 8/3
rho <- 28

# Set up for the Euler method
x <- y <- z <- 1
dx <- dy <- dz <- 0
xs <- x
ys <- y
zs <- z
time_steps <- 1:2000
dt <- 0.015

# Frame-by-frame animation with a for loop
for (t in time_steps) {
  dx <- sigma * (y - x) * dt
  dy <- (x * (rho - z) - y) * dt
  dz <- (x * y - beta * z) * dt
  x <- x + dx
  y <- y + dy
  z <- z + dz
  xs <- c(xs, x)
  ys <- c(ys, y)
  zs <- c(zs, z)

  # `animate` plot
  par(xlim = c(-30, 30), ylim = c(-30, 40))
  plot(x, y, id = "ID-1")
  lines(xs, ys, id = "lines-1")
  Sys.sleep(0.025)
}

# Special transition to x-z plane to show the 'bufferfly'
par(xlim = c(-30, 30), ylim = range(zs))
plot(x, z, id = "ID-1", transition = TRUE)
lines(xs, zs, id = "lines-1", transition = TRUE)

# export("Lorenz_system.json", handler = 'r')
# R.utils::gzip("Lorenz_system.json")
# off()
# detach(device)
