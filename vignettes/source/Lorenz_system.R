# Define the simulation system
Lorenz_sim <- function(sigma = 10, beta = 8/3, rho = 28, x = 1, y = 1, z = 1, dt = 0.015) {
  # Auxiliary variables
  dx <- dy <- dz <- 0
  xs <- x
  ys <- y
  zs <- z
  env <- environment() # a neat way to capture all the variables

  # Update the variables using the ODE within 'env'
  step <- function(n = 1) {
    for (i in 1:n) {
      evalq(envir = env, {
        dx <- sigma * (y - x) * dt
        dy <- (x * (rho - z) - y) * dt
        dz <- (x * y - beta * z) * dt
        x <- x + dx
        y <- y + dy
        z <- z + dz
        xs <- c(xs, x)
        ys <- c(ys, y)
        zs <- c(zs, z)
      })
    }
  }

  env
}

device <- animate$new(600, 400)
attach(device)
world <- Lorenz_sim()
for (i in 1:1500) {
  plot(world$x, world$y, id = "ID-1", xlim = c(-30, 30), ylim = c(-30, 40))
  lines(world$xs, world$ys, id = "lines-1", xlim = c(-30, 30), ylim = c(-30, 40))
  world$step()
  Sys.sleep(0.025)
}
# Switch to xz-plane
plot(world$x, world$z, id = "ID-1", xlim = c(-30, 30), ylim = range(world$zs), transition = TRUE)
lines(world$xs, world$zs, id = "lines-1", xlim = c(-30, 30), ylim = range(world$zs), transition = TRUE)
device$export("vignettes/source/Lorenz_system.json", handler = 'r')
off()
detach(device)
