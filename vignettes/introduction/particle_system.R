# device <- animate$new(500, 500)
# attach(device)

# Number of particles
n <- 50

# Data of the particles
ps <- list(
  # Position
  x = runif(n),
  y = runif(n),
  # Velocity
  vx = rnorm(n) * 0.01,
  vy = rnorm(n) * 0.01,
  # Class
  color = sample(c("black", "red"), n, replace = TRUE),
  # ID
  id = new_id(x)
)

# Simulation of the evolution of one time step
update_one_step <- function(ps) {
  # Turns around when the particle hits the boundary
  x_turn <- ps$x + ps$vx > 1 | ps$x + ps$vx < 0
  ps$vx[x_turn] <- ps$vx[x_turn] * -1

  y_turn <- ps$y + ps$vy > 1 | ps$y + ps$vy < 0
  ps$vy[y_turn] <- ps$vy[y_turn] * -1

  # Update position
  ps$x <- ps$x + ps$vx
  ps$y <- ps$y + ps$vy
  ps
}

# Visualising the system
par(xlim = c(0, 1), ylim = c(0, 1))
for (t in 1:1000) {
  points(ps$x, ps$y, id = ps$id, bg = ps$color)
  ps <- update_one_step(ps)
  Sys.sleep(0.02)
}

# export("particle_system.json", handler = 'r')
# R.utils::gzip("particle_system.json")
# off()
# detach(device)
