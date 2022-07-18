particle_sim <- function(num_particles = 50) {
  # Particles move within the unit box
  x <- runif(num_particles)
  y <- runif(num_particles)
  vx <- rnorm(num_particles) * 0.01
  vy <- rnorm(num_particles) * 0.01
  id <- new_id(x)
  color <- sample(c("black", "red"), num_particles, replace = TRUE, prob = c(0.5, 0.5))

  env <- environment()
  step <- function(n = 1) {
    for (i in 1:n) {
      evalq(envir = env, {
        # The particles turn around when they hit the boundary of the box
        x_turn <- x + vx > 1 | x + vx < 0
        vx[x_turn] <- vx[x_turn] * -1

        y_turn <- y + vy > 1 | y + vy < 0
        vy[y_turn] <- vy[y_turn] * -1

        x <- x + vx
        y <- y + vy
      })
    }
  }

  env
}

device <- animate$new(500, 500)
attach(device)
world <- particle_sim(num_particles = 50)
for (i in 1:400) {
  points(world$x, world$y, id = world$id, bg = world$color,
         xlim = c(0, 1), ylim = c(0, 1))
  world$step()
  Sys.sleep(0.02)
}
device$export("vignettes/source/particle_system.json", handler = "r")
off()
detach(device)
