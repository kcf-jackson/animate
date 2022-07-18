library(animate)

# Simulate particle movements within the unit box
num_particles <- 50

# Set up the particles
x <- runif(num_particles)
y <- runif(num_particles)
vx <- rnorm(num_particles) * 0.01
vy <- rnorm(num_particles) * 0.01
id <- new_id(x)
color <- sample(c("black", "red"), num_particles, replace = TRUE, prob = c(0.5, 0.5))

# Set up the device
device <- animate$new(500, 500)
attach(device)
par(xlim = c(0, 1), ylim = c(0, 1))

# Main
time_steps <- 400
for (i in 1:time_steps) {
    # The particles turn around when they hit the boundary of the box
    x_turn <- x + vx > 1 | x + vx < 0
    vx[x_turn] <- vx[x_turn] * -1
    y_turn <- y + vy > 1 | y + vy < 0
    vy[y_turn] <- vy[y_turn] * -1
    x <- x + vx
    y <- y + vy

    # Plotting
    points(x, y, id = id, bg = color)
    Sys.sleep(0.02)
}

# Clean up
off()
detach(device)
