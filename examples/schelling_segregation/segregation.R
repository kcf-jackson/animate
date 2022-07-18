# Helpers ----------------------------------------------------------------------
check_happy <- function(grid, k = 1, threshold = 0.5) {
  mat <- grid_to_matrix(grid)
  for (i in 1:nrow(grid)) {
    if (grid$race[i] == "empty") {
      grid$happy[i] <- NA
    } else {
      grid$happy[i] <- is_happy(mat, grid$x[i], grid$y[i], k, threshold)
    }
  }
  grid
}

grid_to_matrix <- function(grid) {
  n <- sqrt(nrow(grid))
  mat <- matrix(nrow = n, ncol = n)
  for (i in 1:nrow(grid)) {
     mat[grid$x[i], grid$y[i]] <- grid$race[i]
  }
  mat
}

# A cell is happy when the proportion of neighbours of its own kind is above
# certain threshold
is_happy <- function(grid_mat, x, y, k = 1, threshold = 0.5) {
  clip <- \(x) min(max(x, 1), n)
  neighbours <- grid_mat[clip(x - k):clip(x + k), clip(y - k): clip(y + k)]
  sum(neighbours == grid_mat[x, y]) / length(neighbours) > threshold
}


# A cell moves to a new location if it is not happy
move <- function(grid, show_progress = FALSE, delay = 0.2) {
  for (i in 1:nrow(grid)) {
    if (!is.na(grid$happy[i]) && !grid$happy[i]) {
      empty_space <- sample(which(grid$race == "empty"), 1)
      empty_space_id <- grid$id[empty_space]
      # Move agent to the new position
      grid$id[empty_space] <- grid$id[i]
      grid$race[empty_space] <- grid$race[i]
      # Remove agent in the old position
      grid$id[i] <- empty_space_id
      grid$race[i] <- "empty"
      grid$happy[i] <- NA
    }
    if (show_progress) {
      draw_grid(grid)
      Sys.sleep(delay)
    }
  }
  grid
}

draw_grid <- function(grid, duration = 250) {
  grid <- sort_non_empty_id(grid)
  points(x = grid$x, y = grid$y, id = grid$id,
         pch = "square", cex = 150, bg = grid$race,
         transition = list(duration = duration))
}

sort_non_empty_id <- function(grid) {
  grid |> filter(race != "empty") |> arrange(id)
}


# Statistics
segregation <- function(grid) {
  sum(grid$happy, na.rm = T) / sum(!is.na(grid$happy))
}


# Main -------------------------------------------------------------------------
# Set up device
library(animate)
device <- animate$new(500, 500, id = "SVG_1")
attach(device)

# Set up simulation
library(dplyr)
n <- 30
threshold <- 0.5
step <- 10

# Data
grid <- data.frame(expand.grid(x = 1:n, y = 1:n)) |>
  mutate(id = new_id(1:n^2, prefix = "agent"),
         race = sample(c("red", "blue", "empty"),
                       prob = c(0.45, 0.45, 0.1),
                       size = n^2, replace = TRUE),
         happy = NA)
draw_grid(grid)
grid <- grid |> check_happy(threshold = threshold)

# Optional statistics plot
svg(400, 250, id = "SVG_2", root = "body",
    attr = list(style = "top: 30px; left: 450px; position: absolute;"))
par(mai = c(0.15, 0.25, 0.1, 0.05))
y <- segregation(grid)
print(y)
set("SVG_1")
par(mai = rep(0.082, 4))

# Main loop
for (time in 1:step) {
  grid <- grid |>
    move() |>
    check_happy(threshold = threshold)
  draw_grid(grid)

  # Optional statistics plot
  y <- c(y, segregation(grid))
  set("SVG_2")
  par(mai = c(0.15, 0.25, 0.1, 0.05))
  plot(seq_along(y), y, id = new_id(y), type = 'l',
       xlab = "time step", ylab = "Segregation measure",
       xlim = c(1, step + 1), ylim = c(0.3, 1))
  points(seq_along(y), y, id = new_id(y, "points"),
       xlim = c(1, step + 1), ylim = c(0.3, 1))
  set("SVG_1")
  par(mai = rep(0.082, 4))

  Sys.sleep(1)
}

# Clean up
off()
detach(device)
