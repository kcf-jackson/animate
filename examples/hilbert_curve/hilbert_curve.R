# Helpers ----------------------------------------------------------------------
shift_x <- function(m0, k) { m0[,1] <- m0[,1] + k; return(m0); }

shift_y <- function(m0, k) { m0[,2] <- m0[,2] + k; return(m0); }

flip_45 <-  function(m0) {
  x_ref_origin <- m0[, 1] - min(m0[, 1])
  y_ref_origin <- m0[, 2] - min(m0[, 2])
  new_x <- min(m0[, 1]) + y_ref_origin
  new_y <- min(m0[, 2]) + x_ref_origin
  cbind(new_x, new_y)
}

flip_n45 <-  function(m0) {
  x_ref_origin <- m0[, 1] - min(m0[, 1])
  y_ref_origin <- m0[, 2] - min(m0[, 2])
  new_x <- min(m0[, 1]) - y_ref_origin + diff(range(m0[,2]))
  new_y <- min(m0[, 2]) - x_ref_origin + diff(range(m0[,1]))
  cbind(new_x, new_y)
}

# Compute the Hilbert curve
hilbert <- function(n) {
  if (n == 1) {
    return(cbind(c(0.25, 0.25, 0.75, 0.75),
                 c(0.25, 0.75, 0.75, 0.25)))
  }
  if (n >= 2) {
    k <- 0.5
    bottom_left <- hilbert(n-1) * 0.5
    top_left <- shift_y(bottom_left, k)
    top_right <- shift_x(top_left, k)
    bottom_right <- shift_x(bottom_left, k)
    return(rbind(flip_45(bottom_left), top_left, top_right, flip_n45(bottom_right)))
  }
}

# Plot the Hilbert curve
draw_hilbert <- function(mat) {
  # Set a fixed range
  par(xlim = c(0,1), ylim = c(0,1))

  # Loop over the data points
  for (i in 1:nrow(mat)) {
    pts <- mat[i, ]

    # Draw the point
    points(pts[1], pts[2], cex = 50)

    # Connect to the previous point with a line
    if (i != 1) {
      last <- mat[i-1, ]
      lines(c(pts[1], last[1]), c(pts[2], last[2]))
    }

    Sys.sleep(0.01)
  }
}


# Main -------------------------------------------------------------------------
# Set up
library(animate)
device <- animate$new(600, 600)
attach(device)

# Part 1 - Compute and plot the Hilbert curve
mat <- hilbert(4)
draw_hilbert(mat)
clear()  # Clean up for the next part


# Part 2 - Animating the transition from the low order to the high order
rep_mat <- function(mat, n) {
  matrix(rep(mat, each = n), nrow = nrow(mat) * n)
}

par(xlim = c(0,1), ylim = c(0,1))
max_order <- 6
pts <- rep_mat(hilbert(1), 4^(max_order - 1))
lines(pts[,1], pts[,2], id = "line-1")
for (i in 2:max_order) {
  pts <- rep_mat(hilbert(i), 4^(max_order - i))
  lines(pts[,1], pts[,2], id = "line-1", transition = list(duration = 2000))
  Sys.sleep(2)
}

# Clean up
off()
detach(device)
