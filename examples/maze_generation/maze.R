library(animate)
source("stack.R")
device <- animate$new(680, 340)
attach(device)

# Cells functions --------------------------------------------------------------
cell <- function(id, i, j, walls = rep(1, 4), visited = FALSE) {
  list(id = id, x = i, y = j, walls = walls, visited = visited)
}

cell_color <- \(x) ifelse(x, "green", "white")

draw_cells <- function(x, y, id, color) {
  bars(x = x, y = y, w = 1, h = 1, id = id, bg = color)
}


# Wall functions ---------------------------------------------------------------
wall_color <- \(x) ifelse(x, "black", "green")

draw_walls <- function(x, y, id, walls) {
  `%+%` <- paste0
  cw1 <- sapply(walls[[1]], wall_color)
  cw2 <- sapply(walls[[2]], wall_color)
  cw3 <- sapply(walls[[3]], wall_color)
  cw4 <- sapply(walls[[4]], wall_color)
  bars(  x, y+1, 1.1, 0.1, bg = cw1, id = "top-" %+% id)
  bars(x+1,   y, 0.1, 1.1, bg = cw2, id = "right-" %+% id)
  bars(  x,   y, 1.1, 0.1, bg = cw3, id = "bottom-" %+% id)
  bars(  x,   y, 0.1, 1.1, bg = cw4, id = "left-" %+% id)
}


# Maze functions ---------------------------------------------------------------
generate_maze <- function(n = 5, m = 10) {
  mat0 <- expand.grid(1:m, 1:n)
  cells <- lapply(1:nrow(mat0), \(i) cell(i, mat0[i, 1], mat0[i, 2]))
  cells[[1]]$visited <- TRUE

  # New implementation
  stack <- Stack()
  stack$push(cells[[1]])
  while (!stack$is_empty()) {
    # Plot the current cell
    current_cell <- stack$pop()
    visualise_maze(cells, active_cell_id = current_cell$id)
    Sys.sleep(0.1)  # needed if developing in the R console

    # Given the current cell, find the neighbours that have not been visited
    cid <- current_cell$id
    ids <- get_neighbors_id(cid, n, m)
    not_visited <- ids |>
      sapply(\(ind) if (is.na(ind)) NA else !cells[[ind]]$visited) |>
      which()

    # If there is at least one unvisited neighbour
    if (length(not_visited) >= 1) {
      # Push the current cell to the stack
      stack$push(current_cell)

      # Choose one of the unvisited neighbours
      pos <- ifelse(length(not_visited) == 1,
                    not_visited,
                    sample(not_visited, 1))
      chosen_id <- ids[pos]

      # Remove the walls between the current cell and the chosen cell.
      # Note that there are two walls to be removed, one is to exit
      # the current cell, the other is to enter the chosen cell.
      cells[[cid]]$walls[pos] <- 0

      rev_pos <- which(get_neighbors_id(chosen_id, n, m) == cid)
      cells[[chosen_id]]$walls[rev_pos] <- 0

      # Mark the chosen cell as visited and push it to the stack
      cells[[chosen_id]]$visited <- TRUE
      stack$push(cells[[chosen_id]])
    }
  }
  invisible(cells)
}

visualise_maze <- function(cells, active_cell_id) {
  # Collect the data from the cells for plotting
  cx  <- sapply(cells, \(cell) cell$x)
  cy  <- sapply(cells, \(cell) cell$y)
  cid <- sapply(cells, \(cell) cell$id)
  cbg <- sapply(cells, \(cell) cell_color(cell$visited))
  cwalls <- lapply(1:4, \(wid) sapply(cells, \(cell) cell$walls[wid]))

  if (!missing(active_cell_id)) {
    cbg[which(cid == active_cell_id)] <- "blue"
  }

  # Plot the cells
  draw_cells(cx, cy, cid, cbg)
  draw_walls(cx, cy, cid, cwalls)
}

get_neighbors_id <- function(id, n, m) {
  # Set the value to be NA if the (x,y) coordinates go beyond the grid size
  clip <- function(coord) {
    x <- coord[1]
    y <- coord[2]
    c(ifelse(x > 0 && x <= m, x, NA),
      ifelse(y > 0 && y <= n, y, NA))
  }

  # Given an id, retrieve the (x,y) coordinates
  id_to_xy <- function(id) {
    c(x = 1 + (id - 1) %% m, y = 1 + (id - 1) %/% m)
  }

  # Convert the (x,y) coordinates to an id
  # - Return NA if any of the coordinates contains NA, indicating that the cell is outside of the grid.
  xy_to_id <- function(coord) {
    if (any(is.na(coord))) NA else (coord[2] - 1) * m + coord[1]
  }

  xy <- id_to_xy(id)
  c(top    = xy_to_id(clip(xy + c(0, 1))),
    right  = xy_to_id(clip(xy + c(1, 0))),
    bottom = xy_to_id(clip(xy + c(0, -1))),
    left   = xy_to_id(clip(xy + c(-1, 0))))
}


# Main -------------------------------------------------------------------------
n <- 10
m <- 20
par(xlim = c(1, m+1), ylim = c(1, n+1))
generate_maze(n, m)
off()
detach(device)
