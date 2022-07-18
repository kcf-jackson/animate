random_walk_sim <- function(grid_size = 20, num_walkers = 10) {
  .side <- seq(0, 1, length.out = grid_size)
  grid <- expand.grid(.side, .side)
  id <- paste("ID", 1:grid_size^2, sep = "-")

  .index_to_coord <- function(n) c(ceiling(n / grid_size), (n-1) %% grid_size + 1)
  .coord_to_index <- function(x) (x[1] - 1) * grid_size + x[2]
  .step <- function(coord) {
    k <- sample(list(c(-1,0), c(1,0), c(0,-1), c(0,1)), 1)[[1]]
    (coord + k - 1) %% grid_size + 1
  }

  .walkers_index <- sample(grid_size^2, num_walkers)
  .walkers_coord <- Map(.index_to_coord, .walkers_index)
  .walkers_color <- sample(c("red", "green", "blue", "black", "orange"), num_walkers, replace = TRUE)
  color <- rep("lightgrey", grid_size^2)
  color[.walkers_index] <- .walkers_color

  env <- environment()
  step <- function() {
    evalq(envir = env, expr = {
      # Update each walker's coordinate and change the color state
      .walkers_coord <- Map(.step, .walkers_coord)
      .walkers_index <- unlist(Map(.coord_to_index, .walkers_coord))
      color <- rep("lightgrey", grid_size^2)
      color[.walkers_index] <- .walkers_color
    })
  }

  env
}


device <- animate$new(500, 500)
attach(device)
set.seed(123)
world <- random_walk_sim(grid_size = 15, num_walkers = 8)
for (i in 1:100) {
  coord <- world$grid
  points(coord[,1], coord[,2], id = world$id, bg = world$color, pch = "square", cex = 950, col = "black")
  world$step()
  Sys.sleep(0.3)
}
export("vignettes/source/random_walk_2d.json", handler = "r")
off()
detach(device)
