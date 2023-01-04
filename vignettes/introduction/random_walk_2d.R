random_walk <- function(n_steps) {
  steps <- sample(
    list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)),
    n_steps, replace = T
  )
  coord <- do.call(rbind, steps)
  x <- c(0, cumsum(coord[,1]))
  y <- c(0, cumsum(coord[,2]))
  data.frame(x = x, y = y)
}


library(animate)
device <- animate$new(height = 500, width = 500)
attach(device)

set.seed(20230105)
n_steps <- 250
n_walkers <- 3
color <- c("black", "orange", "blue", "green", "red")
walkers <- lapply(1:n_walkers, function(ind) random_walk(n_steps))

# Use static range
xlim <- range(do.call(rbind, walkers)$x)
ylim <- range(do.call(rbind, walkers)$y)
par(xlim = xlim, ylim = ylim)

for (i in 1:n_steps) {
  for (ind in 1:n_walkers) {
    x <- walkers[[ind]]$x
    y <- walkers[[ind]]$y
    plot(x[1:i], y[1:i], type = 'l', id = paste0("line-", ind), col = color[ind])
    points(x[i], y[i], id = paste0("point-", ind), bg = color[ind])
  }
  Sys.sleep(0.02)
}

# export("random_walk_2d.json", handler = "r")
# R.utils::gzip("random_walk_2d.json")
# off()
# detach(device)
