# Predator-Prey process

# Helper functions -------------------------------------------------------------
draw <- function(animals) {
  points(animals$x, animals$y, id = animals$id,
         bg = ifelse(animals$type == "rabbit", "pink", "brown"),
         shape = ifelse(animals$vx == 0 & animals$vy == 0, "cross", "circle"),
         xlim = c(0, 1), ylim = c(0, 1), cex = 80)
  # Alternatively, use online images instead. Always check (links and licenses) before use.
  # image(x = animals$x, y = animals$y,
  #       id = animals$id,
  #       xlim = c(0, 1), ylim = c(0, 1),
  #       width = 20, height = 20,
  #       href = ifelse(animals$type == "rabbit",
  #                     "https://iconape.com/wp-content/png_logo_vector/easter-16.png",
  #                     "https://static.vecteezy.com/system/resources/thumbnails/000/546/910/small/fox_007.jpg"))
}

# TODO: Use different colors to reflect the ages
# bg <- purrr::map_chr(1:nrow(animals), ~color(animals[.x, ]))
# color <- function(x) {
#   if (x$type == "rabbit") {
#     if (x$age < 50) return("lightpink")
#     if (x$age < 450) return("pink")
#     return("deeppink")
#   }
#   if (x$type == "fox") {
#     if (x$age < 100) return("orange")
#     if (x$age < 950) return("orangered")
#     return("saddlebrown")
#   }
# }

max_speed <- function(animal) {
  0.5 * ((animal$type == "rabbit") * 0.015 + (animal$type == "fox") * 0.01)
  # Random
  # (animal$type == "rabbit") * runif(1, max = 0.015) +
  #   (animal$type == "fox") * runif(1, max = 0.01)
}


# Main -------------------------------------------------------------------------
library(animate)
device <- animate$new(600, 600, id = "SVG_1")
attach(device)

# Data
set.seed(123)
n <- 50
s <- 0.002
alpha <- 0.5
gamma <- 0.5

animals <- data.frame(
  type = sample(c("rabbit", "fox"), size = n, replace = TRUE,
                prob = c(0.7, 0.3)),
  id = new_id(1:n, "agent"),
  x = runif(n),
  y = runif(n),
  vx = runif(n, -s, s),
  vy = runif(n, -s, s),
  age = sample(150, n, replace = TRUE),
  cycle = sample(10:100, n, replace = TRUE),
  energy = sample(450:550, n, replace = TRUE),
  alive = TRUE
)


# Plot
draw(animals)

# Set up plot area for the count statistics
svg(400, 300, id = "SVG_2", attr = list(style = "left: 600px; top: 30px; position: absolute;"))
set("SVG_1")


# Main Loop
history <- list(n_fox = c(), n_rabbit = c())
time <- 2000
m <- n
step <- 1
while (TRUE) {
  step <- step + 1
  new_animals <- data.frame()
  for (i in 1:nrow(animals)) {
    # Animal specific directional behaviour ----
    run <- FALSE
    if (animals$type[i] == "rabbit") {
      # Run away from nearby foxes
      d <- sqrt((animals$x - animals$x[i])^2 + (animals$y - animals$y[i])^2)
      nearby_fox_ind <- which(animals$type == "fox" & d < 0.025)
      if (length(nearby_fox_ind) > 0) {
        nearby_fox <- animals[nearby_fox_ind, ]
        run_speed <- max_speed(animals[i, ])
        animals$vx[i] <- sign(animals$x[i] - mean(nearby_fox$x)) * run_speed
        animals$vy[i] <- sign(animals$y[i] - mean(nearby_fox$y)) * run_speed
        run <- TRUE
      }
    }
    if (animals$type[i] == "fox") {
      # Chase after the closest rabbit
      d <- sqrt((animals$x - animals$x[i])^2 + (animals$y - animals$y[i])^2)
      closest_rabbit_ind <- which(animals$type == "rabbit" & d < 0.05)
      if (length(closest_rabbit_ind) > 0) {
        d[animals$type == "fox"] <- Inf
        closest_rabbit_ind <- which.min(d)
        closest_rabbit <- animals[closest_rabbit_ind, ]
        run_speed <- max_speed(animals[i, ])
        animals$vx[i] <- sign(closest_rabbit$x - animals$x[i]) * run_speed
        animals$vy[i] <- sign(closest_rabbit$y - animals$y[i]) * run_speed
        run <- TRUE
      }
    }
    # Change direction after certain time ----
    if (!run && (animals$age[i] %% animals$cycle[i] == 0)) {
      animals$vx[i] <- runif(1, -s, s)
      animals$vy[i] <- runif(1, -s, s)
      animals$cycle[i] <- sample(30:100, 1)
    }
    # Check death from old age ----
    if (animals$type[i] == "rabbit") {
      # Hard limit
      if (animals$age[i] > 450) {
        animals$vx[i] <- 0
        animals$vy[i] <- 0
      }
      # Soft limit
      # if (animals$age[i] > 500) {
      #   if (runif(1) < 0.01) animals$alive[i] <- FALSE
      # }
    }
    if (animals$type[i] == "fox") {
      # Hard limit
      # if (animals$age[i] > 950) {
      #   animals$vx[i] <- 0
      #   animals$vy[i] <- 0
      # }
      # Soft limit
      if (animals$age[i] > 1000) {
        if (runif(1) < 0.01) animals$alive[i] <- FALSE
      }
    }
    # Boundary reflection and Update position ----
    next_step_x <- animals$x[i] + animals$vx[i]
    next_step_y <- animals$y[i] + animals$vy[i]
    if (next_step_x < 0 || next_step_x > 1) {
      animals$vx[i] <- animals$vx[i] * -1
    } else {
      animals$x[i] <- next_step_x
    }
    if (next_step_y < 0 || next_step_y > 1) {
      animals$vy[i] <- animals$vy[i] * -1
    } else {
      animals$y[i] <- next_step_y
    }
    animals$age[i] <- animals$age[i] + 1
    # Check death from collision ----
    if (animals$type[i] == "rabbit") {
      if (any(d[animals$type == "fox"] < 0.01)) {
        fox_ind <- which.min(d[animals$type == "fox"])
        animals[animals$type == "fox", ][fox_ind, ]$energy <-
          animals[animals$type == "fox", ][fox_ind, ]$energy + 200
        animals$alive[i] <- FALSE
      }
    }
    # Check death from energy
    if (animals$type[i] == "fox") {
      animals$energy[i] <- animals$energy[i] - 1
      if (animals$energy[i] <= 0) {
        animals$alive[i] <- FALSE
      }
    }
    # Reproduce ----
    if ((animals$type[i] == "rabbit" && animals$age[i] %% 150 == 0) ||
        (animals$type[i] == "fox" && animals$age[i] %% 300 == 0)) {
      if (animals$alive[i] && animals$vx[i] > 0) {# hot fix for immobile aged rabbit
        if ((animals$type[i] == "rabbit" && runif(1) < 1) ||
          (animals$type[i] == "fox" && runif(1) < 1)) {
          offspring <- animals[i, ]
          m <- m + 1
          offspring$id <- new_id(m, "agent")
          offspring$vx = runif(1, -s, s)
          offspring$vy = runif(1, -s, s)
          offspring$age = 0
          offspring$cycle = sample(10:100, 1)
          offspring$energy = 500
          new_animals <- rbind(new_animals, offspring)
        }
      }
    }
  }
  # Update death ----
  remove("path.points", id = animals$id[!animals$alive])
  # remove("image", id = animals$id[!animals$alive])
  animals <- animals[animals$alive, ]

  # Update birth ----
  animals <- rbind(animals, new_animals)

  n_rabbit <- sum(animals$type == "rabbit" & animals$vx != 0)
  n_fox <- sum(animals$type == "fox")
  text(x = 0, y = 1,
       labels = paste0("Time:", step, ". Rabbit:", n_rabbit, ". Fox:", n_fox, "."),
       id = "note", xlim = c(0, 1), ylim = c(0, 1))
  draw(animals)

  # Statistics plot ----
  set("SVG_2")
  history$n_fox <- c(history$n_fox, n_fox)
  history$n_rabbit <- c(history$n_rabbit, n_rabbit)
  history_x <- seq_along(history$n_fox)
  plot(x = history_x, y = history$n_fox, id = "n-fox-lines",
       xlim = c(0, step), ylim = c(0, max(unlist(history))), type = 'l',
       col = "brown")
  lines(x = history_x, y = history$n_rabbit, id = "n_rabbit-lines",
       xlim = c(0, step), ylim = c(0, max(unlist(history))), type = 'l',
       col = "pink")
  set("SVG_1")
  Sys.sleep(0.05)
}
