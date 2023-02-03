library(animate)

w <- 600
h <- 400
device <- animate$new(w, h)
attach(device)

par(xlim = c(0, w), ylim = c(0, h))

# Background image
image(href = "https://interaktiv.morgenpost.de/berlin-marathon-2016/images/track2.svg",
      x = 0, y = 0, width = w, height = h, id = "background")

# Finishing time
ftime <- data.frame(
  labels = c('100%', '90%', '80%', '70%', '60%', '50%', '40%', '30%', '20%', '10%', '0%'),
  time = c('02:03:03', '03:16:22', '03:33:11', '03:46:07', '03:56:42', '04:07:14', '04:19:32',
          '04:33:24', '04:50:44', '05:16:27', '08:41:33'),
  x = rep(0, 11),
  y = h - seq(10, by = 16, length.out = 11)
)
for (i in 1:nrow(ftime)) {
  text(ftime$x[i], ftime$y[i], ftime$labels[i], id = paste0("qtext-", i))
  text(ftime$x[i] + 60, ftime$y[i], ftime$time[i], id = paste0("ttext-", i))
}

# Movement
as_time <- \(x) {
  sprintf("%.2d:%.2d:%.2d", x %/% 3600, (x %% 3600) %/% 60, (x %% 60))
}

# Data
load("marathon_berlin", verbose = TRUE)
num_athletes <- length(x)
num_timepoints <- length(x[[1]])
for (t in 1:num_timepoints) {
  for (i in 1:num_athletes) {
    points(x[[i]][t], h - y[[i]][t], id = paste0("athlete-", i))
    text(0, 0, as_time(round(t * 31293 / 8751)), id = "timer",
         style = list("font-size" = "24px"))
  }
  Sys.sleep(0.01)
}

clear()
off()
detach(device)
