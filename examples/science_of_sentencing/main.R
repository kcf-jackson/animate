library(animate)
device <- animate$new(1000, 500)
attach(device)

source("background.R")
source("data.R")
source("stages.R")
source("layout.R")
source("simulation.R")

set.seed(123)
run_sim(18, steps = 20)
