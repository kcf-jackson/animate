# The game design here is based on `2048_simple.R`; see more information in that file.
# The "Minecraft" font is by NubeFonts @ https://www.fontspace.com/minecraft-ten-font-f40317
source("board_engine.R")
source("board_plot.R")
source("animate_effect.R")
library(animate)
w <- 600
h <- 700
device <- animate$new(w, h)  # wait for this line to finish
attach(device)

board <- init_board(4)
print_board(board)

setup_board(board, w, h)
draw_board(board)
add_game_panel()

# Use only one of the two options below
# 1. Use browser keyboard input
add_keyboard_control()

# 2. Use R terminal input
# while (TRUE) {
#     print_board(board)
#     move <- readline("Move?")
#     key_mapping <- list("w" = "up", "a" = "left", "s" = "down", "d" = "right")
#     board <- game_loop(board, key_mapping[[move]])
# }
