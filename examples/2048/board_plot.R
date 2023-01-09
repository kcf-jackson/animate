# Draw the initial board
setup_board <- function(board, w, h) {
  n <- board$n
  par(xlim = c(0, n+1), ylim = c(0, (n+1) * h / w))
  bars(x = 0.9, y = 0.9, n + 0.1, n + 0.1, bg = "#BEAAA0")
  for (row in 1:n) {
    bars(x = 1:n, y = row, rep(0.9, n), rep(0.9, n), bg= "#C8BEB4")
  }
}


# Update the full board
draw_board <- function(board, ...) {
  block_ind <- which(value(board$blocks) != 0)
  for (ind in block_ind) {
    cur_block <- board$blocks[[ind]]
    xy <- as.vector(which(board$position == ind, arr.ind = TRUE))
    if  (length(cur_block$id) > 1) {
        # When two blocks occupy the same block, we want to keep the values 
        # as-is until a new block is created to "cover up" the old blocks. 
        for (bid in cur_block$id) {
            bid <- cur_block$id
            draw_block(xy[2], xy[1], value(cur_block) / 2, bid, ...)
        }    
    } else {
        bid <- cur_block$id
        draw_block(xy[2], xy[1], value(cur_block), bid, ...)
    }
  }
}


# With_bouncing effect
enter_exit_board <- function(board, enter_id, exit_id, ...) {
    for (ind in seq_along(board$blocks)) {
        cur_block <- board$blocks[[ind]]
        bid <- cur_block$id
        if (bid %in% enter_id) {
            xy <- as.vector(which(board$position == ind, arr.ind = TRUE))
            draw_block(xy[2], xy[1], value(cur_block), bid, 
                       w = 0.1, h = 0.1, font_size = "5px",
                       bars_offset = c(0.45, 0.45))
            draw_block(xy[2], xy[1], value(cur_block), bid, 
                       w = 1.1, h = 1.1, 
                       bars_offset = c(-0.1, -0.1),
                       transition = list(duration = 150))
            draw_block(xy[2], xy[1], value(cur_block), bid, 
                       w = 0.9, h = 0.9, transition = list(duration = 100, 
                                                           delay = 150))
        }
    }
    Sys.sleep(0.1)
    for (bid in exit_id) {
        remove(paste0("rect-", bid))
        remove(paste0("text-", bid))
    }
}

# Without bouncing effect
enter_exit_board_no_bounce <- function(board, enter_id, exit_id, ...) {
    for (ind in seq_along(board$blocks)) {
        cur_block <- board$blocks[[ind]]
        bid <- cur_block$id
        if (bid %in% enter_id) {
            xy <- as.vector(which(board$position == ind, arr.ind = TRUE))
            draw_block(xy[2], xy[1], value(cur_block), bid, 
                       w = 0.1, h = 0.1, font_size = "5px",
                       bars_offset = c(0.45, 0.45), 
                       style = list(alpha = 0.1))
            draw_block(xy[2], xy[1], value(cur_block), bid, 
                       w = 0.9, h = 0.9, style = list(alpha = 1),
                       transition = TRUE)
        }
    }
    Sys.sleep(0.1)
    for (bid in exit_id) {
        remove(paste0("rect-", bid))
        remove(paste0("text-", bid))
    }
}

draw_block <- function(x, y, value0, bid, 
                       w = 0.9, h = 0.9, font_size, 
                       bars_offset = c(0, 0),  ...) {
    bars(x = x + bars_offset[1], y = y + bars_offset[2], 
         w, h, bg = value_to_color(value0),
         id = paste0("rect-", bid), ...)
    offset <- value_to_offset(value0)
    text(x = x + offset[1], y = y + offset[2], labels = value0,
         id = paste0("text-", bid),
         style = list("font-size" = ifelse(missing(font_size), 
                                           value_to_size(value0),
                                           font_size),
                      "fill" = value_to_text_color(value0),
                      "font-family" = "Minecraft"),
         ...)
}

value_to_text_color <- function(value) {
    ifelse(value < 8, "#000000", "#EEEEEE")
}

value_to_color <- function(value) {
    c("#F0E6DC", "#F0E1C8", "#F0B478", 
      "#F5915F", "#FA5F3C", "#DC5532", 
      "#EBC86E", "#EBC86E", "#EBC86E",
      "#EBC86E", "#EBC86E")[(log2(value) - 1) %% 11 + 1]
}

value_to_offset <- function(value) {
    if (value == 2) return(c(0.3, 0.29))
    if (value == 4) return(c(0.26, 0.29))
    if (value == 8) return(c(0.3, 0.29))
    if (value == 16) return(c(0.18, 0.29))
    if (value == 32) return(c(0.16, 0.29))
    if (value == 64) return(c(0.14, 0.29))
    if (value == 128) return(c(0.06, 0.29))
    if (value == 256) return(c(0.04, 0.29))
    if (value == 512) return(c(0.08, 0.29))
    if (value == 1024) return(c(0.05, 0.33))
    if (value == 2048) return(c(0.03, 0.33))
}

value_to_size <- function(value) {
    if (value < 10) return("50px")
    if (value < 100) return("48px")
    if (value < 1000) return("46px")
    if (value < 10000) return("34px")
}


# The main game loop
game_loop <- function(board, move) {
    last_board <- board
    board <- slide_board(board, move)
    
    # Animate blocks movements
    draw_board(board, transition = TRUE)
    
    # Merge board
    merged_board <- merge_board(board)
    board <- merged_board$new_board
    enter_id <- merged_board$enter_id
    exit_id <- merged_board$exit_id
    # Animate blocks enter and exit
    enter_exit_board(board, enter_id, exit_id)
    
    # Add a 2 if the position has changed
    if (!identical(last_board, board)) {
        new_board <- add_new_block(board)
        board <- new_board$board
        enter_id <- new_board$enter_id
        # Animate blocks enter
        enter_exit_board_no_bounce(board, enter_id, c())
    }
    
    # Handle end game conditions
    if (is_won(board)) {
        Sys.sleep(0.1)
        end_game(board, win = TRUE)
        off()
        return(board)
    } 
    if (is_lost(board)) {
        Sys.sleep(0.1)
        end_game(board, win = FALSE)
        off()
        return(board)
    }
    
    board
}


# End game
end_game <- function(board, win) {
    win_text <- ifelse(win, "You won!", "You lost!")
    n <- board$n
    fade(bars, 0, 0.73)(0.9, 0.9, n + 0.1, n + 0.1, 
                        id = "end-game-overlay", bg = "#eee4da",
                        transition = list(duration = 1000))
    fade(text, 0, 1)(
        x = -0.3 + n/2 + ifelse(win, 0, -0.11), 
        y = 0.8 + n/2, 
        labels = win_text, 
        id = "end-game-text",
        style = list("font-family" = "Minecraft",
                     "font-size" = "60px",
                     "fill" = "#786E64"),
        transition = list(duration = 1000))
}


# Add keyboard control
add_keyboard_control <- function() {
    event("body", "keypress", function(evt) {
        print(evt$keyCode)
        if (evt$keyCode == 119) move <- "up"
        if (evt$keyCode == 115) move <- "down"
        if (evt$keyCode == 97) move <- "left"
        if (evt$keyCode == 100) move <- "right" 
        board <<- game_loop(board, move)
    })
}


# Add game panel
add_game_panel <- function() {
    text(1, 5.5, "2048 in R",
         style = list("font-size" = "60px",
                      "font-family" = "Minecraft"),
         id = "panel-title")
    text(1, 5.15, "made with 'animate'",
         style = list("font-size" = "32px",
                      "font-family" = "Minecraft"),
         id = "panel-subtitle")
    text(2.775, 5.15, "animate",
         style = list("font-size" = "32px",
                      "font-family" = "Minecraft",
                      "fill" = "#F5915F",
                      "stroke" = "#F5915F"),
         id = "panel-subtitle-2")
}
