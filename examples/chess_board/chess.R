# Plotting functions -----------------------------------------------------------
#' Plot a chess board
#'
#' @description A board is a 8 x 8 grid of "bars"
#' @param size An integer; the size of the board in pixel value.
#' @param margin An integer; the size of the margins in pixel value.
#'
#' @export
board <- function(size, margin) {
  grid <- expand.grid(0:7, 0:7)
  row_colors <- rep(c("rgb(140, 95, 40)", "rgb(230, 200, 170)"), 4)
  bg_colors <- rep(c(row_colors, rev(row_colors)), 4)
  cell_size <- size / 8
  bars(x = margin + cell_size * grid[, 1],
       y = margin + cell_size * grid[, 2],
       w = cell_size, h = cell_size, bg = bg_colors)
}


#' Plot the board coordinate labels
#'
#' @param size An integer; the size of the board in pixel value.
#' @param margin An integer; the size of the margins in pixel value.
#' @param font_size An integer; the size of the text.
#'
#' @export
board_text <- function(size, margin, font_size = 20) {
  cell_size <- size / 8
  text(x = 0, y = margin / 2 + (0:7 + 0.5) * cell_size, labels = 1:8)
  text(y = 0, x = margin / 2 + (0:7 + 0.5) * cell_size, labels = letters[1:8],
       style = list("font-size" = font_size))
}


#' Plot all chess pieces on the board
#'
#' @param size An integer; the size of the board in pixel value.
#' @param margin An integer; the size of the margins in pixel value.
#'
#' @export
board_chess <- function(size, margin) {
  piece_df <- init_pieces()
  for (i in 1:nrow(piece_df)) {
    args <- as.list(piece_df[i, ])
    args$size <- size
    args$margin <- margin
    do.call(new_piece, args)
  }
}


#' Add a chess piece to the board
#'
#' @param id A character string; an id for reference.
#' @param img A character string; the SVG image path corresponding to a chess piece.
#' @param position A character string; the algebraic notation.
#' @param size An integer; the board size.
#' @param margin An integer; the board margin.
#'
#' @examples
#' new_piece("sample", image_of("dark", "n"), "g3", 640, 20)
new_piece <- function(id, img, position, size, margin, ...) {
  image_pos <- an_to_xy(position)
  cell_size <- size / 8
  image(id = id, href = image_fpath(img),
        x = margin + cell_size * image_pos[1],
        y = margin + cell_size * image_pos[2],
        width = cell_size, height = cell_size, ...)
}


#' Create a chess image (file path)
#' @param x A character string. An image file name.
image_fpath <- \(x) base64enc::dataURI(file = "./chess_pieces/Chess_" %+% x,
                                       mime = "image/svg+xml", encoding = "base64")

#' Create a chess image (file name)
#' @param color "dark" or "light".
#' @param piece One of {"r", "n", "b", "q", "k", "p"}.
image_of <- \(color, piece) piece %+% first_char(color) %+% "t45.svg"

first_char <- \(x) substring(x, 1, 1)

`%+%` <- paste0


# Functions for handling the chess position ------------------------------------

# Initial position
init_pieces <- function() {
  # TODO: Allow picking side
  # col = c("white", "black")
  # if (!play_white) col <- rev(col)
  names <- c("rook-1", "knight-1", "bishop-1", "queen", "king",
              "bishop-2", "knight-2", "rook-2", paste("pawn", 1:8, sep = "-"))
  pieces <- c("r", "n", "b", "q", "k", "b", "n", "r", rep("p", 8))

  pieces_df <- data.frame(
    id = c("light-"%+% names, "dark-" %+% names),
    img = c(pieces %+% "lt45.svg", pieces %+% "dt45.svg"),
    pos = c(paste0(letters[1:8], 1), paste0(letters[1:8], 2),
            paste0(letters[1:8], 8), paste0(letters[1:8], 7))
  )
  pieces_df
}

# Convert "algebraic notation" to XY-coordinates
# e.g. "a1" -> (0, 0)
an_to_xy <- function(x) {
  c(x = which(substring(x, 1, 1) == letters) - 1,
    y = as.numeric(substring(x, 2, 2)) - 1)
}

# Convert XY-coordinates to "algebraic notation"
# e.g. (1, 0) -> "a2", (0, 1) -> "b1"
xy_to_an <- function(x) {
  paste0(letters[x[1] + 1], x[2] + 1)
}


# Functions for plotting a game ------------------------------------------------

move_and_update <- function(piece_df, from, to) {
  piece_df <- move(piece_df, from, to)
  update_board(piece_df)
}

update_board <- function(piece_df) {
  for (i in 1:nrow(piece_df)) {
    args <- as.list(piece_df[i, ])
    args$size <- size
    args$margin <- margin
    args$transition <- TRUE
    do.call(new_piece, args)
  }
  piece_df
}

move <- function(piece_df, from, to) {
  ind <- which(piece_df$pos == from)
  is_occupied <- piece_df$pos == to  # Handle capturing of pieces
  piece_df$pos[ind] <- to
  if (any(is_occupied)) {
    remove(piece_df$id[which(is_occupied)])
    return(piece_df[!is_occupied, ])
  }
  piece_df
}


# Main -------------------------------------------------------------------------
# Data
game_1 <- data.frame(rbind(
  c("e2", "e4"), c("e7", "e5"), c("g1", "f3"), c("b8", "c6"), c("f1", "b5"),
  c("g8", "e7"), c("c2", "c3"), c("d7", "d6"), c("d2", "d4"), c("c8", "d7"),
  c("e1", "g1"), c("h1", "f1"), c("e7", "g6"), c("f3", "g5"), c("h7", "h6"),
  c("g5", "f7"), c("e8", "f7"), c("b5", "c4"), c("f7", "e7"), c("d1", "h5"),
  c("d8", "e8"), c("h5", "g5"), c("h6", "g5"), c("c1", "g5")
))
colnames(game_1) <- c("from", "to")
game_1


# Set up the device
library(animate)
size <- 640
margin <- 20   # leave some margin for text
width <- size + 2 * margin
device <- animate$new(width, width)
attach(device)
par(xlim = c(0, width), ylim = c(0, width))


# Set up the board and the pieces
board(size, margin)
board_text(size, margin)
board_chess(size, margin)


# Plot the game
df0 <- init_pieces()
for (i in 1:nrow(game_1)) {
  cur_move <- game_1[i, ]
  df0 <- move_and_update(df0, cur_move$from, cur_move$to)
  Sys.sleep(1)
}


# Clean up
clear()
off()
detach(device)
