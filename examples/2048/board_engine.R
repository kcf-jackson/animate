# Helpers -----------------------------------------------------------------
block <- \(value, id) structure(list(value = value, id = id), class = "block")
is_block <- \(x) class(x) == "block"
is_block_list <- \(xs) all(sapply(xs, is_block))

broadcast <- \(f) \(x) if (is_block_list(x)) sapply(x, f) else f(x)
value <- broadcast(\(x) x$value)
id <- broadcast(\(x) x$id)
prepend <- \(v, xs) c(list(v), xs)


merge_with_id <- function(xs) {
  merged <- combine_with_id(xs[which(value(xs) > 0)])
  padding <- rep(list(block(0, -1)), length(xs) - length(merged))
  c(merged, padding)
}

combine_with_id <- function(inp) {
  if (length(inp) < 2)
    return(inp)

  x <- inp[[1]]
  y <- inp[[2]]
  xs <- inp[-(1:2)]

  if (value(x) == value(y))
    return(prepend(block(value(x) * 2, c(id(x), id(y))),
                   combine_with_id(xs)))

  prepend(x, combine_with_id(prepend(y, xs)))
}

# Test cases
#
# all2 <- \(xs, f) all(sapply(xs, f))
# equal_block <- \(xs, ys) all2(names(xs), \(key) identical(xs[key], ys[key]))
# equal_blocks <- \(xs, ys) (length(xs) == length(ys)) &&
#   all2(seq_along(xs), \(ind) equal_block(xs[ind], ys[ind]))
#
# equal_blocks(
#   combine_with_id(list(block(2, 1), block(2, 2), block(0, -1), block(0, -1))),
#   list(block(4, c(1,2)), block(4, c(3,4)))
# )
# equal_blocks(
#   combine_with_id(list(block(2, 1), block(2, 2), block(2, 3), block(0, -1))),
#   list(block(4, c(1,2)), block(2, 3), block(0, -1))
# )
# equal_blocks(
#   combine_with_id(list(block(2, 1), block(2, 2), block(2, 3), block(2, 4))),
#   list(block(4, c(1,2)), block(4, c(3, 4)))
# )


# Board functions -----------------------------------------------------------------
# Initialise the board
init_board <- function(n = 4) {
  # Set up n^2 blocks
  blocks <- lapply(1:n^2, \(bid) block(value = 0, id = bid))

  # Randomly pick two blocks and give them the starting value 2
  for (i in sample(n^2, 2)) {
    blocks[[i]]$value <- 2
  }

  # Return the board
  list(position = matrix(1:n^2, n, n),
       blocks = blocks,
       n = n)
}


# Print the board in plain text
print_board <- function(board, show = TRUE) {
  n <- board$n
  values <- value(board$blocks)
  positions <- as.vector(board$position)
  m <- matrix(values[positions], nrow = n, ncol = n)
  result <- m[nrow(m):1, ]
  if (show) print(result)
  result
}


# Perform a slide on the board
slide_board <- function(board, move) {
  if (move == "up") {
    board$position <- rotate_180(board$position)
    board <- update_board(board)
    board$position <- rotate_180(board$position)
  }
  if (move == "left") {
    board$position <- rotate_90(board$position)
    board <- update_board(board)
    board$position <- rotate_270(board$position)
  }
  if (move == "down") {
    board <- update_board(board)
  }
  if (move == "right") {
    board$position <- rotate_270(board$position)
    board <- update_board(board)
    board$position <- rotate_90(board$position)
  }
  board
}

update_board <- function(board) {
  for (j in 1:board$n) {
    column_j_ind <- board$position[, j]
    board$blocks[column_j_ind] <- merge_with_id(board$blocks[column_j_ind])
  }
  board
}

rotate_90 <- \(m0) t(m0)[, ncol(m0):1]
rotate_180 <- \(m0) rotate_90(rotate_90(m0))
rotate_270 <- \(m0) rotate_180(rotate_90(m0))


# Merge overlapping blocks after the slide
merge_board <- function(board) {
    enter_id <- c()
    exit_id <- c()
    new_board <- board
    new_id <- create_new_id(board)
    for (ind in seq_along(board$blocks)) {
        block <- board$blocks[[ind]]
        if (length(block$id) > 1) {
            enter_id <- c(enter_id, new_id)
            exit_id <- c(exit_id, block$id)
            new_board$blocks[[ind]]$id <- new_id
            new_id <- new_id + 1
        }
    }
    list(board = board,
         enter_id = enter_id,
         exit_id = exit_id,
         new_board = new_board)
}


# Add a new block on the board
add_new_block <- function(board) {
  empty_ind <- which(value(board$blocks) == 0)
  if (length(empty_ind) > 0) {
    if (length(empty_ind) == 1) {
      ind <- empty_ind
    } else {
      ind <- sample(empty_ind, 1)
    }
    new_id <- create_new_id(board)
    board$blocks[[ind]]$id <- new_id
    board$blocks[[ind]]$value <- 2
  }
  list(board = board, enter_id = new_id)
}

create_new_id <- function(board) {
  max(unlist(lapply(board$blocks, id))) + 1
}


# Check end game
is_won <- function(board) {
    any(value(board$blocks) == 2048)
}

is_lost <- function(board) {
    board_value <- print_board(board, show = FALSE)
    all(board_value != 0) &&
        all(apply(board_value, 1, diff) != 0) &&
        all(apply(board_value, 2, diff) != 0)
}
