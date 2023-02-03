# References
# [1] https://gregorulm.com/2048-in-90-lines-haskell/
# [2] https://gist.github.com/coolbutuseless/ffc5eef475f4c514731fb72d3d813a2d

# The `merge` and `combine` functions are from Gregor Ulm [1]
merge <- function(xs) {
  merged <- combine(xs[xs > 0])
  padding <- rep(0, length(xs) - length(merged))
  c(merged, padding)
}
# # Test cases
# equal <- \(x, y) all(x == y)
# equal(merge(c(2, 2, 0, 0)), c(4, 0, 0, 0))
# equal(merge(c(2, 2, 2, 0)), c(4, 2, 0, 0))
# equal(merge(c(2, 2, 2, 2)), c(4, 4, 0, 0))


combine <- function(inp) {
  if (length(inp) < 2)
    return(inp)

  x <- inp[1]
  y <- inp[2]
  xs <- inp[-(1:2)]

  if (x == y)
    return(c(x * 2, combine(xs)))

  c(x, combine(c(y, xs)))
}
# # Test cases
# equal <- \(x, y) all(x == y)
# equal(combine(c(2, 2, 0, 0)), c(4, 0))
# equal(combine(c(2, 2, 2, 0)), c(4, 2, 0))
# equal(combine(c(2, 2, 2, 2)), c(4, 4))
# equal(combine(c(2, 4, 4, 2)), c(2, 8, 2))
# equal(combine(c(4, 2, 4, 2)), c(4, 2, 4, 2))


# The game loop is from Mike @ coolbutuseless [2]
main <- function(n = 4) {
  # Initialisation
  m <- matrix(0, nrow = n, ncol = n)
  m[sample(n^2, 2)] <- sample(c(2, 4), 2)
  m_prior <- m
  print(m)

  while (max(m) < 2048 && any(m == 0)) {
    if (!identical(m, m_prior)) {
      # if the board has changed, then add a new '2' tile
      m[sample(which(m == 0), 1)] <- 2L
      print(m)
    }
    m_prior <- m
    m <- switch(
      readline(prompt = "move?: "),
      'q' = break,
      'w' = apply(m, 2, merge),                            # up
      'a' = t(apply(m, 1, merge)),                         # left
      's' = apply(m[nrow(m):1,], 2, merge)[nrow(m):1,]   , # down
      'd' = t(apply(m[,nrow(m):1], 1, merge))[,nrow(m):1], # right
      m
    )
  }

  if (max(m) >= 16) {
    message("You won!")
  }
}
main()


# The following is also from [2]
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Plot game board
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot_m <- function(m) {
#     require(ggplot2)
#     grid <- expand.grid(1:N, 1:N)
#     idx  <- arrayInd(which(m > 0), .dim = c(N, N))
#     df   <- cbind(as.data.frame(idx), V3 = factor(m[m > 0], levels = 2^(1:11)))
#     ggplot(df, aes(V2, N + 1L - V1)) +
#         geom_tile(data = grid, aes(Var1, Var2), fill = 'grey90', width = 0.9, height = 0.9) +
#         geom_tile(aes(fill = V3), width = 0.9, height = 0.9) +
#         geom_text(aes(label = V3), colour = 'white', size = 10) +
#         theme_void() +
#         xlim(0, N+1L) + ylim(0, N+1L) +
#         coord_equal() +
#         theme(legend.position = 'none') +
#         scale_fill_viridis_d(option = 'D', drop = FALSE, end = 0.9)
# }
