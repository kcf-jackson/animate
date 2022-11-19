# Custom animation
custom_transition <- function(f, from, to) {
  function(...) {
    attr <- names(from)
    # From
    args <- list(...)
    args$transition <- NULL
    args[[attr]] <- append(from[[attr]], args[[attr]])
    do.call(f, args)
    # To
    args <- list(...)
    if (is.null(args$transition)) {
      args$transition <- TRUE
    }
    args[[attr]] <- append(to[[attr]], args[[attr]])
    do.call(f, args)
  }
}

fade <- function(f, from = 0, to = 1) {
  custom_transition(f,
                    from = list(style = list(opacity = from)),
                    to = list(style = list(opacity = to)))
}
