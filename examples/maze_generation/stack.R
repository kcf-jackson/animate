Stack <- function() {
  env <- new.env()
  env$storage <- list()

  push <- function(x) {
    len <- length(env$storage)
    env$storage[[len + 1]] <- x  # add an item at the end
  }

  pop <- function() {
    len <- length(env$storage)
    last_item <- env$storage[[len]]   # retrieve the last item
    env$storage <- env$storage[-len]  # remove it from the sequence
    last_item                         # return the last item
  }

  peek <- function() {
    len <- length(env$storage)
    if (len == 0) NULL else env$storage[[len]]
  }

  is_empty <- function() {
    length(env$storage) == 0
  }

  list(push = push, pop = pop, peek = peek, is_empty = is_empty)
}

# # Example
# my_stack <- Stack()
# my_stack$peek()
# #> NULL
# my_stack$is_empty()
# #> [1] TRUE
#
# my_stack$push(15)
# my_stack$push(32)
# my_stack$peek()
# #> [1] 32
#
# my_stack$pop()
# #> [1] 32
# my_stack$peek()
# #> [1] 15
# my_stack$is_empty()
# #> [1] FALSE
