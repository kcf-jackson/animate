#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))
seq <- function(from, to, by = 1) {
  res <- Array()
  res$push(from)

  if (from == to) {
    return(res)
  }

  if (Math::sign(to - from) != Math::sign(by)) {
    stop("Cannot go from the first argument to the second arguemnt using a step size given in the third argument")
  }

  cur <- from + by
  while (cur <= to) {
    res$push(cur)
    cur <- cur + by
  }
  res
}

range <- R::range2

map2 <- R::map2
