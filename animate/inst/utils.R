#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

add <- broadcast(lambda(x, y, x + y))
minus <- broadcast(lambda(x, y, x - y))
times <- broadcast(lambda(x, y, x * y))
divide <- broadcast(lambda(x, y, x / y))

ARG <- R::`__`
c <- Array
isArray <- Array::isArray

Id <- lambda(x, "#" %+% x)
parse_px <- lambda(x, parseInt(x$replace("px", "")))
end_string <- lambda(x, x %+% "\n")
translate <- lambda(x, y, "translate(" %+% x %+% "," %+% y %+% ")")

seq <- function(from, to, by = 1) {
  res <- Array()
  res$push(from)

  if (from == to) return(res)

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

generate_id = (function(id_count) {
  res_fun <- function(prefix, n = 1) {
    res <- c()
    for (i in seq(1, n)) {
      id_count <<- id_count + 1
      id <- ifelse(prefix, prefix %+% "_" %+% id_count, id_count)
      res$push(id)
    }
    # Return scalar if the Array is of length 1
    if (res$length == 1) return(res[0])
    res
  }
  res_fun
})(0)
