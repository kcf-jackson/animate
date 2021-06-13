#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

add <- broadcast(lambda(x, y, x + y))
minus <- broadcast(lambda(x, y, x - y))
times <- broadcast(lambda(x, y, x * y))
divide <- broadcast(lambda(x, y, x / y))

ARG <- R::`__`
c <- Array
isArray <- Array::isArray
