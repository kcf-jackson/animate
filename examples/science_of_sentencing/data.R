# Id generator for data points
id_generator <- function(count = 0) {
    env <- new.env()
    env$count <- count
    function() {
        id <- paste("prisoner", sep = "-", env$count)
        env$count <- env$count + 1
        return(id)
    }
}

#' The simulated agent
# 
#' @param n An integer.
#' @param uuid A function to generate id. It should take no argument, and 
#' generate an unique character string id every time it is called.
#' 
#' @return A data frame.
prisoner <- function(n, uuid) {
    risk_prob <- runif(n, 0, 1)
    data.frame(
        risk_prob = risk_prob,
        reoffend = sapply(risk_prob, \(p) sample(c(T, F), 1, prob = c(p, 1-p))),
        state = rep("birth", n),
        id = sapply(1:n, \(x) uuid())
    )
}

# Usage
# pid <- id_generator(0)
# prisoner(10, pid)
