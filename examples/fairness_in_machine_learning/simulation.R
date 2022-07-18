#' Simulate credit score with a mixture Gaussian distribution
#'
#' @param n An integer; the number of data points to simulate.
#' @param weights A numeric vector; the mixture weights.
#' @param mean A numeric vector; the mean of each Gaussian distribution.
#' @param sd A numeric vector; the sd of each Gaussian distribution.
#' @param labels (Optional) A character vector; the labels of the components.
#' 
#' @return A data.frame.
#'
#' @examples 
#' # Usage
#' set.seed(123)
#' n <- 10000
#' labels <- c("default", "repay")
#' 
#' blue <- sim_mixture_normal(0.25*n, c(0.5, 0.5), mean = c(525, 580), sd = c(125, 125), labels = labels)
#' blue$population <- "blue"
#' 
#' orange <- sim_mixture_normal(0.75*n, c(0.5, 0.5), mean = c(620, 640), sd = c(100, 100), labels = labels)
#' orange$population <- "orange"
#' 
#' full <- rbind(blue, orange)
#' head(full)
#' 
#' @export
sim_mixture_normal <- function(n, weights, mean, sd, labels) {
    stopifnot(all(weights >= 0))
    weights <- weights / sum(weights)
    if (missing(labels)) {
        labels <- seq_along(weights)
    }
    
    population <- sample(x = labels, prob = weights, size = n, replace = TRUE)
    credit_score <- numeric(n)
    for (i in seq_along(labels)) {
        ind <- population == labels[i]
        credit_score[ind] <- rnorm(sum(ind), mean[i], sd[i])
    }
    ind <- (credit_score >= 300 & credit_score < 800)
    
    data.frame(class = population[ind], credit_score = credit_score[ind])
}
