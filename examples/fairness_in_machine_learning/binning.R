require(magrittr)

#' Convert observation into coordinate points for plotting with preserved subgroup
#' 
#' @param s A data frame; the output from `sim_mixture_normal`.
#' @param threshold A number; the threshold to be applied to the credit score.
#' 
#' @return A data frame.
#' 
#' @export
sim_data_to_plot_data <- function(s, threshold) {
    p <- unique(s$class) %>% 
        lapply(function(target_class) {
            cols <- c("lightgray", "gray", "lightblue", "blue")
            ind <- s$class == target_class
            scores <- s$credit_score[ind]
            
            bins <- seq(300, 800, 10)
            p <- obs_to_coord(scores, bins = bins, unit = 5, unit_2 = 50)
            p$bins <- bins[p$x]
            p$default <- target_class == "default"
            p$granted <- p$bins >= threshold
            p$color <- cols[1 + ((!p$default) + 2 * p$granted)]
            p
        }) %>% 
        do.call(rbind, .)
    p[order(p$color, decreasing = T), ]
}

#' Convert observation into coordinate points for plotting
#' 
#' @param xs A numeric vector; the vector to be binned.
#' @param bins A numeric vector; the end points of the bins.
#' @param unit A positive number; the number of data points a ball represents.
#' @param unit_2 A positive number; the size of the balls.
#' @return A data.frame with the "x" and "y" columns.
#' 
#' @examples
#' ys <- obs_to_coord(orange$credit_score)
#' plot(ys$x, ys$y, pch = 19)
obs_to_coord <- function(xs, bins = seq(300, 800, 10), unit = 5, unit_2 = 30) {
    coords <- xs %>% 
        bin_vector() %>% 
        bins_to_balls(unit = unit) %>%
        lapply(balls_to_coordinates, unit = unit_2)
    result <- seq_along(coords) %>% 
        lapply(function(i) cbind(i, coords[[i]])) %>%
        do.call(rbind, .) %>% 
        as.data.frame() %>% 
        set_colnames(c("x", "y"))
    result
}

# Bin a numeric vector
bin_vector <- function(xs, bins = seq(300, 800, 10)) {
    n_bins <- length(bins) - 1
    sapply(1:n_bins, \(ind) sum(xs >= bins[ind] & xs < bins[ind+1]))
}

# Convert to integer number of balls
bins_to_balls <- function(xs, unit) {
    round(xs) %/% unit
}

# Convert a number of balls to a set of coordinates
balls_to_coordinates <- function(n, unit) {
    if (n == 0) 0 else c(0, seq(n)) * unit
}
