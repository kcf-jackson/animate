# Plot functions
enter_stage <- function(person) {
    gutter <- 0.05
    col_width <- (1 - 3 * gutter) / 3
    x_unit <- gutter + col_width
    
    r <- runif(1, 0, 0.1)
    theta <- runif(1, 0, 2 * pi)
    x <- x_unit / 2 + r * sin(theta)
    y <- 0.485 + 2 * r * cos(theta)
    
    points(x, y, id = person$id, 
           bg = ifelse(person$reoffend, "#00CCFF", "white"), 
           col = "#00CCFF", lwd = 2, cex = 0.001)
    points(x, y, id = person$id, 
           bg = ifelse(person$reoffend, "#00CCFF", "white"), 
           col = "#00CCFF", lwd = 2, cex = 60, 
           transition = list(delay = person$delay_0))
}

classify <- function(person, bounds) {
    risk_label <- person$risk_class
    risk_colors <- c("red", "#ffd00b", "green")
    ind <- which(bounds$id == risk_label)
    x <- bounds$x + bounds$width * runif(1, 0.1, 0.9)
    y <- bounds$y[ind] + bounds$height[ind] * runif(1, 0.2, 0.65)
    points(x, y, id = person$id,
           bg = ifelse(person$reoffend, risk_colors[ind], "white"),
           col = risk_colors[ind], lwd = 2, cex = 60,
           transition = list(delay = person$delay_1,
                             duration = 1500))
}

decide <- function(person, layout_managers) {
    risk_label <- person$risk_class
    risk_colors <- list("high-risk" = "red", 
                        "medium-risk" = "#ffd00b", 
                        "low-risk" = "green")
    manager_id <- 1 + 1 * (!person$reoffend) + 2 * (!person$granted)
    layout_manager <- layout_managers[[manager_id]]
    layout_manager$add(person$id)
    d <- layout_manager$get_xy(person$id)
    points(d$x, d$y, id = person$id,
           bg = ifelse(person$reoffend, risk_colors[[risk_label]], "white"),
           col = risk_colors[[risk_label]], lwd = 2, cex = 60,
           transition = list(delay = person$delay_2, duration = 1500))
}

exit_stage <- function(prisoners, layout_managers) {
    for (layout_manager in layout_managers) {
        while (layout_manager$length() > 25) {
            all_id <- layout_manager$data$queue
            remove_id <- head(all_id, 5)
            keep_id <- setdiff(all_id, remove_id)
            prisoners_to_remove <- prisoners[prisoners$id %in% remove_id, ]
            prisoners_to_update <- prisoners[prisoners$id %in% keep_id, ]
            
            ds <- lapply(remove_id, \(id) layout_manager$get_xy(id))
            ds <- list(x = sapply(ds, \(d) d$x), y = sapply(ds, \(d) d$y))
            update_all_points(prisoners_to_remove, ds, list(duration = 1500, remove = NULL))
            sapply(remove_id, \(id) layout_manager$remove(id))
            
            ds <- lapply(keep_id, \(id) layout_manager$get_xy(id))
            ds <- list(x = sapply(ds, \(d) d$x), y = sapply(ds, \(d) d$y))
            update_all_points(prisoners_to_update, ds, list(duration = 1500))
        }
    }
}

update_all_points <- function(prisoners, ds, transition) {
    risk_colors <- list("high-risk" = "red", 
                        "medium-risk" = "#ffd00b", 
                        "low-risk" = "green")
    bcols <- as.character(risk_colors[prisoners$risk_class])
    fcols <- bcols
    fcols[!prisoners$reoffend] <- "white"
    points(ds$x, ds$y, 
           id = prisoners$id,
           bg = fcols, col = bcols,
           lwd = 2, cex = 60,
           transition = transition)
}
