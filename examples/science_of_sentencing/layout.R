layout_manager <- function(cx, cy) {
    env <- new.env()
    env$queue <- c()
    env$cx <- cx
    env$cy <- cy

    add <- function(id) { 
        env$queue <- c(env$queue, id) 
    }
    
    remove <- function(id) {
        ind <- which(env$queue == id)
        env$queue <- env$queue[-ind]
    }
    
    length0 <- function() {
        length(env$queue)
    }    
    
    get_xy <- function(id) {
        compute_xy(which(env$queue == id))
    }
    
    # Rectanglar layout
    compute_xy <- function(t) {
        # convert t to (i,j) coordinates
        size <- 5
        col_ind <- ((t - 1) %% size) + 1
        row_ind <- (t - 1) %/% size
        # convert (i,j) coordinates to real coordinates
        col_size <- 0.012
        row_size <- 0.028
        x <- env$cx + col_ind * col_size
        y <- env$cy - row_ind * row_size
        list(x = x, y = y)
    }
    
    # Spiral layout
    # compute_xy <- function(t) {
    #     v <- 0.005
    #     w <- 2 * pi / 10
    #     if (t == 1) return(list(x = env$cx, y = env$cy))
    #     list(x = env$cx + v*t/2 * cos(w*t), y = env$cy + v*t * sin(w*t))
    # }
    
    # # Development
    # render_all <- function() {
    #     for (i in seq_along(env$queue)) {
    #         d <- compute_xy(i)
    #         points(d$x, d$y, id = env$queue[i], cex = 50)
    #     }
    # }

    list(add = add, remove = remove,
         get_xy = get_xy, 
         length = length0,
         # render_all = render_all,
         data = env)
}

# Usage
# M <- layout_manager(0.76, 0.76)
# sapply(1:10, \(x) M$add(x))
# as.list(M$data)
# M$render_all()
