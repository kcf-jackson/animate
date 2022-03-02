#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

controller <- R6Class(
  "controller",
  public = list(
    #' @field plots A 'plot2'-class object
    plots = NULL,
    player_pointer = -1,
    player_handle = NULL,  # needed to handle loop

    initialize = function(p) {
      self$plots <- p

      num_cmd <- self$plots$get_max_num_commands()
      self$player_pointer <- ifelse(num_cmd > 0 || num_cmd == -1, 0, -1)

      self
    },

    #' @description Play the next frame
    #' @note If the last frame is reached, play the first frame.
    play = function() {
      p_obj   <- self$plots
      num_cmd <- self$plots$get_max_num_commands()
      ind     <- self$player_pointer
      if (ind >= 0) {
        p_obj$dispatch_by_idx(ind)
        self$player_pointer <- (ind + 1) %% num_cmd
      }
      TRUE
    },

    #' @description Play the next n frames
    #' @param n_frames An integer.
    #' @param wait A number; the number of milliseconds to wait for before the
    #' next frame is drawn.
    play_for = function(n_frames, wait = 20, callback) {
      if (self$player_pointer < 0) {
        return(NULL)
      }
      play_n_frame <- function() {
        frame_counter <- 0
        function(handle) {
          self$play()
          frame_counter <<- frame_counter + 1
          if (frame_counter >= n_frames) {
            window$cancelAnimationFrame(handle)
            if (callback) callback()
          }
          TRUE
        }
      }
      self$player_handle <- make_animation(play_n_frame(), wait)()
      TRUE
    },

    #' @description Play until a certain frame is reached
    #' @param frame_num An integer; the target frame number.
    #' @param wait A number; the number of milliseconds to wait for before the
    #' next frame is drawn.
    #' @note The playing of the frames is exclusive, i.e., the function stops
    #' right before the target frame is reached, and the target frame is not
    #' executed. To loop through the entire commands stack, use 0 for the `frame_num`.
    play_until = function(frame_num, wait = 20, callback) {
      if (self$player_pointer < 0) {
        return(NULL)
      }
      play_until_frame <- function() {
        function(handle) {
          self$play()
          if (self$player_pointer == frame_num) {
            window$cancelAnimationFrame(handle)
            if (callback) callback()
          }
          TRUE
        }
      }
      self$player_handle <- make_animation(play_until_frame(), wait)()
      TRUE
    },

    #' @description Loop through the available frames n times
    #' @param times An integer; the number of times to loop.
    #' @param wait A number; the number of milliseconds to wait for before the
    #' next frame is drawn.
    loop = function(times = 1, wait = 20, callback) {
      if (self$player_pointer < 0) {
        return(NULL)
      }
      loop_through <- function() {
        counter <- 0
        function(handle) {
          self$play()
          if (self$player_pointer == 0) {
            counter <<- counter + 1
          }
          if (counter >= times) {
            window$cancelAnimationFrame(handle)
            if (callback) callback()
          }
          TRUE
        }
      }
      self$player_handle <- make_animation(loop_through(), wait)()
      TRUE
    },

    #' @description Execute a callback function when a event is triggered.
    #' @param selector A character string; the query selector.
    #' @param event A character string; a HTML DOM event.
    #' @param callback A function; the callback function.
    watch = function(selector, event, callback) {
      select_dom(selector)$addEventListener(event, callback)
    },

    #' @description Remove an event listener.
    #' @param selector A character string; the query selector.
    #' @param event A character string; a HTML DOM event.
    #' @param callback A function; the callback function.
    unwatch = function(selector, event, callback) {
      select_dom(selector)$removeEventListener(event, callback)
    }
  ),
  private = list()
)


#' Execute a function every s milliseconds
#'
#' @param f A function; the first argument is the handle, which can be used to
#' stop the animation when an ending condition is reached.
#' @param wait The milliseconds.
#'
#' @examples
#' f <- function() {
#'   counter <- 0
#'   function(handle) {
#'     counter <<- counter + 1
#'     console::log(counter)
#'     if (counter >= 20) {
#'       window$cancelAnimationFrame(handle)
#'     }
#'   }
#' }
#' timer_1 <- make_animation(f(), 200)
#' res <- timer_1()
#'
#' @export
make_animation <- function(f, wait) {
  then <- performance$now()
  main <- function() {
    timer <- requestAnimationFrame(main)
    now <- performance$now()
    elapsed <- now - then
    if (elapsed > wait) {
      then <<- now - (elapsed %% wait)
      # main animation loop
      f(timer)
    }
    return(timer)
  }
  main
}
