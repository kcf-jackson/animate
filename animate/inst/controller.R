#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

controller <- R6Class(
  "controller",
  public = list(
    plots = NULL,
    player_pointer = -1,
    player_handle = NULL,  # needed to handle loop

    initialize = function(p) {
      self$plots <- p
      self$player_pointer <- ifelse(p$length() > 0, 0, -1)
      self
    },

    play = function() {
      p_obj <- self$plots
      ind <- self$player_pointer
      if (ind >= 0) {
        p_obj$dispatch_by_idx(ind)
        self$player_pointer <- (ind + 1) %% p_obj$length()
      }
      TRUE
    },

    loop = function() {
      p_obj <- self$plots
      if (!self$player_handle && self$player_pointer >= 0) {
        self$player_handle <- setInterval(function() {
          p_obj$dispatch_by_idx(self$player_pointer)
          self$player_pointer <- (self$player_pointer + 1) %% p_obj$length()
          TRUE
        }, 300)
      }
      TRUE
    }
  ),
  private = list()
)

# ctrl <- controller$new(JS_device)
