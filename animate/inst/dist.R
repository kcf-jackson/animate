#! config(debug = T, rules = animate_rules(), deparsers = dp("basic", "auto"), asset_tags = basic_tags())

#! load_library("dom")
#! load_library("io")
#! load_library('websocket')
#! load_script('message.R')

#! load_script("assets/")  # d3, ramda, broadcast, d3-symbol
#! load_script("src/plot_primitives/")
#! load_script("class_interface.R")
#! load_script("d3_helpers.R")
#! load_script("utils.R")
#! load_script("plot_helpers.R")
#! load_script("svg_to_png.R")
#! load_script("controller.R")


plot2 <- R6Class(
  "plot2",
  # Public fields and methods ==================================================
  list(
    # Fields -------------------------------------------------------------------
    #' @field plot_commands A stack to store plotting commands.
    plot_commands = c(),

    #' @field device The active device.
    device = NULL,

    #' @field list_of_device A stack to store devices.
    list_of_device = c(),

    #' @field max_num_commands The maximum stack size for `plot_commands`. Use
    #' -1 for unlimited. This is useful when one wants to replay the animation
    #' without regenerating the animation.
    max_num_commands = 0,

    # Constructor, Get and Set -------------------------------------------------
    #' Constructing / initialising the 'plot2' object
    initialize = function(max_num_commands = 0) {
      self$max_num_commands <- max_num_commands
      self
    },

    #' Set maximum stack size of the plot commands
    set_max_num_commands = function(n) {
      self$max_num_commands <- n
      self
    },

    #' Get maximum stack size of the plot commands
    get_max_num_commands = function() {
      self$plot_commands$length
    },

    #' Set active device
    #' @param device_id A character string.
    set_active_device = function(device_id) {
      target_device <- find_device(self$list_of_device, device_id)
      if (target_device) {
        self$device <- target_device
      }
      target_device
    },

    #' Set graphical parameters
    # Interactions with the active device --------------------------------------
    new_device = function(param) {
      device <- svg_device(param)
      self$device <- device
      self$list_of_device$push(device)
    },

    delete_device = function(device_id) {
      # use the active device by default
      if (!device_id) {
        device_id <- self$device$id
        self$device <- NULL
      }
      # remove all elements in that selected device and then the device itself
      cur_device <- find_device(self$list_of_device, device_id)
      cur_device$remove("*", NULL)
      remove_device(self$list_of_device, device_id)
    },

    plot   = function(param) { plot(param, self$device)   },
    axis   = function(param) { axis(param, self$device)   },
    lines  = function(param) { lines(param, self$device)  },
    points = function(param) { points(param, self$device) },
    text   = function(param) { text(param, self$device)   },
    image  = function(param) { image(param, self$device)  },
    bars   = function(param) { bars(param, self$device)   },

    #' Set a parameter of the active device
    set_par = function(param) { self$device$set_par(param) },

    #' Remove an element from the active device
    remove = function(selector, id) { self$device$remove(selector, id) },

    # Interactions with external functions --------------------------------------------------------
    #' Save the plotting commands so that the plots can be detached
    record = function(data) {
      if ((self$max_num_commands == 0) || (data$type == "fn_export")) {
        return(self$plot_commands)
      }

      if ((self$max_num_commands == -1) || (self$plot_commands$length < self$max_num_commands)) {
        self$plot_commands$push(data)
        return(self$plot_commands)
      }

      if (self$plot_commands$length == self$max_num_commands) {
        self$plot_commands$shift()
        self$plot_commands$push(data)
        return(self$plot_commands)
      }

      if (self$plot_commands$length > self$max_num_commands) {
        stop("Plot stack exceeded the allocated size. There may be racing issues.")
      }

      stop("This line should not be reached. Please raise an issue on Github. Your `plot_commands` has size " %+%
             self$plot_commands$length %+% ", and your allocated `max_num_commands` is " %+%
             self$max_num_commands %+% " (note that -1 corresponds to 'unlimited').")

      TRUE
    },

    #' External functions use this function to pass data into the device
    dispatch = function(data) {
      for (dispatch_fn in private$dispatchers) {
        if (dispatch_fn$predicate(data$type)) {
          dispatch_fn$handler(data$message)
        }
      }
      TRUE
    },

    #' External functions use this function to pass data into the device
    dispatch_by_idx = function(idx) {
      data <- self$plot_commands[idx]
      self$dispatch(data)
    },

    #' Import the plot setting
    #' @param setting A JSON object
    import = function(setting) {
      self$plot_commands <- setting$plot_commands
      self$device <- import_device(setting$device)
      self$list_of_device <- setting$list_of_device$map(import_device)
      self$max_num_commands <- setting$max_num_commands
      self
    },

    #' Export the plot setting
    export = function() {
      setting <- list(
        plot_commands = self$plot_commands,
        device = self$device$export(),
        list_of_device = self$list_of_device$map(~.x$export()),
        max_num_commands = self$max_num_commands
      ) %>%
        JSON::stringify() %>%
        end_string()
      write(setting, "animate.json")
      TRUE
    }
  ),
  # Private fields and methods =================================================
  list(
    dispatchers = c(
      Decoder("fn_init_svg", message %=>% self$new_device(message)),
      Decoder("fn_axis", message %=>% self$axis(message)),
      Decoder("fn_bars", message %=>% self$bars(message)),
      Decoder("fn_points", message %=>% self$points(message)),
      Decoder("fn_lines", message %=>% self$lines(message)),
      Decoder("fn_image", message %=>% self$image(message)),
      Decoder("fn_text", message %=>% self$text(message)),
      Decoder("fn_export", message %=>% self$export()),
      Decoder("fn_set", message %=>% self$set_active_device(message$device_id)),
      Decoder("fn_remove", message %=>% self$remove(message$selector, message$id)),
      Decoder("fn_delete", message %=>% self$delete_device(message$id)),
      Decoder("fn_plot", message %=>% self$plot(message)),
      Decoder("fn_par", message %=>% self$set_par(message)),
      Decoder("fn_max_stacksize", message %=>% self$set_max_num_commands(message$n))
    )
  )
)

find_device <- function(list_of_device, device_id) {
  list_of_device$find(x %=>% x$id == device_id)
}

remove_device <- function(list_of_device, device_id) {
  list_of_device$filter(x %=>% x$id != device_id)
}

import_device <- function(setting) {
  new_device <- Device()
  new_device$import(setting)
  new_device
}


# # Imperative style for potential performance gain
# find_device <- function(list_of_device, device_id) {
#   i <- 0
#   while (i < list_of_device$length) {
#     current_device <- list_of_device[i]
#     if (current_device$id == device_id) {
#       return(current_device)
#     }
#     i <- i + 1
#   }
#   NULL
# }
JS_device <- plot2$new(-1)
