#' Start a Websocket server
#'
#' @description A thin wrapper of the `httpuv` package, modified to serve animated
#' plots.
websocket <- R6::R6Class("websocket", public = list(
  #' @field app A list of functions that define the application.
  app = NULL,

  #' @field server A server handle to be used by 'stopServer'.
  server = NULL,

  #' @field ws A WebSocket channel to handle the communication between
  #' the R session and the browser session.
  ws = NULL,

  #' @field in_handler A function to handle instructions sent by the
  #' browser session.
  in_handler = NULL,

  #' @field port An integer; the TCP port number.
  port = 9454,

  #' @field connected TRUE or FALSE; whether a connection has been established.
  #' One should start the WebSocket server before launching the web page that
  #' connects to the server.
  connected = FALSE,

  #' @field started TRUE or FALSE; whether a server has been started. Use
  #' the \code{startServer} method to start a server.
  started = FALSE,

  #' @description Start a WebSocket server
  startServer = function() {
    if (self$started) {
      message("There is an existing server running.")
      return(invisible(NULL))
    }
    # Start the server if not already started
    self$server <- httpuv::startServer("0.0.0.0", self$port, self$app, 250)
    message("Server started.")
    self$started <- TRUE
  },

  #' @description Stop a WebSocket server
  stopServer = function() {
    if (!self$started) {
      message("There is no server running.")
      return(invisible(NULL))
    }
    # Stop the server if not already stopped
    httpuv::stopServer(self$server)
    message("Server stopped.")
    self$started <- FALSE
    self$connected <- FALSE
  },

  #' @description List all running WebSocket servers
  listServers = function() {
    res <- httpuv::listServers()
    message(glue::glue("{length(res)} server(s) is/are running."))
    return(res)
  },

  #' @description Stop all running WebSocket servers
  stopAllServers = function() {
    httpuv::stopAllServers()
    self$started <- FALSE
    self$connected <- FALSE
    self$listServers()
  },

  #' @description Initialise a WebSocket connection
  #'
  #' @param in_handler A function to handle incoming message, default to
  #' be \link[base:print]{print} which only displays the message without
  #' any processing.
  #' @param port An integer; the TCP port number.
  #'
  #' @return A 'websocket' object.
  #
  # @examples
  # \donttest{
  # # Launch a WebSocket server
  # ws <- websocket$new()
  # ws$startServer()
  # ws$listServers()    # Check that a server is running
  #
  # ws$stopServer()
  # ws$listServers()    # Confirm no server is running
  # }
  initialize = function(in_handler, port = 9454) {
    self$app <- list(
      call = function(req) {  # nocov start
        list(
          status = 200L,
          headers = list("Content-Type" = "text/html"),
          body = "Server is running."
        )
      },
      onWSOpen = function(ws) {
        self$ws <- ws
        self$connected <- TRUE
        ws$onMessage(function(binary, input) {
          in_handler(input)
        })
      }  # nocov end
    )
    self$in_handler <- in_handler
    self$port <- port
  }
))
