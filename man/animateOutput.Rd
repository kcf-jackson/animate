% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{animateOutput}
\alias{animateOutput}
\title{Create an animate output (container) element}
\usage{
animateOutput(
  outputId = "animateOutput",
  width = "100\%",
  height = "400px",
  ...
)
}
\arguments{
\item{outputId}{output variable to read the plot/image from.}

\item{width}{Width of the plot area. Must be a valid CSS unit (like "100\%", "400px", "auto").}

\item{height}{Height of the plot area. Must be a valid CSS unit (like "100\%", "400px", "auto").}

\item{...}{Optional CSS styling for the container of the plot.}
}
\description{
Create an animate output (container) element
}
\note{
(Advanced usage) A "stack_limit" parameter can be included in the optional parameters
to control how many directives the device should keep track of.
}
\examples{
# Using 'animate' in a 'Shiny' app
library(shiny)

ui <- fluidPage(
  actionButton("buttonPlot", "Plot"),
  actionButton("buttonPoints", "Points"),
  actionButton("buttonLines", "Lines"),
  animateOutput()
)

server <- function(input, output, session) {
  device <- animate$new(600, 400, session = session)
  id <- new_id(1:10)

  observeEvent(input$buttonPlot, {     # Example 1
    device$plot(1:10, 1:10, id = id)
  })

  observeEvent(input$buttonPoints, {   # Example 2
    device$points(1:10, runif(10, 1, 10), id = id, transition = TRUE)
  })

  observeEvent(input$buttonLines, {    # Example 3
    x <- seq(1, 10, 0.1)
    y <- sin(x)
    id <- "line_1"
    device$lines(x, y, id = id)
    for (n in 11:100) {
      x <- seq(1, n, 0.1)
      y <- sin(x)
      device$lines(x, y, id = id)
      Sys.sleep(0.05)
    }
  })
}

# shinyApp(ui = ui, server = server)  # Launch the 'Shiny' app

}
