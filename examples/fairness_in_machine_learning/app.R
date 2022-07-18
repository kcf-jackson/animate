source("binning.R")
source("simulation.R")
source("plot.R")

# Simulate some data
set.seed(123)
n <-  10000
labels <- c("default", "repay")
s <- sim_mixture_normal(
  0.25*n, weights = c(0.5, 0.5), mean = c(450, 580), sd = c(100, 125),
  labels = labels
)
head(s)


# Shiny App ---------------------------------------------------------------
library(shiny)
library(animate)
library(htmltools)

# UI ----
ui <- fluidPage(
    titlePanel("Fairness in Machine Learning"),
    h4("This app replicates the visualisation as it is seen in the article",
       a(href='https://bair.berkeley.edu/blog/2018/05/17/delayed-impact/',
         '"Delayed impact of fair machine learning"'), "."),
    mainPanel(
      sliderInput(
        "threshold", "Credit score threshold:",
        min = 300, max = 800, value = 450, width = "500px",
      ),
        animateOutput(
          outputId = "animateOutput", 
          width = "100%", 
          height = "400px"
        )
    ),
    # Use "oninput" listener for better reactivity
    #   This observes the slider named "threshold" (set above in `ui`), then 
    # trigger an event called "threshold_oninput" with the slider value
    # to be used in `server` below.
    tags$script('
        window.onload = function() {
            document.querySelector("#threshold").oninput = function() {
              Shiny.onInputChange("threshold_oninput", this.value);
            };
        }
    ')
)


# Server ----
server <- function(input, output, session) {
    # Set up the device
    device <- animate$new(600, 600, session = session, id = "svg_1")
    attach(device)
    
    # Add a second svg element
    svg(500, 500, id = "svg-text-panel", root = "#animateOutput",
        style = list(width="500px", height="500px"))
    
    # Update plot when the slider value changes
    observeEvent(input$threshold_oninput, {
        threshold <- as.numeric(input$threshold_oninput)
        plot_distribution(threshold)
    }, ignoreNULL = FALSE)
}


# Definition of the plotting function ----
# It is cleaner to separate the key logic from `ui` and `server`.
plot_distribution <- function(threshold = 450) {
    # Handle initial empty input from shiny
    if (length(threshold) == 0 || is.null(threshold)) {
        threshold <- 450
    }
  
    p <- sim_data_to_plot_data(s, threshold)
    draw_first_panel(p, threshold)
    
    stat <- p |> 
      dplyr::group_by(granted, default) |> 
      dplyr::summarise(count = dplyr::n(), .groups = "drop_last") |>
      dplyr::ungroup()
    total <- sum(stat$count)
    
    n_payback <- sum(stat[stat$granted & !stat$default, "count"])
    n_default <- sum(stat[stat$granted & stat$default, "count"]) 
    
    # For simplicity, we assume each ball represents 5 loans (rounded up)
    multiplier <- 5
    param <- list(
      n_loan = total * multiplier,
      accepted = (n_payback + n_default) / total,
      payback = n_payback / total,
      default = n_default / total,
      profit = n_payback * multiplier, 
      loss = n_default * multiplier * -3,
      PL_net = multiplier * (n_payback + n_default * -3),
      score_gain = n_payback * multiplier * 50, 
      score_loss = n_default * multiplier * -80,
      score_net = (n_payback * 50 + n_default * -80) / total
    )
    
    draw_second_panel(param)
}


shinyApp(ui = ui, server = server)
