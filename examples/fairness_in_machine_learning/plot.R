# First panel ------------------------------------------------------------------
draw_first_panel <- function(p, threshold, svg_id = "svg_1") {
  # Switch to the first canvas
  set(svg_id)
  
  # Set scale
  par(xlim = c(300, 800), ylim = c(-500, 1500))
  
  # Histogram
  points(300 + 10 * p$x, p$y, id = 1:nrow(p), bg = p$color, col = p$color,
         cex = 60)
  
  # Labels
  baseline <- list("font-size" = "10pt")
  
  xlab <- seq(300, 800, 50)
  text(x = xlab - 15, y = rep(1400, length(xlab)), labels = as.character(xlab),
       id = new_id(xlab, "xaxis-label"), style = baseline)
  
  # Threshold with text
  lines(x = c(threshold, threshold),  y = c(-100, 1150), lwd = 2, id = "threshold-line")
  text(x = threshold - 100, y = 1200, labels = paste0("loan threshold: ", threshold),
       id = "threshold-text", style = list("font-weight" = 700))
  
  # Legend
  text(350, -250, "denied loan / would default", id = "legend-text-1", style = baseline)
  text(350, -350, "denied loan / would pay back", id = "legend-text-2", style = baseline)
  text(590, -250, "granted loan / defaults", id = "legend-text-3", style = baseline)
  text(590, -350, "granted loan / pays back", id = "legend-text-4", style = baseline)
  bars(540, -270, 20, 80, bg = "lightgray", id = "legend-box-1")
  bars(540, -370, 20, 80, bg = "gray", id = "legend-box-2")
  bars(565, -270, 20, 80, bg = "lightblue", id = "legend-box-3")
  bars(565, -370, 20, 80, bg = "blue", id = "legend-box-4")
}


# Second panel -----------------------------------------------------------------
draw_second_panel <- function(param, svg_id = "svg-text-panel") {
  # Switch to the second canvas
  set(svg_id)
  
  # Formatting and Colors
  par(xlim = c(0, 1), ylim = c(0, 1), mar = c(0, 0, 0, 0))
  pct <- \(x) round(100 * x)
  red <- \(x) paste0("<span style='color:red;font-weight:700;'>", x, "</span>")
  green <-  \(x) paste0("<span style='color:limegreen;font-weight:700;'>", x, "</span>")
  blue <- \(x) paste0("<span style='color:blue;font-weight:700;'>", x, "</span>")
  lightblue <- \(x) paste0("<span style='color:lightblue;font-weight:700;'>", x, "</span>")
  gray <- \(x) paste0("<span style='color:gray;font-weight:700;'>", x, "</span>")
  bold <- \(x) paste0("<span style='font-weight:700;'>", x, "</span>")
  
  accept_phrase <- blue(glue::glue("{pct(param$accepted)}% of loans are accepted"))
  deny_phrase <- gray(glue::glue("{pct(1 - param$accepted)}% of loans are denied"))
  payback_phrase <- blue(glue::glue("{pct(param$payback)}% of people will pay back their loans"))
  default_phrase <- lightblue(glue::glue("{pct(param$default)}% of people who won't"))
  
  gain_loss <- \(x, y) ifelse(x >= 0, green(y), red(y))
  net_profit <- gain_loss(param$PL_net,
                          glue::glue("net profit is {param$PL_net}"))
  net_score <- gain_loss(round(param$score_net, 1),
                         glue::glue("total change of the average credit score \\
                                    in the population is {round(param$score_net, 1)}"))
  
  # The main command
  objects(
    0, 0.1, 1, 1, 
    content = glue::glue("
    <div>{accept_phrase}, and {deny_phrase}.
    <br><br>\\
    {payback_phrase}, \\
    and {default_phrase}. \\
    <br><br>
    The former generates a profit of {round(param$profit)}, and \\
    the latter generates a loss of {round(param$loss)}. \\
    The {net_profit}.
    <br><br>
    The people who pay back earn a credit score of 50, \\
    and the people who don't lose a credit score of -100.
    The {net_score}.
    </div>"
    ),
    id = "text-desc",
    style = list("font-size" = "19px")
  )
}
