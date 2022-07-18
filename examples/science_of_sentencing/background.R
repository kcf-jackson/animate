# A function for drawing the background
make_background <- function() {
    gutter <- 0.05
    par(xlim = c(0, 1), ylim = c(0, 1))
    col_width <- (1 - 3 * gutter) / 3
    x_unit <- gutter + col_width
    half_gutter <- gutter / 2
    bars(0, 0, 1, 1, bg = "#F6F6F4")
    
    # Left Column
    v_ref_0 <- x_unit / 2
    v_ref <- v_ref_0 - 0.075
    main_font <- list("font-size" = "20px", "font-family" = "Courier")
    desc_font <- list("font-size" = "12px", "font-family" = "Courier")
    points(v_ref_0, 0.485, cex = 200^2, bg = "white", col = "black", lwd = 1)
    text(v_ref_0 - 0.11, 0.85, "PAROLE-ELIGIBLE", style = main_font)
    text(v_ref, 0.8, "PRISONERS", style = main_font)
    points(v_ref, 0.1675, cex = 50, bg = "#00CCFF", col = "#00CCFF", lwd = 1)
    points(v_ref, 0.1275, cex = 50, bg = "white", col = "#00CCFF", lwd = 1)
    text(v_ref + 0.015, 0.16, "WILL REOFFEND", style = desc_font)
    text(v_ref + 0.015, 0.12, "WILL NOT REOFFEND", style = desc_font)
    
    # Middle Column
    prop <- c(0.4, 0.3, 0.3)  # H-M-L
    bounds <- update_middle_column(prop)
    
    # Right Column
    rates <- c(0, 0)
    update_right_column(rates)
    
    # Reference Lines
    # add_reference_lines(x_unit, gutter)
    bounds 
}

update_middle_column <- function(prop) {
    gutter <- 0.05
    half_gutter <- 0.5 * gutter
    total_height <- (1 - 2 * gutter - 2 * half_gutter)
    hs <- total_height * prop
    
    col_width <- (1 - 3 * gutter) / 3
    x_unit <- gutter + col_width
    main_font <- list("font-size" = "20px", "font-family" = "Courier")
    
    risk_box <- function(y, h, id) {
        bars(x_unit, y, col_width, h, 
             bg = "white", col = "black", lwd = 1,
             id = id)    
        bars(x_unit, y, 0.015, h, 
             bg = ifelse(id == "high-risk", "red", 
                         ifelse(id == "medium-risk", "#ffd00b", "green")), 
             id = paste0(id, "-banner"))
        if (h >= 0.15) {
            risk_label <- toupper(gsub(x = id, "-", " "))
            text(x = x_unit + gutter, 
                 y = max(y + 0.7 * h, y + h - 2 * gutter), 
                 id = paste0(id, "-text"),
                 labels = risk_label, 
                 style = main_font)
        }
    }
    
    risk_box(gutter, hs[1], "high-risk")
    risk_box(gutter + hs[1] + 0.5 * gutter, hs[2], "medium-risk")
    risk_box(gutter + sum(hs[1:2]) + gutter, hs[3], "low-risk")    
    
    list(
        id = c("high-risk", "medium-risk", "low-risk"),
        total_height = total_height,
        prop = prop, 
        width = col_width,
        x = x_unit,
        height = hs,
        y = c(gutter, 
              gutter + hs[1] + 0.5 * gutter,
              gutter + sum(hs[1:2]) + gutter)
    )
}

update_right_column <- function(rates) {
    gutter <- 0.05
    col_width <- (1 - 3 * gutter) / 3
    x_unit <- gutter + col_width
    main_font <- list("font-size" = "20px", "font-family" = "Courier")
    desc_font <- list("font-size" = "12px", "font-family" = "Courier")
    
    bars(2 * x_unit, 0.5 + gutter, col_width, 0.4, bg = "white", col = "black", lwd = 1)
    bars(2 * x_unit, gutter, col_width, 0.4, bg = "white", col = "black", lwd = 1)
    text(2 * x_unit + gutter / 2 + 0.022, 0.5 - 2.5 * gutter, "DENIED PAROLE", style = main_font)
    text(2 * x_unit + gutter / 2 + 0.015, 1 - 2.5 * gutter, "AWARDED PAROLE", style = main_font)
    
    x_ref <- 2 * x_unit + gutter / 2 + 0.025
    text(x = x_ref + 0.035, 
         y = 0.515 + 2 * gutter, 
         "AWARDED PAROLE AND", style = desc_font)
    text(x = x_ref + 0.035, 
         y = 0.515 + 1.5 * gutter, 
         "THEN REOFFENDED", style = desc_font)
    text(x = x_ref + 0.035, 
         y = 0.015 + 2.3 * gutter, 
         "DENIED PAROLE BUT", style = desc_font)
    text(x = x_ref + 0.035, 
         y = 0.015 + 1.8 * gutter, 
         "WOULDN'T HAVE", style = desc_font)
    text(x = x_ref + 0.035, 
         y = 0.015 + 1.3 * gutter, 
         "REOFFENDED", style = desc_font)
    update_rates(rates)
}

update_rates <- function(rates, delay = 0) {
    gutter <- 0.05
    col_width <- (1 - 3 * gutter) / 3
    x_unit <- gutter + col_width
    x_ref <- 2 * x_unit + gutter / 2 + 0.025
    main_font <- list("font-size" = "20px", "font-family" = "Courier")
    
    text(x_ref - 0.015, 
         0.515 + 1.75 * gutter, 
         paste0(round(rates[1] * 100), "%"),
         id = "rate-1",
         style = main_font, transition = list(delay = delay))
    text(x_ref - 0.015, 
         0.015 + 2.05 * gutter, 
         paste0(round(rates[2] * 100), "%"),
         id = "rate-2",
         style = main_font, transition = list(delay = delay))
}

add_reference_lines <- function(x_unit, gutter) {
    abline(v = 0)
    abline(v = 1 * x_unit)
    abline(v = 2 * x_unit)
    abline(v = 3 * x_unit)
    
    abline(h = 0)
    abline(h = gutter)
    abline(h = 0.5 + gutter)
    abline(h = 0.5 - gutter)
    abline(h = 1 - gutter)
    abline(h = 1)    
}

# Usage
# clear()
# make_background()
