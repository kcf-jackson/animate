evaluate_prisoners <- function(prisoners, threshold) {
    prisoners$granted <- sample(c(T, F), nrow(prisoners), replace = T)
    prisoners$granted[prisoners$risk_prob < threshold[1]] <- T
    prisoners$granted[prisoners$risk_prob >= threshold[2]] <- F
    
    prisoners$risk_class <- "medium-risk"
    prisoners$risk_class[prisoners$risk_prob < threshold[1]] <- "low-risk"
    prisoners$risk_class[prisoners$risk_prob >= threshold[2]] <- "high-risk"
    
    t0 <- runif(nrow(prisoners), 0, 6000)
    t1 <- runif(nrow(prisoners), 2000, 6000)
    prisoners$delay_0 <- t0
    prisoners$delay_1 <- t1
    prisoners$delay_2 <- t0 + t1
    prisoners
}

run_sim <- function(n, steps = 2) {
    clear()
    make_background()
    
    pid <- id_generator(0)    
    prisoners <- prisoner(n, pid)
    
    threshold <- c(0.3, 0.8)
    prop <- rev(diff(c(0, threshold, 1)))  # High-Medium-Low
    bounds <- update_middle_column(prop)
    
    layout_managers <- list(
        layout_manager(0.72, 0.84), layout_manager(0.83, 0.84),
        layout_manager(0.72, 0.34), layout_manager(0.83, 0.34)
    )
    
    prisoners <- evaluate_prisoners(prisoners, threshold)
    for (time in 1:steps) {
        for (i in 1:nrow(prisoners)) {
            person <- prisoners[i, ]
            
            # Call the different plot functions depending on the stages
            if (person$state == "birth") {
                enter_stage(person)
                prisoners[i, ]$state <- "classify"
                next
            }
            if (person$state == "classify") {
                classify(person, bounds)
                prisoners[i, ]$state <- "decided"
                next
            }
            if (person$state == "decided") {
                decide(person, layout_managers)
                # Update rates
                ind <- prisoners$state %in% c("decided", "exit")
                rates <- c(
                    sum(prisoners$reoffend & prisoners$granted & ind),
                    sum((!prisoners$reoffend) & (!prisoners$granted) & ind)
                ) / sum(ind)
                update_rates(rates, delay = person$delay_2 + 1000)
                
                prisoners[i, ]$state <- "exit"
                next
            }
            if (person$state == "exit") {
                exit_stage(prisoners, layout_managers)
                next
            }
        }
        # Wait for animation to complete
        Sys.sleep(6)
        
        # Add more prisoners every iteration
        new_prisoners <- prisoner(sample(10:15, 1), pid)
        new_prisoners <- evaluate_prisoners(new_prisoners, threshold)
        prisoners <- rbind(prisoners, new_prisoners)
    }
}
