# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0("\n\n", 
               paste(rep("#", level), collapse = ""), 
               " ", text, "\n"))
}

# Return a vector with all the accepeted dates
allowedDates <- function(months = 12){
    require(zoo)
    
    allowedMonths <- {as.yearmon( Sys.Date() ) - seq(from = 0, length.out = months, by = 1/12)}
    allowedMonths <- as.Date(allowedMonths)[order(allowedMonths)]
    if (Sys.Date() > max(allowedMonths)) {
        allowedMonths[length(allowedMonths)] <- Sys.Date()
    }
    
    dates <- c()
    for (ind in seq_along(allowedMonths)[-1]) {
        dates <- rbind(
            dates, 
            seq.Date(
                from = allowedMonths[ind-1], 
                to = allowedMonths[ind], 
                by = "day"
            )
        )
    }
    as.Date(dates)
}

priceByStore <- function(data){
    require(kableExtra)
    require(dplyr)
    require(tidyr)
    
    kableExtra::kable(data)
}
