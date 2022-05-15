# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0("\n\n", 
               paste(rep("#", level), collapse = ""), 
               " ", text, "\n"))
}

# Return a vector with all the accepeted dates
allowedDates <- function(months = 12){
    require(zoo)
    
    months <- as.integer(months)
    if (months < 1) {months <- as.integer(1)}
    
    firstMonth <- {as.yearmon( Sys.Date() ) - (months - 1) * 1/12}
    firstMonth <- as.Date(firstMonth)

    dates <- seq.Date(
        from = firstMonth, 
        to =  Sys.Date(), 
        by = "day")
    dates
}

priceByStore <- function(data){
    require(kableExtra)
    require(dplyr)
    require(tidyr)
    
    kableExtra::kable(data)
}
