color_palette <- c("#2196f3", "#234d66")

# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0(
        "\n\n", 
        paste(rep("#", level), collapse = ""), 
        " ", text, "\n\n"))
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

# Utility function to print prices over time
timePlot <- function(data, currency) {
    shureThing <- c("PriceMedian", "PriceLow", "PriceHigh", "Date")
    # must contain these 4 variables
    for (var in shureThing) {
        if (!{var %in% names(data)}) {
            stop("Necessary variables not present...")
    }}
    
    plot <- ggplot(data, aes(x = Date)) +
        theme_minimal(base_size = 14) +
        scale_y_continuous(
            labels = scales::dollar_format(
                prefix = currency, big.mark = ".", decimal.mark = ",")) +
        labs(x = "", y = "")
    
    if (nrow(data) > 1) {
        plot <- plot + 
            geom_ribbon(aes(
                ymin = PriceLow, 
                ymax = PriceHigh), 
                alpha = 0.33,
                fill = color_palette[2]) +
            geom_line(
                aes(y = PriceMedian), 
                color = color_palette[1],
                size = 1.6)
    } else {
        plot <- plot +
            geom_errorbar(aes(
                ymin = PriceLow, 
                ymax = PriceHigh), 
                alpha = 0.33,
                color = color_palette[2])
    }
    plot <-  plot +
        geom_point(aes(y = PriceMedian), color = color_palette[1], size = 4)
    ggplotly(plot)
}

# Plot average of the medians of the products over time
plotIndexr <- function(prind_list){
    require(shiny)
    require(dplyr)
    require(tidyr)
    require(scales)
    require(ggplot2)
    require(plotly)
    
    currency <- prind_list[[1]]$Currency[1]
    
    agg_table <- data.frame()
    for (table in PRODUCTS) {
        agg_table <- rbind(
            agg_table, 
            select(prind_list[[table]], Date, Price))}
    agg_table <- agg_table %>% group_by(Date) %>%
        summarise(
            PriceMedian = median(Price), 
            PriceLow = quantile(Price, 0.2),
            PriceHigh = quantile(Price, 0.8))
    
    timePlot(agg_table, currency)
}

# Plot progression of the price with time
plotPrice <- function(prind_table){
    require(shiny)
    require(dplyr)
    require(tidyr)
    require(scales)
    require(ggplot2)
    require(plotly)
    
    currency <- prind_table$Currency[1]
    
    new_table <- prind_table %>%
        select(Price, Date) %>%
        group_by(Date) %>%
        summarise(
            PriceMedian = median(Price), 
            PriceLow = quantile(Price, 0.2),
            PriceHigh = quantile(Price, 0.8))
    
    timePlot(new_table, currency)
}

priceByStore <- function(prind_list){
    require(kableExtra)
    require(stringr)
    require(dplyr)
    require(tidyr)
    
    agg_table <- data.frame()
    for (table in PRODUCTS) {
        agg_table <- rbind(
            agg_table, 
            select(prind_list[[table]], Date, Price, Store))}
    agg_table <- agg_table %>% .[.$Date == max(.$Date), ] %>%
        mutate(Store = str_to_upper(Store))

    stores <- unique(agg_table$Store)
    output <- list()
    for (store in stores) {
        output[[store]] <- agg_table %>% 
            .[.$Store == store, ] %>%
            .[order(.$Price, decreasing = FALSE), ] %>%
            head(., n = 5)}
    output
}
