library(forecast)
library(zoo)

cores <- c(
    bg="#222222", fg="#E0E0E0", main="#69B57E", 
    second="#FD5D63", third="#63A2BB")

# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0(
        "\n\n", 
        paste(rep("#", level), collapse = ""), 
        " ", text, "\n\n"))
}

# Default theme for all plots
plot_theme <- function() {
    theme(
        plot.title=element_text(color=cores["bg"]),
        plot.background=element_rect(fill=cores["bg"], color=cores["bg"]),
        panel.background=element_rect(fill=cores["bg"], color=cores["bg"]),
        panel.grid=element_line(color=cores["bg"]),
        axis.text=element_text(color=cores["fg"], size=12),
        axis.title=element_text(color=cores["fg"]),
        strip.text=element_text(color=cores["fg"], face="bold", size=16),
        panel.grid.minor = element_line(color=cores["bg"]),
        panel.grid.major = element_line(color=cores["bg"]),
    )
}

# Format indexr data
indexr_data <- function(price_table) {
    ### create index data
    date <- price_table$Date
    price <- price_table$Price
    chip <- price_table$ProductName
    # Add week data
    week <- as.POSIXct(date, tz=Sys.timezone()) |> 
        cut.POSIXt(breaks="1 week", labels=F)
    # Best price for every GPU per week
    index_table <- aggregate(
        price, list(week, chip), FUN=min) |>
        setNames(c("Semana", "Chip", "Melhor preço"))
    # Mean of best prices for each week
    index_table <- aggregate(index_table$`Melhor preço`, list(index_table$Semana), FUN=mean) |>
        setNames(c("Semana", "Indice"))
    # Adding week last dates
    dates_table <- aggregate(date, list(week), FUN=max) |>
        setNames(c("Semana", "Dia"))
    index_table <- merge(index_table, dates_table, by="Semana")
    
    return(index_table)
}

# Plot indexer
plot_indexr <- function(price_table) {
    index_table <- indexr_data(price_table=price_table)
    
    ### Estimate next week prediction
    prediction_date <- max(index_table$Dia) + 604800
    prediction_week <- max(index_table$Semana) + 1
    
    ets_mdl <- ets(index_table$Indice)
    ets_pred <- forecast.ets(ets_mdl, h=1, level=c(60))
    
    # Add new point of data
    new_line <- c(prediction_week, ets_pred$mean[[1]], prediction_date)
    index_table <- rbind(index_table, new_line)
    
    ### Plot index
    p <- ggplot(index_table, aes(x=Dia, y=Indice)) + 
        geom_line(
            data=~subset(index_table, Semana>=prediction_week-1),
            linewidth=1.2, color=cores["fg"], linetype="dotted") +
        geom_line(
            data=~subset(index_table, Semana<prediction_week), 
            linewidth=1.2, color=cores["main"]) + 
        geom_point(
            data=~subset(index_table, Semana<prediction_week),
            size=4, color=cores["main"]) +
        geom_point(
            data=~subset(index_table, Semana==prediction_week),
            size=4, color=cores["fg"]) +
        geom_errorbar(
            data=~subset(index_table, Semana==prediction_week),
            aes(ymin=ets_pred$upper[[1]], ymax=ets_pred$lower[[1]]),
            color=cores["fg"]) +
        labs(x=NULL, y=NULL) +
        plot_theme()
    ggplotly(p) %>%
        config(displayModeBar = FALSE) %>%
        layout(margin = list(t = 0, b = 0, l = 0, r = 0))
}

# Return a vector with all the accepeted dates
allowedDates <- function(months = 12){
    require(zoo)
    require(scales)
    
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
    sureThing <- c("PriceMedian", "PriceLow", "PriceHigh", "Date")
    # must contain these 4 variables
    for (var in sureThing) {
        if (!{var %in% names(data)}) {
            stop("Necessary variables not present...")
    }}
    
    plot <- ggplot(data, aes(x = Date)) +
        theme_minimal(base_size = 14) +
        scale_y_continuous(
            labels = dollar_format(
                prefix = currency, big.mark = ".", decimal.mark = ",")) +
        labs(x = "", y = "")
    
    if (nrow(data) > 1) {
        plot <- plot + 
            geom_ribbon(aes(
                ymin = PriceLow, 
                ymax = PriceHigh), 
                alpha = 0.33,
                fill = cores["main"]) +
            geom_line(
                aes(y = PriceMedian), 
                color = cores["main"],
                size = 1.6)
    } else {
        plot <- plot +
            geom_errorbar(aes(
                ymin = PriceLow, 
                ymax = PriceHigh), 
                alpha = 0.33,
                color = cores["second"])
    }
    plot <-  plot +
        geom_point(aes(y=PriceMedian), color=cores["main"], size=4)
    ggplotly(plot)
}

# Plot average of the medians of the products over time
plotIndexr <- function(prind_list){
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
    
    currency <- prind_list[[1]]$Currency[1]
    
    agg_table <- data.frame()
    for (table in PRODUCTS) {
        agg_table <- rbind(
            agg_table, 
            select(prind_list[[table]], Date, Price, Store, Name))}
    agg_table <- agg_table %>% .[.$Date == max(.$Date), ] %>%
        mutate(
            Store = str_to_upper(Store),
            Price = {dollar(Price,
                prefix = currency, big.mark = ".", decimal.mark = ",")})

    stores <- unique(agg_table$Store)
    output <- list()
    for (store in stores) {
        output[[store]] <- agg_table %>% 
            .[.$Store == store, ] %>%
            .[order(.$Price, decreasing = FALSE), ] %>%
            select(-Store) %>%
            head(., n = 5)}
    output
}
