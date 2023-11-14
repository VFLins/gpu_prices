library(forecast)
library(bslib)
library(htmltools)
library(DT)
library(flexdashboard)
source("setup_data.R")

cores <- c(
    bg="#222222", fg="#E0E0E0", 
    main="#69B57E", second="#FD5D63", third="#63A2BB")

# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0(
        "\n\n", 
        paste(rep("#", level), collapse = ""), 
        " ", text, "\n\n"))
}

format_number <- function(x, prefix="R$", suffix="") {
    paste(
        prefix, 
        formatC(x=x, format="f", decimal.mark=",", digits=2), 
        suffix
    )
}

totitle <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
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

# Plot indexer
plot_indexr <- function() {
    index_table <- index_data
    
    ### Estimate next week prediction
    prediction_date <- max(index_table$Dia) + 604800
    prediction_week <- max(index_table$Semana) + 1
    
    ets_mdl <- ets(index_table$Indice)
    ets_pred <- forecast.ets(ets_mdl, h=1, level=c(70))
    
    # Add new point of data
    new_line <- c(prediction_week, prediction_date, ets_pred$mean[[1]])
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
        config(displayModeBar=FALSE) %>%
        layout(margin=list(t=0, b=0, l=0, r=0))
}

# scatterplot for rasterization data with regression line
plot_perf_scatter <- function(dataset=price_raster_perf, preset="fhd_ultra") {
    discrete_palette <- c("#63A2BB", "#69B57E", "#FD5D63")
    
    curr_dataset <- data.frame(
        Chip = dataset$model,
        Preço = dataset$`Melhor preço`,
        Performance = dataset[[preset]],
        Família = dataset$chip_family
    )
    
    model <- lm(Performance~Preço, data=curr_dataset)
    coefs <- coef(model)
    
    p <- ggplot(
        curr_dataset, 
        aes(x=Preço, y=Performance, color=Família, group=Chip)) +
        geom_point(size=4, alpha=.8) +
        geom_abline(intercept=coefs[1], slope=coefs[2], color=cores["fg"]) +
        geom_hline(yintercept=60, color=cores["fg"], linetype="dotted") +
        scale_color_manual(values=discrete_palette) +
        labs(x="Preço (R$)", y="Desempenho") + 
        theme(legend.position="none") +
        plot_theme()
    ggplotly(p) %>%
        config(displayModeBar=FALSE) %>%
        layout(margin=list(t=0, b=0, l=0, r=0))
}

plot_perf_table <- function(dataset=price_raster_perf, preset="fhd_ultra", table_cols) {
    curr_dataset <- data.frame(
        Chip = dataset$model,
        Preço = dataset$`Melhor preço`,
        Performance = dataset[[preset]]
    ) |> na.omit() |> setNames(c("Chip", "Preço", table_cols[1]))
    
    price <- curr_dataset$`Preço`
    perfr <- curr_dataset[[table_cols[1]]]
    curr_dataset[table_cols[2]] <- price / perfr
    
    # ordering and numerating from most relevant to least
    out <- curr_dataset |> dplyr::arrange_at(table_cols[2])
    row.names(out) <- seq_along(curr_dataset$Chip)
    
    # formatting columns for pretty printing
    out[table_cols[2]] <- format_number(out[[table_cols[2]]])
    out["Preço"] <- format_number(out[["Preço"]])
    out[table_cols[1]] <- format_number(out[[table_cols[1]]], prefix="")
    
    DT::datatable(
        out, 
        style="bootstrap4", 
        options=list(
            lengthMenu=list(c(15, -1), c("15", "All")),
            pageLength=15
        )
    )
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
