library(forecast)
library(Benchmarking)
library(bslib)
library(bsicons)
library(htmltools)
library(DT)
library(flexdashboard)

cores <- c(
    bg = "#222222", fg = "#E0E0E0", 
    main = "#69B57E", second = "#FD5D63", third = "#63A2BB")
palette_greens <- c(
    "#70AFA5", "#B7E0D5", "#9DD3B0", "#80C68C",
    "#ABE598", "#92B67F", "#A4C79C", "#C4EFBB"
)
palette_blues <- c(
    "#548FC9", "#6FACBF", "#75C7D0", "#53BAB3",
    "#A3CCE0", "#7F90C8", "#ACA3CC", "#948ECD"
)
palette_reds <- c(
    "#C85A49", "#CF766D", "#E37748", "#CD6132",
    "#D69B5C", "#F4C398", "#CCA688", "#DFB77A"
)

# Dinamically generate headers for tabsets
catHeader <- function(text = "", level = 3) {
    cat(paste0(
        "\n\n", 
        paste(rep("#", level), collapse = ""), 
        " ", text, "\n\n")
    )
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
        axis.ticks=element_line(color=cores["bg"]),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.background=element_rect(fill=cores["bg"], size=0.5, linetype="solid", colour ="darkgray"),
        legend.text=element_text(colour=cores["fg"])
    )
}

#Create a table with latest best prices observed
best_prices_table <- function(product_names) {
    #' @title Create a recent best prices table
    #' @description Generate a `DT::datatable()` with the recent best prices for
    #' the selected products
    #' @param product_names Character Vector containing the product names to
    #' be used
    url <- "https://cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese.json"
    opts <- list(
        lengthMenu=list(c(17, -1), c("17", "All")), 
        pageLength=17, language=list(url=url)
    )
    df <- price_by_date(product_names)
    last_obs_date <- aggregate(df$Dia, list(df$Chip), FUN=max) |>
        setNames(c("Chip", "Dia"))
    out <- merge(df, last_obs_date)[, c("Dia", "Nome", "Preço", "Loja")]
    return(DT::datatable(out, , style="bootstrap4", options=opts))
}


plot_multiple_prices <- function(product_names, palette="Greens") {
    #' @title Plot the price history of multiple products in a single canvas
    #' @description Generate a plotly plot with project's default theme and
    #' a predefined color palette.
    #' @param product_names Character Vector containing the product names to
    #' be used
    #' @param palette Character with the palette name
    #' @note Values of `product_names` must be present in 
    #' `PRICES$ProductName |> unique()`.
    #' Value of `palette` must be one of RColorBrewer's palettes
    df <- price_by_date(product_names=product_names, group_by_week=TRUE)
    p <- ggplot(df, aes(x=Dia, y=Preço, color=Chip)) +
        geom_line(linewidth=1.2) + geom_point(size=4) +
        scale_color_brewer(palette=palette) +
        labs(x=NULL, y=NULL) +
        plot_theme()
    ggplotly(p) |>
        config(displayModeBar=FALSE) |>
        layout(margin=list(t=0, b=0, l=0, r=0))
}

# Plot indexer
plot_indexr <- function() {
    index_table <- index_data[-nrow(index_data),]
    
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
plot_perf_scatter <- function(
        dataset=price_raster_perf, preset="fhd_ultra", 
        threshold=NULL, low_threshold=NULL) {
    discrete_palette <- c("#63A2BB", "#69B57E", "#FD5D63")
    
    # Standard names to `dataset`
    curr_dataset <- data.frame(
        Chip = dataset$model,
        Preço = dataset$`Melhor preço`,
        Performance = dataset[[preset]],
        Família = dataset$chip_family
    )
    
    clean_dataset <- na.omit(curr_dataset)
    
    # Metadata for recommendations
    efficient <- dea.direct( 
        clean_dataset$Preço, 
        clean_dataset$Performance,
        DIRECT=1, RTS="fdh")$eff == 1
    
    if (is.null(low_threshold))
        min_perf <- clean_dataset$Performance > 0
    else
        min_perf <- clean_dataset$Performance > low_threshold
    
    if (is.null(low_threshold))
        ideal_perf <- clean_dataset$Performance > 0
    else
        ideal_perf <- clean_dataset$Performance > threshold
    
    price_limit <- quantile(clean_dataset$Preço, probs=c(.33, .66))
    
    efficient_chips <- clean_dataset$Chip[efficient]
    
    low_budget_recom <- clean_dataset$Chip[
        efficient & 
        min_perf & 
        (clean_dataset$Preço <= price_limit[1])
    ]
    average_recom <- clean_dataset$Chip[
        efficient & 
        ideal_perf &
        (clean_dataset$Preço > price_limit[1]) &
        (clean_dataset$Preço <= price_limit[2])
    ]
    high_end_recom <-  clean_dataset$Chip[
        efficient & 
        ideal_perf &
        (clean_dataset$Preço > price_limit[2])
    ]
    
    # Recommendation cards
    recommends <- list(
        low=value_box(
            title = "",
            value = "Opções para o orçamento mais apertado",
            theme_color = "primary",
            showcase = bs_icon("wallet2"),
            !!!lapply(low_budget_recom, function(x) tags$li(x |> toupper()))
        ),
        mid=value_box(
            title = "",
            value = "Desempenho satisfatório, custo moderado",
            theme_color = "success",
            showcase = bs_icon("hand-thumbs-up"),
            !!!lapply(average_recom, function(x) tags$li(x |> toupper()))
        ),
        hi=value_box(
            title = "",
            value = "Total desempenho, poder sem limites!",
            theme_color = "danger",
            showcase = bs_icon("rocket-takeoff"),
            !!!lapply(high_end_recom, function(x) tags$li(x |> toupper()))
        )
    )

    # Price x Performance plot
    p <- ggplot(
        curr_dataset, 
        aes(x=Preço, y=Performance, color=Família)) +
        geom_line(
            data=~subset(curr_dataset, Chip %in% efficient_chips),
            linewidth=1.2, color=cores["fg"]) +
        geom_point(mapping=aes(group=Chip), size=4, alpha=.8) +
        scale_color_manual(values=discrete_palette) +
        labs(x="Preço (R$)", y="Desempenho") + 
        theme(legend.position="none") +
        plot_theme()
    
    if (!is.null(threshold)){
        p <- p + 
            geom_hline(
                yintercept=threshold, 
                color=cores["fg"], linetype="dotted")
    }
    
    p <- plotly::ggplotly(p) %>%
        config(displayModeBar=FALSE) %>%
        layout(
            margin=list(t=0, b=0, l=0, r=0), 
            autosize=TRUE
        )
    
    # Card with Value-Boxes and Plot
    return(list(recommends, p))
}

plot_perf_table <- function(
        dataset=price_raster_perf, preset="fhd_ultra", table_cols) {
    
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
            pageLength=15,
            language=list(url="https://cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese.json")
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
