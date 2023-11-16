library(stringr)
library(plotly)

source("routine/update_prices.r")

######## Primary data sets ########
PRICES <- readRDS("data/prices.rds")
RASTER <- read.csv("data/tomshardware_raster_avg_fps.csv")
RAYTRC <- read.csv("data/tomshardware_rt_avg_fps.csv")
BLENDR <- readxl::read_excel("data/prods.xlsx", sheet="blender")
VIDEOS <- readxl::read_excel("data/prods.xlsx", sheet="videos")
RAY5VD <- read.csv("data/vray5_benchmarks.csv")[, c("model", "score")]
GENRAI <- readxl::read_excel("data/prods.xlsx", sheet="gen_ai")

# Eliminate stores with non-representative prices
foreign_stores <- c(
    "Amazon.com.br - Seller", "AliExpress.com", "Smart Info Store", 
    "Tiendamia.com.br", "Shopee", "Techinn.com", "Amazon.com.br - Retail", 
    "swsimports.com.br", "B&H Photo-Video-Audio")
used_stores <- c(
    "Enjoei.com", "MeuGameUsado", "Ledebut", "bringIT", "Mercado Livre", 
    "Black Friday")
unavailable_chips <- c(
    "Geforce Rtx 3090 Ti", "Radeon Rx 6800 Xt"
                       )
if (nrow(PRICES) > 0) 
    PRICES <- PRICES[
        !(PRICES$Store %in% c(foreign_stores, used_stores)) & 
        !(PRICES$ProductName %in% unavailable_chips), ]

######## Manipulation functions ########
indexr_data <- function(price_table=PRICES, group_for_week=FALSE) {
    ### create index data
    date <- price_table$Date
    price <- price_table$Price
    chip <- price_table$ProductName
    
    # Add week data
    week <- as.POSIXct(date, tz=Sys.timezone()) |> 
        cut.POSIXt(breaks="week", labels=F)
    #week <- strftime(date, format="%V", tz=Sys.timezone()) |> as.numeric()
    
    # Best price for every GPU per week
    index_table <- aggregate(
        price, list(week, chip), FUN=min) |>
        setNames(c("Semana", "Chip", "Melhor preço"))
    
    # Adding week last dates
    dates_table <- aggregate(date, list(week), FUN=max) |>
        setNames(c("Semana", "Dia"))
    index_table <- merge(index_table, dates_table, by="Semana")
    
    if (group_for_week) {
        # Mean of best prices for each week
        price <- index_table$`Melhor preço`
        week <- index_table$Semana
        date <- index_table$Dia
        
        index_table <- aggregate(price, list(week, date), FUN=mean) |> 
            setNames(c("Semana", "Dia", "Indice"))
    }
    return(index_table)
}

perf_data <- function(perf_table=RASTER) {
    perf_cols <- names(perf_table)
    prices_cols <- c("model", "Melhor preço", "Dia", "Semana")
    
    # Select last best prices
    prices_table <- indexr_data()
    prices_table <- subset(
        prices_table, 
        prices_table$Semana == max(prices_table$Semana))
    
    # Padronize model names
    prices_table["model"] <- tolower(prices_table$Chip)
    perf_table["model"] <- tolower(perf_table$model)
    perf_table["model"] <- gsub("intel ", "", perf_table$model)
    
    # Merge performance and prices by model names
    out <- merge(perf_table[, perf_cols], prices_table[, prices_cols])
    out["chip_family"] <- sapply(
        out$model, 
        function(x )strsplit(x, " ")[[1]][1])
    
    return(out)
}

price_drops <- function(n_weeks, include_last_update=FALSE) {
    full_data <- indexr_data()
    last_week <- max(full_data$Semana)-1
    
    first <- subset(full_data, full_data$Semana == last_week-n_weeks+1)
    older <- subset(full_data, full_data$Semana >= last_week-n_weeks+1)
    current <- subset(full_data, full_data$Semana >= last_week)
    
    total_pdiff <- function(x) {
        percent_diff_decimal <- sum(diff(x)/x[-length(x)]) *100
    }
    
    percent_var <- aggregate(
        older$`Melhor preço`, 
        by = list(older$Chip), 
        FUN = total_pdiff
    )
    
    curr_price <- aggregate(
        current$`Melhor preço`, 
        by = list(current$Chip), 
        FUN = function(x) format_number(min(x))
    )
    first_price <- aggregate(
        first$`Melhor preço`, 
        by = list(first$Chip), 
        FUN = function(x) format_number(min(x))
    )
    
    out <- merge(percent_var, curr_price, by="Group.1") 
    out <- merge(out, first_price) |> 
        setNames(
            c(
                "Chip", "Variação do preço", 
                "Melhor preço atual", 
                paste("Melhor preço", (n_weeks-1)*7, "dias atrás")
            )
        )
    if (include_last_update){
        last_update <-  aggregate(
            PRICES$LastUpdate, 
            by = list(PRICES$ProductName), 
            FUN = max
        ) |> setNames(c("Chip", "Última atualização"))
        out <- merge(out, last_update, by="Chip")
    }
    
    out <- out[order(out$`Variação do preço`), ]
    
    out$`Variação do preço` <- format_number(
        out$`Variação do preço`, 
        prefix="", 
        suffix="%"
    )
    row.names(out) <- NULL
    return(out)
}

######## Secondary datasets ########
index_data <- indexr_data(group_for_week=TRUE)
weekly_best_prices <- indexr_data()
price_raster_perf <- perf_data()
price_rt_perf <- perf_data(RAYTRC)
price_blender_perf <- perf_data(BLENDR)
price_videos_perf <- perf_data(VIDEOS)
price_vray5_perf <- perf_data(RAY5VD)
price_gen_ai_perf <- perf_data(GENRAI)
