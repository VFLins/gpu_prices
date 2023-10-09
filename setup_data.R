library(stringr)
library(plotly)

######## Primary data sets ########
PRICES <- readRDS("data/prices.rds")
RASTER <- read.csv("data/tomshardware_raster_avg_fps.csv")
RAYTRC <- read.csv("data/tomshardware_rt_avg_fps.csv")

# Eliminate stores with non-representative prices
foreign_stores <- c(
    "Amazon.com.br - Seller", "AliExpress.com", "Smart Info Store", 
    "Tiendamia.com.br", "Shopee", "Techinn.com", "Amazon.com.br - Retail", 
    "swsimports.com.br")
used_stores <- c(
    "Enjoei.com", "MeuGameUsado", "Ledebut", "bringIT", "Mercado Livre")
if (nrow(PRICES) > 0) 
    PRICES <- PRICES[
        !(PRICES$Store %in% c(foreign_stores, used_stores)), ]

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

raster_data <- function(raster_table=RASTER) {
    raster_cols <- c("model", "fhd_medium", "fhd_ultra", "qhd_ultra", "uhd_ultra")
    prices_cols <- c("model", "Melhor preço", "Dia", "Semana")
    
    prices_table <- indexr_data()
    prices_table <- subset(
        prices_table, 
        prices_table$Semana == max(prices_table$Semana))
    
    prices_table["model"] <- tolower(prices_table$Chip)
    raster_table["model"] <- tolower(raster_table$model)
    raster_table["model"] <- gsub("intel ", "", raster_table$model)
    
    out <- merge(raster_table[, raster_cols], prices_table[, prices_cols])
    out["chip_family"] <- sapply(
        out$model, 
        function(x )strsplit(x, " ")[[1]][1])
    
    return(out)
}

######## Secondary datasets ########
index_data <- indexr_data(group_for_week=TRUE)
weekly_best_prices <- indexr_data()
price_raster_perf <- raster_data()
