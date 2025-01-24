library(stringr)
suppressMessages(library(plotly))
library(reshape2)
#source(here::here("frontend", "scripts", "update_prices.R"))

PRICES_RDS_PATH <- here::here("backend", "data", "prices.Rds")
VRAY5_BENCH_PATH <- here::here("backend", "data", "vray5_benchmarks.csv")
TH_RASTER_PERF_PATH <- here::here("backend", "data", "tomshardware_raster_avg_fps.csv")
TH_RT_PERF_PATH <- here::here("backend", "data", "tomshardware_rt_avg_fps.csv")
PRODUCTS_SHEET_PATH <- here::here("backend", "data", "prods.xlsx")

######## Primary data sets ########
PRICES <- readRDS(PRICES_RDS_PATH)
RASTER <- read.csv(TH_RASTER_PERF_PATH)
RAYTRC <- read.csv(TH_RT_PERF_PATH)
BLENDR <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="blender")
VIDEOS <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="videos")
GENRAI <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="gen_ai")
RAY5VD <- read.csv(VRAY5_BENCH_PATH)[, c("model", "score")]

# Eliminate stores with non-representative prices
multi_grep <- function(
        patterns, x, ignore.case = FALSE, perl = FALSE,
        value = FALSE, fixed = FALSE,
        useBytes = FALSE, invert = FALSE) {
    output <- c()
    for (term in patterns) {
        new_elems <- grep(
            pattern=term, x=x,
            ignore.case=ignore.case, perl=perl,
            value=value, fixed=fixed,
            useBytes=useBytes, invert=invert)
        output <- append(output, new_elems)
    }
    output
}

foreign_stores <- unique(multi_grep(
    c(
        "AliExpress", "Amazon.com.br - Seller", "Amazon.com.br - Retail",
        "B&H Photo-Video-Audio", "eBay", "Shopee", "Smart Info Store",
        "swsimports", "Tiendamia", "Techinn", "Nissei"
    ),
    PRICES$Store,
    ignore.case=TRUE,
    value=TRUE
))
# foreign_stores <- c(
#     "Amazon.com.br - Seller", "AliExpress.com", "Smart Info Store", 
#     "Tiendamia.com.br", "Shopee", "Techinn.com", "Amazon.com.br - Retail", 
#     "swsimports.com.br", "B&H Photo-Video-Audio", "eBay", "eBay - pluto-house",
#     "eBay - imicros", "eBay - mktllc", "eBay - mujitech3", "aliexpress.com",
#     "AliExpress", "AliExpress.com - ...", "AliExpress-4914015399",
#     "AliExpress.com - AliExpress-4449900785", "Nissei"
# )

used_stores <- c(
    "", "Enjoei.com", "MeuGameUsado", "Ledebut", "bringIT", "Mercado Livre", 
    "Black Friday", "4Gamers", "Site Oficial", "Rhr Cosméticos",
    "Portal Celular", "Nat Vita Suplementos", "ProGaming Computer",
    "Login Informática", "Gi Eletronica", "Bontempo", "Ka Eletronicos",
    "B&H Photo-Video-Audio", "Luck Oficial", "XonGeek", "Promotop",
    "Atacado Connect", "Fun4kids", "Luck Oficial", "Gi Ferretti Comercio"
)
# Eliminate unavailable or badly priced GPUs
unavailable_chips <- c(
    "Geforce Rtx 3090 Ti", "Geforce Rtx 3090", "Geforce Rtx 3080 Ti",
    "Geforce Rtx 3080", "Geforce Rtx 3070 Ti", "Geforce Rtx 3070",
    "Geforce Rtx 3060 Ti Gddr6X", "Geforce Rtx 3060 Ti", "Geforce Rtx 4080",
    "Radeon Rx 6900 Xt", "Radeon Rx 6800 Xt", "Radeon Rx 6800"
)
if (nrow(PRICES) > 0) 
    PRICES <- PRICES[
        !(PRICES$Store %in% c(foreign_stores, used_stores)), ]
#!(PRICES$ProductName %in% unavailable_chips)

######## Manipulation functions ########
indexr_data <- function(price_table=PRICES, group_for_week=FALSE) {
    ### create index data
    date <- price_table$Date
    price <- price_table$Price
    chip <- price_table$ProductName
    
    # Add week data
    week <- as.POSIXct(date, tz=Sys.timezone()) |> 
        cut.POSIXt(breaks="week", labels=FALSE)
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
        
        index_table_wide <- dcast(
            index_table, 
            Semana + Dia ~ Chip, 
            value.var="Melhor preço"
        )
        #rownames(index_table_wide)
        
        base_week <- index_table_wide$Semana |> min()
        base_value <- index_table[index_table$Semana == base_week, ] |>
            _[["Melhor preço"]] %>% mean()
        
        ignore_cols <- c("Semana", "Dia")
        use_cols <- index_table$Chip |> unique()
        
        cum_pdiff <- function(x) {
            cumsum(diff(x)/x[-length(x)])
        }
        
        scaled_index_table_wide <- sapply(
            index_table_wide[, use_cols], 
            cum_pdiff)
        scaled_index_table_wide[is.na(scaled_index_table_wide)] <- 0
        
        price_index <- rowMeans(scaled_index_table_wide, na.rm=TRUE)
        price_index <- c(0, price_index)
        price_index <- base_value + (base_value*price_index)
        
        index_table <- aggregate(price, list(week, date), FUN=mean) |> 
            setNames(c("Semana", "Dia", "Indice"))
        index_table[["Indice"]] <- price_index
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
        prices_table$Semana >= max(prices_table$Semana)-1)

    redundant_mask <- !(prices_table$Chip |> duplicated(fromLast=TRUE))
    prices_table <- prices_table[redundant_mask, ]
    
    
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
        current[["Melhor preço"]], 
        by = list(current[["Chip"]]), 
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
    
    out <- out[order(out[["Variação do preço"]]), ]
    
    out[["Variação do preço"]] <- format_number(
        out$`Variação do preço`, 
        prefix="", 
        suffix="%"
    )
    row.names(out) <- NULL
    return(out)
}

prodcut_price_history <- function(product_name) {
    PRICES
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
