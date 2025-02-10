library(stringr)
suppressMessages(library(plotly))
library(reshape2)

PRICES_RDS_PATH     <- here::here("backend", "data", "prices.Rds")
VRAY5_BENCH_PATH    <- here::here("backend", "data", "vray5_benchmarks.csv")
TH_RASTER_PERF_PATH <- here::here("backend", "data", "tomshardware_raster_avg_fps.csv")
TH_RT_PERF_PATH     <- here::here("backend", "data", "tomshardware_rt_avg_fps.csv")
PRODUCTS_SHEET_PATH <- here::here("backend", "data", "prods.xlsx")


######## Funções auxiliares ########
multi_grep <- function(
        patterns, x, ignore.case = FALSE, perl = FALSE,
        value = FALSE, fixed = FALSE,
        useBytes = FALSE, invert = FALSE) {
    #' @title Apply multiple patterns with `grep`
    #' @description Search for multiples patterns along a given character vector x.
    #' @param patterns Character vector containing regular expressions to be matched
    #' @note Every other parameter should be interpreted exactly the same as in [grep](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/grep) function.
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

group_by_week_ <- function(dataset, date_col, factor_col) {
    #'
}
########


######## Conjuntos de dados principais ########
#' [PRICES] Data Frame com os dados de preços
#' 
#' Colunas:
#' ProductId[integer]: Número identificador de produto
#' NameId[integer]: Número identificador de nome de produto
#' ProductName[character]: Nome do produto indicado por `NameId` (Exemplo: Geforce RTX 4090)
#' ProductModel[character]: Nome do modelo referente à `ProductId` (Exemplo: TUF Gaming)
#' ProductBrand[character]: Nome da marca referente à `ProductId` (Exemplo: Asus)
#' ProductFilters[character]: Nomes filtrados da pesquisa referente à `ProductId`
#' Created[datetime]: Data e hora que os preços do produto começaram a ser monitorados referente à `ProductId`
#' LastUpdate[datetime]: Data e hora da última vez que um preço foi coletado com sucesso referente à `ProductId`
#' PriceId[integer]: Identificador de cada preço coletado, único para cada linha deste data frame
#' Date[datetime]: Momento em que este preço foi coletado, referente à `PriceId`
#' Currency[character]: Simbolo da moeda no rótulo de preço, referente à `PriceId`
#' Price[double]: Valor do preço no registro de preço, referente à `PriceId`
#' Name[character]: Título do anúncio do produto no resultado da pesquisa, referente à `PriceId`
#' Store[character]: Nome da loja anunciante no resultado da pesquisa, referente à `PriceId`
#' Url[character]: Endereço web do produto anunciado no resultado da pesquisa, referente à `PriceId`
PRICES <- readRDS(PRICES_RDS_PATH)
if (nrow(PRICES) == 0) stop("No price data available, cannot proceed with data setup.")

#' [RASTER] Dados de desempenho em jogos rasterizados (FPS médio) em um conjunto de jogos rasterizados
#'
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' fhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Ultra"
#' fhd_medium[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Médio"
#' qhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 2560x1440, com preset "Ultra"
#' uhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 3840x2160, com preset "Ultra"
#' specs[character]: Informações das especificações da placa de vídeo
RASTER <- read.csv(TH_RASTER_PERF_PATH)

#' [RAYTRC] Dados de desempenho em jogos rasterizados com efeitos de Ray Tracing adicionados (FPS médio) em um conjunto de jogos
#'
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' fhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Ultra"
#' fhd_medium[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Médio"
#' qhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 2560x1440, com preset "Ultra"
#' uhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 3840x2160, com preset "Ultra"
#' specs[character]: Informações das especificações da placa de vídeo
RAYTRC <- read.csv(TH_RT_PERF_PATH)

#' [BELNDR] Dados de desempenho no benchmark do Blender
#' 
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' CUDA[double]: Pontuação usando o método de computação CUDA
#' HIP[double]: Pontuação usando o método de computação HIP
#' OneAPI[double]: Pontuação usando o método de computação OneAPI
BLENDR <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="blender")

#' [VIDEOS] Dados de desempenho em renderização de vídeo
#' 
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' redshift_seconds[double]: Tempo de renderização do vídeo de referência em segundos
#' redshift_score[double]: Pontuação de desempenho baseada no tempo de renderização, 100.000/`redshift_seconds`
VIDEOS <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="videos")

#' [GENRAI] Dados de desempenho com IA generativa
#' 
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' stable_diffusion_512p_img_per_sec[double]: Quantidade de imagens em resolução 512x512 geradas por minuto com Stable Diffusion v1-5
#' stable_diffusion_768p_img_per_sec[double]: Quantidade de imagens em resolução 768x768 geradas por minuto com Stable Diffusion v1-5
GENRAI <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="gen_ai")

#' [RAY5VD] Pontuação de Benchmark no Ray-5
#' 
#' Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' score[double]: Pontuação no benchmark de renderização próprio do software
RAY5VD <- read.csv(VRAY5_BENCH_PATH)[, c("model", "score")]
########


######## Conjuntos de dados assistentes ########
#' [COUNTS] Data Frame de contagens
#'
#' Colunas:
#' Name[character]: Nome do produto
#' PricesCount[int]: Quantidade de preços coletados para este `Name`
#' ProductsCount[int]: Quantidade de produtos cadastrados para este `Name`
#' PricesPerProduct[double]: Razão entre PricesCount e ProductsCount
COUNTS <- aggregate(
    x=PRICES[, c("PriceId", "ProductId")],
    by=list(PRICES$ProductName),
    FUN=function(x)length(unique(x))
)
names(COUNTS) <- c("Name", "PricesCount", "ProductsCount")
COUNTS$PricesPerProduct <- COUNTS$PricesCount / COUNTS$ProductsCount

#' [superseded_chips] Nome dos produtos substituiídos por outro modelo
superseded_chips <- c(
    "Geforce Rtx 3090 Ti", "Geforce Rtx 3090",
    "Geforce Rtx 3080 Ti", "Geforce Rtx 3080",
    "Geforce Rtx 3070 Ti", "Geforce Rtx 3070",
    "Geforce Rtx 3060 Ti Gddr6X", "Geforce Rtx 3060 Ti",
    "Geforce Rtx 4080",
    "Geforce Rtx 4070", "Geforce Rtx 4070 Ti",
    "Radeon Rx 6900 Xt",
    "Radeon Rx 6800 Xt", "Radeon Rx 6800"
)

#' [foreign_stores] Nomes de lojas estrangeiras
foreign_stores <- unique(multi_grep(
    c(
        "AliExpress", "Amazon.com.br - Seller", "Amazon.com.br - Retail",
        "B&H Photo-Video-Audio", "eBay", "Shopee", "Smart Info Store",
        "swsimports", "Tiendamia", "Techinn", "Nissei", "Ubuy"
    ),
    PRICES$Store,
    ignore.case=TRUE,
    value=TRUE
))

#' [used_stores] Nomes de lojas que vendem produtos com preços de usado
used_stores <- c(
    "", "Enjoei.com", "MeuGameUsado", "Ledebut", "bringIT", "Mercado Livre", 
    "Black Friday", "4Gamers", "Site Oficial", "Rhr Cosméticos",
    "Portal Celular", "Nat Vita Suplementos", "ProGaming Computer",
    "Login Informática", "Gi Eletronica", "Bontempo", "Ka Eletronicos",
    "B&H Photo-Video-Audio", "Luck Oficial", "XonGeek", "Promotop",
    "Atacado Connect", "Fun4kids", "Luck Oficial", "Gi Ferretti Comercio"
)
########


######## Geradores de conjuntos de dados ########
price_by_date <- function(product_names=c(), group_by_week=FALSE) {
    #' @title Generate a Data Frame of best prices by date
    #' @description Table in long format with the best prices for every
    #' "ProductName" selected by date
    #' @param product_names Character vector, should contain a list of the
    #' product names desired, if is an empty vector, will use all products available
    #' @param group_by_week Boolean, `False` if should return all available dates
    
    cols <- c("Date", "Price", "ProductBrand", "ProductName", "ProductModel", "Store")
    if (length(product_names) == 0) {
        df <- PRICES[, cols]
    } else {
        df <- PRICES[(PRICES$ProductName %in% product_names), cols]
    }
    # datetime do date
    df$Date <- as.Date(format(df$Date, format="%Y-%m-%d"))

    # best price by combination of Date and ProductName
    best_prices <- aggregate(x=df$Price, by=list(df$Date, df$ProductName), FUN=min) |>
        setNames(c("Date", "ProductName", "Value"))

    # get first row index where the best price is found
    # for each combination of Date and ProductName
    sel_rows <- c()
    for (idx in 1:nrow(best_prices)) {
        date_mask <-           df$Date == best_prices[idx, "Date"]
        product_mask <- df$ProductName == best_prices[idx, "ProductName"]
        price_mask <-         df$Price == best_prices[idx, "Value"]

        rows <- df[date_mask & product_mask & price_mask, ] |> rownames()
        sel_rows <- append(sel_rows, rows)
    }

    # reset row and column names
    colnames(df) <- c("Dia", "Preço", "Brand", "Chip", "Model", "Loja")
    rownames(df) <- NULL
    # select rows and columns
    df["Nome"] <- paste(df$Brand, df$Chip, df$Model)
    df <- df[sel_rows, c("Dia", "Chip", "Preço", "Nome", "Loja")]

    if (!group_by_week) {
        return(df)
    } else {
        stop("Not implemented: group_by_week=`TRUE`")
        #return(group_by_week_(df, "Dia", "Chip"))
    }
}

indexr_data <- function(price_table=PRICES, group_by_week=FALSE) {
    #' @title Generate datasets of best prices by date
    #' @description If multiple products are available in `price_table`, 
    #' will create a "Price Index" data, or a simple "best prices by date"
    #' dataset otherwise.
    #' @param price_table Data Frame, either full `PRICES` or a subset of it
    #' over `PRICES$NameId` or `PRICES$ProductName`.
    #' @param group_by_week Boolean, `False` if should return all available dates
    ### create index data
    date <- price_table$Date
    price <- price_table$Price
    chip <- price_table$ProductName

    # Add week data
    week <- as.POSIXct(date, tz=Sys.timezone()) |> 
        cut.POSIXt(breaks="week", labels=FALSE)
    #week <- strftime(date, format="%V", tz=Sys.timezone()) |> as.numeric()

    # Best price for every GPU per week
    index_table <- aggregate(price, list(week, chip), FUN=min) |>
        setNames(c("Semana", "Chip", "Melhor preço"))

    # Adding week last dates
    dates_table <- aggregate(date, list(week), FUN=max) |>
        setNames(c("Semana", "Dia"))
    index_table <- merge(index_table, dates_table, by="Semana")

    if (group_by_week) {
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
PRICES <- PRICES[
    !(PRICES$Store %in% c(foreign_stores, used_stores)) &
    !(PRICES$ProductName %in% superseded_chips), ]
index_data <- indexr_data(group_by_week=TRUE)
weekly_best_prices <- indexr_data()
price_raster_perf <- perf_data()
price_rt_perf <- perf_data(RAYTRC)
price_blender_perf <- perf_data(BLENDR)
price_videos_perf <- perf_data(VIDEOS)
price_vray5_perf <- perf_data(RAY5VD)
price_gen_ai_perf <- perf_data(GENRAI)
