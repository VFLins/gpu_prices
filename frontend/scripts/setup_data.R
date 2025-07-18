library(stringr)
suppressMessages(library(plotly))
library(reshape2)

PRICES_RDS_PATH     <- here::here("backend", "data", "prices.Rds")
VRAY5_BENCH_PATH    <- here::here("backend", "data", "vray5_benchmarks.csv")
TECHPOWERUP_PERF    <- here::here("backend", "data", "techpowerup_avg_fps.xlsx")
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
    #' @param patterns Character Vector containing regular expressions to be matched
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

group_by_week_ <- function(
        df,
        date_colname="Dia",
        factor_colname="Chip",
        value_colname="Preço") {
    #' @title Group daily or semi-daily data to weekly
    #' @param df Data Frame containing the data to be grouped
    #' @param date_colname Character, column name of the date values
    #' @param factor_colname Character, column name of categorical data that
    #' should be preserved in the resulting Data Frame
    #' @param value_colname Character, column name with the values
    date <- df[[date_colname]]
    fact <- df[[factor_colname]]
    val <- df[[value_colname]]
    week_id <- as.POSIXct(date, tz=Sys.timezone()) |>
        cut.POSIXt(breaks="week", labels=FALSE)

    value_table <- aggregate(val, list(week_id, fact), FUN=min) |>
        setNames(c("IdSemana", factor_colname, value_colname))
    date_table <- aggregate(date, list(week_id, fact), FUN=max) |>
        setNames(c("IdSemana", factor_colname, date_colname))
    merged_tables <- merge(date_table, value_table, no.dups=TRUE)
    return(merged_tables[order(merged_tables$IdSemana), ])
}


last_index <- function(df, as_numeric=TRUE) {
    idx_vector <- rownames(df)
    if (is.null(idx_vector))
        return(NA)
    last_item <- idx_vector[length(idx_vector)]
    if (as_numeric)
        return(as.numeric(last_item))
    else
        return(last_item)
}


cleanse_performance_data <- function(df) {
    #' @title Keep only the highest performance score when there are multiple available
    #' @description Expects a "model" column, where it will detect the duplicated data from
    #' @param df Data Frame containing the data to be cleansed
    duplicated_model_names <- df[duplicated(df$model), "model"]
    if (length(duplicated_model_names) == 0)
        return(df)
    for (model_name in duplicated_model_names) {
        tmp <- df[df$model==model_name, ]
        new_row_id <- as.character(last_index(df) + 1)
        for (col in colnames(tmp)) {
            df[new_row_id, col] <- max(tmp[[col]], na.rm=TRUE)
        }
        df <- df[!(rownames(df) %in% rownames(tmp)), ]
    }
    return(df)
}


append_data_source <- function(orig, new, match_colname="model") {
    #' @title Adiciona dados de `new` que estão ausentes em `orig`
    #' @param orig data.frame com os dados que serão preservados
    #' @param new data.frame com possíveis novos dados
    #' @param match_colname character indicando o nome de uma coluna que ambos os
    #'   data frames possuem em comum, deve possuir valores únicos que identificam
    #'   uma unidade de informação
    #' @note Precisa que exista uma coluna com nome indicado em `match_colname`
    #'   em ambos os conjuntos de dados `orig` e `new`, não fará nada se não econtrar
    new_data_mask <- !(tolower(new[[match_colname]]) %in% tolower(orig[[match_colname]]))
    if (sum(new_data_mask) == 0)
        return(orig)
    new <- new[new_data_mask, ]

    common_columns_mask <- (colnames(orig) %in% colnames(new))
    cols <- colnames(orig)[common_columns_mask]

    for (new_data in new[[match_colname]]) {
        new_index <- as.character(last_index(orig) + 1)
        for (column in cols) {
            orig[new_index, column] <- new[new[[match_colname]]==new_data, column]
        }
    }
    return(orig)
}

#perf_cols <- c("model", "fhd_medium", "fhd_ultra", "qhd_ultra", "uhd_ultra")
#price_raster_perf[, perf_cols] <- cleanse_performance_data(price_raster_perf[, perf_cols])

######## Conjuntos de dados principais ########
#' [PRICES] Data Frame com os dados de preços
#' 
#' @section Colunas: 
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
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' fhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Ultra"
#' fhd_medium[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Médio"
#' qhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 2560x1440, com preset "Ultra"
#' uhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 3840x2160, com preset "Ultra"
#' specs[character]: Informações das especificações da placa de vídeo
RASTER <- read.csv(TH_RASTER_PERF_PATH)
RASTER$model <- gsub("Intel ", "", RASTER$model)
RASTER <- RASTER |>
    append_data_source(readxl::read_excel(TECHPOWERUP_PERF, sheet="raster"))

#    merge(x=_, y=readxl::read_excel(TECHPOWERUP_PERF, sheet="raster"), all=TRUE) |>
#    cleanse_performance_data()
#temp <- readxl::read_excel(TECHPOWERUP_PERF, sheet="raster")
#mask <- !(tolower(temp$model) %in% tolower(RASTER$model))
#RASTER <- rbind(RASTER[, colnames(temp)], temp[mask, ])

#' [RAYTRC] Dados de desempenho em jogos rasterizados com efeitos de Ray Tracing adicionados (FPS médio) em um conjunto de jogos
#'
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' fhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Ultra"
#' fhd_medium[double]: Medida de desempenho (FPS médio) em resolução 1920x1080, com preset "Médio"
#' qhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 2560x1440, com preset "Ultra"
#' uhd_ultra[double]: Medida de desempenho (FPS médio) em resolução 3840x2160, com preset "Ultra"
#' specs[character]: Informações das especificações da placa de vídeo
RAYTRC <- read.csv(TH_RT_PERF_PATH)

#' [BELNDR] Dados de desempenho no benchmark do Blender
#' 
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' CUDA[double]: Pontuação usando o método de computação CUDA
#' HIP[double]: Pontuação usando o método de computação HIP
#' OneAPI[double]: Pontuação usando o método de computação OneAPI
BLENDR <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="blender")

#' [VIDEOS] Dados de desempenho em renderização de vídeo
#' 
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' redshift_seconds[double]: Tempo de renderização do vídeo de referência em segundos
#' redshift_score[double]: Pontuação de desempenho baseada no tempo de renderização, 100.000/`redshift_seconds`
VIDEOS <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="videos")

#' [GENRAI] Dados de desempenho com IA generativa
#' 
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' stable_diffusion_512p_img_per_sec[double]: Quantidade de imagens em resolução 512x512 geradas por minuto com Stable Diffusion v1-5
#' stable_diffusion_768p_img_per_sec[double]: Quantidade de imagens em resolução 768x768 geradas por minuto com Stable Diffusion v1-5
GENRAI <- readxl::read_excel(PRODUCTS_SHEET_PATH, sheet="gen_ai")

#' [RAY5VD] Pontuação de Benchmark no Ray-5
#' 
#' @section Colunas:
#' model[character]: Nome do produto, equivalente aos valores únicos de `PRICES$ProductName`
#' score[double]: Pontuação no benchmark de renderização próprio do software
RAY5VD <- read.csv(VRAY5_BENCH_PATH)[, c("model", "score")]


######## Conjuntos de dados assistentes ########
#' [COUNTS] Data Frame de contagens
#'
#' Colunas:
#' Name[character]: Nome do produto
#' PricesCount[int]: Quantidade de preços coletados para este `Name`
#' ProductsCount[int]: Quantidade de produtos cadastrados para este `Name`
#' PricesPerProduct[double]: Razão entre PricesCount e ProductsCount
COUNTS <- aggregate(
    x=PRICES[, c("Id", "ProductId")],
    by=list(PRICES$ProductName),
    FUN=function(x)length(unique(x))
)
names(COUNTS) <- c("Name", "PricesCount", "ProductsCount")
COUNTS$PricesPerProduct <- COUNTS$PricesCount / COUNTS$ProductsCount

#' [superseded_chips] Nome dos produtos substituiídos por outro modelo mais recente
superseded_chips <- PRICES[!is.na(PRICES$SupersededBy), "ProductName"] |> unique()

#' [foreign_stores] Nomes de lojas estrangeiras
foreign_stores <- unique(multi_grep(
    c(
        "AliExpress", "Amazon.com.br - Seller", "Amazon.com.br - Retail",
        "B&H Photo-Video-Audio", "eBay", "Shopee", "Smart Info Store",
        "swsimports", "Tiendamia", "Techinn", "Nissei", "Ubuy", "Realschematic",
        "Chic Cart", "Microless.com", "Bitworks", "7.", "5.",
        "Shop de cooler & Informática em Geral", "mercadolivre.com.br"
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
    "Atacado Connect", "Fun4kids", "Luck Oficial", "Gi Ferretti Comercio",
    "phatron.com.br", "NMS Comércio", "Zumaia Acessórios", "Glacon Informática"
)

#' [trusted_stores] Nomes de lojas nacionais que historicamente mostram bons preços de produtos novos
trusted_stores = c(
    "Pichau", "Terabyteshop", "KaBuM!",
    "OctoShop BR", "Patoloco.com.br", "Gigantec"
)

PRICES <- PRICES[
    !(PRICES$Store %in% c(foreign_stores, used_stores)) &
    !(PRICES$ProductName %in% superseded_chips), 
]

#' [available_nvidia_chips] GPUs da Nvidia disponíveis no mercado
available_nvidia_chips <- grep("Geforce", unique(PRICES$ProductName), value=TRUE) |>
    setdiff(superseded_chips)

#' [available_amd_chips] GPUs da AMD disponíveis no mercado
available_amd_chips <- grep("Radeon", unique(PRICES$ProductName), value=TRUE) |>
    setdiff(superseded_chips)

#' [available_intel_chips] GPUs da Intel disponíveis no mercado
available_intel_chips <- grep("Arc", unique(PRICES$ProductName), value=TRUE) |>
    setdiff(superseded_chips)

#' [entry_available_nvidia_chips] GPUs de entrada da Nvidia disponíveis no mercado
entry_available_nvidia_chips <- PRICES[
    (PRICES$ProductName %in% available_nvidia_chips) &
    (PRICES$Price <= quantile(PRICES$Price, .3)),
    "ProductName"
] |> unique()

#' [entry_available_amd_chips] GPUs de entrada da Nvidia disponíveis no mercado
entry_available_amd_chips <- PRICES[
    (PRICES$ProductName %in% available_amd_chips) &
    (PRICES$Price <= quantile(PRICES$Price, .3)),
    "ProductName"
] |> unique()

#' [entry_available_intel_chips] GPUs de entrada da Nvidia disponíveis no mercado
entry_available_intel_chips <- PRICES[
    (PRICES$ProductName %in% available_intel_chips) &
    (PRICES$Price <= quantile(PRICES$Price, .3)),
    "ProductName"
] |> unique()

#' [midend_available_nvidia_chips] GPUs intermediárias da Nvidia disponíveis no mercado
midend_available_nvidia_chips <- PRICES[
    (PRICES$ProductName %in% available_nvidia_chips) &
    (PRICES$Price > quantile(PRICES$Price, .3)) &
    (PRICES$Price <= quantile(PRICES$Price, .6)),
    "ProductName"
]

#' [midend_available_amd_chips] GPUs intermediárias da AMD disponíveis no mercado
midend_available_amd_chips <- PRICES[
    (PRICES$ProductName %in% available_amd_chips) &
    (PRICES$Price > quantile(PRICES$Price, .3)) &
    (PRICES$Price <= quantile(PRICES$Price, .6)),
    "ProductName"
]

#' [midend_available_intel_chips] GPUs intermediárias da Intel disponíveis no mercado
midend_available_intel_chips <- PRICES[
    (PRICES$ProductName %in% available_intel_chips) &
    (PRICES$Price > quantile(PRICES$Price, .3)),
    "ProductName"
] |> unique()


#' [highend_available_nvidia_chips] GPUs high-end da Nvidia disponíveis no mercado
highend_available_nvidia_chips <- available_nvidia_chips |>
    setdiff(entry_available_nvidia_chips) |>
    setdiff(midend_available_nvidia_chips)

#' [highend_available_amd_chips] GPUs high-end da Nvidia disponíveis no mercado
highend_available_amd_chips <- available_amd_chips |>
    setdiff(entry_available_amd_chips) |>
    setdiff(midend_available_amd_chips)

######## Geradores de conjuntos de dados ########
price_by_date <- function(product_names=c(), group_by_week=FALSE, hyperlinks=FALSE) {
    #' @title Generate a Data Frame of best prices by date
    #' @description Table in long format with the best prices for every
    #' "ProductName" selected by date
    #' @param product_names Character vector, should contain a list of the
    #' product names desired, if is an empty vector, will use all products available
    #' @param group_by_week Boolean, `False` if should return all available dates
    #' @param hyperlinks Boolean, `False` if shuld the store name column have html hyperlink
    #' @section Dataset returned: 
        #' 
    
    cols <- c("Date", "Price", "ProductBrand", "ProductName", "ProductModel", "Store", "Url")
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

    # translate column names
    colnames(df) <- c("Dia", "Preço", "Brand", "Chip", "Model", "Loja", "Url")
    # select rows and columns
    df["Nome"] <- paste(df$Brand, df$Chip, df$Model)
    if (hyperlinks) {
        df["Loja"] <- paste0("<a href='", df$Url, "'>", df$Loja, "</a>")
    }
    df <- df[sel_rows, c("Dia", "Chip", "Preço", "Nome", "Loja")]
    # reset rownames at the end to avoid problems with `sel_rows`
    rownames(df) <- NULL

    if (!group_by_week) {
        return(df)
    } else {
        return(group_by_week_(
            df=df,
            date_colname="Dia",
            factor_colname="Chip",
            value_colname="Preço"
        ))
    }
}

best_price_by_week <- function(chip_names=NULL, only_trusted=FALSE) {
    #' @title Cria uma série temporal com os melhores preços de cada semana
    #' @param chip_name vetor `character` com nomes presentes em PRICES$ProductName, ou
    #' `NULL` para usar todos os produtos
    #' @param only_trusted `bool` indicando se deve usar apenas preços das `trusted_stores`
    if (is.null(chip_names)) {
        slice <- PRICES
    } else {
        slice <- PRICES[(PRICES$ProductName %in% chip_names), ]
    }
    if (only_trusted) {
        slice <- slice[(slice$Store %in% trusted_stores), ]
    }

    chip <- slice$ProductName
    week <- as.POSIXct(slice$Date, tz=Sys.timezone()) |> 
        cut.POSIXt(breaks="week", labels=FALSE)

    best_prices <- aggregate(slice$Price, list(week, chip), FUN=min) |>
        setNames(c("Semana", "Chip", "Melhor preço"))
    dates_table <- aggregate(slice$Date, list(week), FUN=max) |>
        setNames(c("Semana", "Dia"))
    return(merge(dates_table, best_prices, by="Semana"))
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
        function(x) tools::toTitleCase(strsplit(x, " ")[[1]][1])
    )

    # Return model names to title case
    out$model <- tools::toTitleCase(out$model)
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
index_data <- indexr_data(group_by_week=TRUE)
weekly_best_prices <- indexr_data()
price_raster_perf <- perf_data() |> cleanse_performance_data()
price_rt_perf <- perf_data(RAYTRC)
price_blender_perf <- perf_data(BLENDR)
price_videos_perf <- perf_data(VIDEOS)
price_vray5_perf <- perf_data(RAY5VD)
price_gen_ai_perf <- perf_data(GENRAI)
