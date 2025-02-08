library(txtplot)
PROJECT_ROOT = here::here()

#PRICES0 <- readRDS(here::here("backend", "data", "prices.Rds"))
source(here::here("frontend", "scripts", "setup_data.R"))

general_counts <- function() {
    #' `data.frame` de contagens de Produtos
    #' 
    #' Obtenha uma tabela com a contagens dos elementos relevantes de `PRICES$ProductName`.
    #' 
    #' @section Estrutura do `data.frame`:
    #' * Name[character]: Nome do produto
    #' * PricesCounts[integer]: Quantidade de preços coletados para produtos com este _Name_
    #' * ProductsCount[integer]: Quantidade de produtos cadastrados para este _Name_
    prices_counts <- aggregate(
        x=PRICES$PriceId,
        by=list(PRICES$ProductName),
        FUN=function(x)length(x)
    )
    colnames(prices_counts) <- c("Name", "PricesCount")

    products_counts <- aggregate(
        x=PRICES$ProductId,
        by=list(PRICES$ProductName),
        FUN=function(x)length(unique(x))
    )
    colnames(products_counts) <- c("Name", "ProductsCount")

    merged_counts <- merge(
        x=products_counts,
        y=prices_counts,
        by="Name"
    )
    return(merged_counts)
}


low_availability_chip_names <- function() {
    #' `c` Combinação de nomes de chips com baixa disponibilidade (experimental)
    #' 
    #' Cria um vetor com os valores de `PRICES$ProductName` que corresponde aos produtos com baixa disponibilidade no mercado atualmente
    counts_tbl <- general_counts()
    counts_tbl$PricesPerProduct <- counts_tbl$PricesCount / counts_tbl$ProductsCount

    availability_index <- sort(
        counts_tbl$PricesPerProduct
        - median(counts_tbl$PricesPerProduct)
        - sd(counts_tbl$PricesPerProduct) * 0.2
    )
    names(availability_index) <- counts_tbl$Name

    names(availability_index[availability_index < 0])
}

low_availability_chip_names()
# chip_names <- sort(unique(PRICES[, 3]))
# display_data <- c()
# for (name in chip_names) {
#     temp_df <- indexr_data(
#         PRICES[PRICES$ProductName==name, ]
#     )
#     new_obs <- {
#         sqrt(var(temp_df$`Melhor preço`)) /
#         mean(temp_df$`Melhor preço`)
#     }
#     display_data <- c(display_data, new_obs)
# }
# names(display_data) <- chip_names
# threshold <- mean(display_data)
# print("inconsistent price history")
# print(display_data[display_data > threshold])

# display_data <- c()
# for (name in chip_names) {
#     temp_df <- indexr_data(
#         PRICES[PRICES$ProductName==name, ]
#     )
#     new_obs <- {
#         max(diff(temp_df$Dia))
#     }
#     display_data <- c(display_data, new_obs)
# }
# names(display_data) <- chip_names
# threshold <- mean(display_data)
# print("low availability")
# print(display_data[display_data > threshold])
