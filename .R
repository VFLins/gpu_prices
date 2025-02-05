library(txtplot)
PROJECT_ROOT = here::here()

#PRICES0 <- readRDS(here::here("backend", "data", "prices.Rds"))
source(here::here("frontend", "scripts", "setup_data.R"))

prices_counts <- sort(table(PRICES$ProductName))

prices_counts_percent <- (
    prices_counts / sum(prices_counts)
) *100

products_per_name <- aggregate(
    x=PRICES$ProductId,
    by=list(PRICES$ProductName),
    FUN=function(x)length(unique(x))
)
colnames(products_per_name) <- c("Name", "ProductCount")
names_vec <- products_per_name$Name
products_per_name <- products_per_name$ProductCount
names(products_per_name) <- names_vec

prices_counts_per_product <- c()
for (name in names(prices_counts)) {
    prices_counts_per_product <- append(
        prices_counts_per_product,
        prices_counts[name] / products_per_name[name]
    )
}
availability_index <- sort(
    prices_counts_per_product
    - median(prices_counts_per_product)
    + sd(prices_counts_per_product) * 0.2
)

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
