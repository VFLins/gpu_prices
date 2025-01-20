library(txtplot)
PROJECT_ROOT = here::here()

PRICES0 <- readRDS(here::here("backend", "data", "prices.Rds"))
source(here::here("frontend", "scripts", "setup_data.R"))

prices_counts <- sort(table(PRICES[, 3]))
prices_counts_percent <- (
    prices_counts / sum(prices_counts)
) *100

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


PRICES[
    (PRICES$Price < 2200) &
    (PRICES$ProductName == "Geforce Rtx 3070 Ti"),
    c("Name", "Store")]
