library(DBI)

### Update price data ###

# connecting to database
db_path <- readRDS(file=here::here("frontend", "assets", "dbpath.rds"))
con <- dbConnect(RSQLite::SQLite(), db_path)

# loading SQL SELECT statement
stmt <- readLines(here::here("backend", "scripts", "prices_table.sql")) |>
    paste(collapse=" ")

# loading data
data <- dbGetQuery(con, stmt)
dbDisconnect(con)

# fixing date columns datatype
data$Date <- as.POSIXct(data$Date)
data$Created <- as.POSIXct(data$Created)
data$LastUpdate <- as.POSIXct(data$LastUpdate)

# remove duplicated rows
duplicate_mask <- duplicated(data[c('Name', 'Date', 'Price', 'Store')])
data <- data[!duplicate_mask,]

# keep only last 3 months
floor_date <- as.POSIXct((Sys.Date() - 190))
data <- data[data$Date >= floor_date, ]

# remove extremely discrepant prices
discrepant_prices_mask <- function(dataset) {
    chip_names <- unique(dataset$ProductName)
    discrepant_price_ids <- c()
    for (chip_name in chip_names){
        slice <- dataset[(dataset$ProductName == chip_name), ]
        min <- median(slice$Price) - (sd(slice$Price) * 1.645)
        max <- median(slice$Price) + (sd(slice$Price) * 1.645)
        ids <- slice[(slice$Price > max) | (slice$Price < min), "Id"]
        discrepant_price_ids <- c(discrepant_price_ids, ids)
    }
    return(dataset$Id %in% discrepant_price_ids)
}
data <- data[!discrepant_prices_mask(data), ]

# save file
saveRDS(data, file = here::here("backend", "data", "prices.rds"))

# cleanup
rm(duplicate_mask, db_path, floor_date, 
   products, prices, data, con)
