library(DBI)

### Update price data ###

# connecting to database

db_path <- readRDS(file=here::here("frontend", "assets", "dbpath.rds"))
con <- dbConnect(RSQLite::SQLite(), db_path)

# loading tables
products <- dbReadTable(con, 'products')
prices <- dbReadTable(con, 'prices')

# merging tables together
data <- merge(x=products, y=prices, by.x='Id', by.y='ProductId')
# fixing columns names
colnames(data)[c(1,9)] <- c('ProductId', 'PriceId')
# fixing date columns datatype
data$Date <- as.POSIXct(data$Date)
data$Created <- as.POSIXct(data$Created)
data$LastUpdate <- as.POSIXct(data$LastUpdate)

# remove duplicated rows
duplicate_mask <- duplicated(data[c('Name', 'Date', 'Price', 'Store')])
data <- data[!duplicate_mask,]

# keep only last 3 months
floor_date <- as.POSIXct((Sys.Date() - 184))
data <- data[data$Date >= floor_date, ]

# save file
saveRDS(data, file = here::here("backend", "data", "prices.rds"))

# cleanup
dbDisconnect(con)
rm(duplicate_mask, db_path, floor_date, 
   products, prices, data, con)
