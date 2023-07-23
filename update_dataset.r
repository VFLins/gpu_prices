library(DBI)

# connecting to database
db_path <- 'C:/Users/vflin/Projetos/Price_indexr/data/database.db'
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

# save file
saveRDS(data, file = "data.rds")
