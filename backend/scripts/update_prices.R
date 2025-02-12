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
floor_date <- as.POSIXct((Sys.Date() - 184))
data <- data[data$Date >= floor_date, ]

# save file
saveRDS(data, file = here::here("backend", "data", "prices.rds"))

# cleanup
rm(duplicate_mask, db_path, floor_date, 
   products, prices, data, con)
