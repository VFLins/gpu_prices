library(httr)
library(rvest)

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36 Edg/115.0.1901.203"
hmtl <- GET("https://www.tomshardware.com/reviews/gpu-hierarchy,4388.html", user_agent(ua))
tables <- html_elements(".table_body__data , .table__head__heading--left")
