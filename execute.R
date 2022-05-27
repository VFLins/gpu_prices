## Check packages and install missing ones
packages <- c(
    "zoo", "DBI", "shiny", "dplyr", "tidyr", "scales", "plotly", "rmarkdown",
    "ggplot2", "stringr", "RSQLite", "kableExtra", "htmltools")
for (pkg in packages) {
    if (!require(pkg)) {install.packages(pkg)}
    library(pkg, character.only = TRUE)}


