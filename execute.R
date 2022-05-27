## Check packages and install missing ones
packages <- c(
    "zoo", "DBI", "shiny", "dplyr", "tidyr", "scales", "plotly", "rmarkdown",
    "ggplot2", "stringr", "RSQLite", "kableExtra", "htmltools")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        install.packages(pkg, repos = "https://cloud.r-project.org/")}
    library(pkg, character.only = TRUE)}

message("Using pandoc version:")
rmarkdown::find_pandoc(
    dir = paste0(Sys.getenv("USERPROFILE"),"\\AppData\\Local\\Pandoc"))

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 1) {
    rmarkdown::knit_params_ask(file = "analysis.Rmd", output_file = args[[1]])
} else {
    message("Incorrect number of arguments, if it contains spaces, use quotes.")
}

