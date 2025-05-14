# cd into this script directory and run like: rscript render.R
# to set a new prices db path: rscript render.R \path\to\database.db
## Check packages and install missing ones
packages <- c(
    "zoo", "DBI", "shiny", "dplyr", "tidyr", "scales", "plotly", "rmarkdown",
    "ggplot2", "stringr", "RSQLite", "kableExtra", "htmltools", "here")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        install.packages(pkg, repos = "https://cloud.r-project.org/")
    }
    library(pkg, character.only = TRUE)
}

PROJECT_ROOT = here::here()

# Check pandoc installation places dynamically
pandoc_places <- c(
    paste0(Sys.getenv("USERPROFILE"),"\\AppData\\Local\\Pandoc"),
    paste0(Sys.getenv("PROGRAMFILES"),"\\Pandoc")
)
for (place in pandoc_places) {
    if (!rmarkdown::pandoc_available()) {
        rmarkdown::find_pandoc(dir = place)
    }
}
if (!rmarkdown::pandoc_available()) {
    message("Pandoc not found, please install it on an usual location.")
    quit()    
}
message(paste("Using pandoc version:", rmarkdown::pandoc_version()))

# Update database path
args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 1) {
    if (file.exists(args[1])) {
        message(paste("Setting dbpath variable to", args[1]))
        saveRDS(args[1], file=here::here("frontend", "assets", "dbpath.rds"))
    } else {
        message("Could not update dbpath variable, file path does not exist.")
    }
}

# exit if dbpath is not set
if (!file.exists(here::here("frontend", "assets", "dbpath.rds"))) {
    message("dbpath variable is not set, run this again with the dbpath as an argument.")
    quit()
}

# Render document
rmarkdown::render(
    input = file.path(PROJECT_ROOT, "frontend", "index.Rmd"), 
    output_file = file.path(PROJECT_ROOT, "index.html")
)
