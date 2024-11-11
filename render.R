# run like: rscript .\render.R
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
message("Using pandoc version:")

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

# Render document
rmarkdown::render(
    input = file.path(PROJECT_ROOT, "frontend", "index.Rmd"), 
    output_format = "html_document",
    output_file = file.path(PROJECT_ROOT, "index.html")
)
