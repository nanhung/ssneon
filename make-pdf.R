# Method 1: Simple if statement
if (!require("rmarkdown", quietly = TRUE)) {
    install.packages("rmarkdown")
}

rmarkdown::render("manuscript.Rmd")
