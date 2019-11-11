packages <-   c(
  "shiny", 
  "shinyjs",
  "bsplus",
  "tidyverse",
  "data.table",
  "DT",
  "haven",
  "knitr",
  "kableExtra",
  "margins",
  "readxl",
  "twang"
)

install.packages(packages)

for (package in packages) {
  print(sprintf("%s: %s", package, packageDescription(package, fields="License")))
}

