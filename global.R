library(shiny)
library(shinyjs)
library(tidyverse)
library(data.table)
library(DT)
library(knitr)
library(kableExtra)
library(margins)
library(twang)

# RAND purple: #663399

# tab names
tab.names <- c("intro", "upload", "model", "eval", "effects", "weights")

# stop methods 
stop.methods <- c("es.mean", "es.max", "ks.mean", "ks.max")

# plot types
plot.types <- c(
  "Convergence", 
  "Propensity score boxplots", 
  "Balance before and after weighting",
  "ES p-values",
  "KS p-values"
)

# round to whole number or significant digits
myround <- function(x,d=3){
   ifelse( abs(x) < 10^d , signif(x,d) , round(x,0) )
}
