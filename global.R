library(shiny)
library(shinyjs)
library(tidyverse)
library(knitr)
library(kableExtra)
library(twang)
library(margins)

# RAND purple: #663399

# prototyping stuff
data(lalonde)

# tab names
tab.names <- c("intro", "model", "eval", "effects")

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
