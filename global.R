library(shiny)
library(shinyjs)
library(tidyverse)
library(knitr)
library(kableExtra)
library(twang)

# TODO: this should be an input
set.seed(1)

# RAND purple: #663399

# prototyping stuff
data(lalonde)

# plot types
plot.types <- c(
  "Convergence", 
  "Propensity score boxplots", 
  "Balance before and after weighting",
  "ES p-values",
  "KS p-values"
)
