library(shiny)
library(shinyjs)
library(tidyverse)
library(knitr)
library(kableExtra)
library(twang)

# TODO: this should be an input
set.seed(1)

# RAND purple: #663399

# debugging stuff
data(lalonde)

# more debugging stuff
load("debug.RData")
