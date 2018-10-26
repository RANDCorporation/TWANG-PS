library(tidyverse)
library(knitr)
library(kableExtra)
library(shinyjs)
library(twang)

# TODO: this should be an input
set.seed(1)

# debugging stuff
data(lalonde)

# more debugging stuff
load("degbug.RData")
