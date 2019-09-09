library(shiny)
library(shinyjs)
library(bsplus)
library(tidyverse)
library(data.table)
library(DT)
library(haven)
library(knitr)
library(kableExtra)
library(margins)
library(readxl)
library(twang)

# RAND purple: #663399

# tab names
tab.names <- c("intro", "upload", "model", "eval", "effects", "weights")

# stop methods 
stop.methods <- c("es.mean", "es.max", "ks.mean", "ks.max")

# round to whole number or significant digits
myround <- function(x,d=3){
   ifelse( abs(x) < 10^d , signif(x,d) , round(x,0) )
}

# pop-over text
n.trees.text <- 
  "Sets the maximum number of iterations that the GBM will run. With each iteration, 
    the GBM becomes more complex. With too few, it will miss salient features of the data. 
    With too many, it will over-fit the data. It is best to make this number large 
    (e.g., 5000 or 10,000 to start). There will be a warning if the estimated optimal 
    number of iterations is not large enough."

interaction.depth.text <- 
  "Sets the level of interactions allowed between covariates. Value = 1 implies no 
    interactions are allowed; Value = 2 allows all possible two-way interactions; 
    Value = 3 allows all possible three-way interactions."

shrinkage.text <-
  "Sets the amount of shrinkage used to enhance the smoothness of the GBM. Small 
    values of 0.01 or 0.001 usually work."

estimand.text <- 
  "The causal estimate of interest. Average treatment effect (ATE) estimates the 
    average treatment effect across the whole sample while Average treatment effect 
    on the treated (ATT) estimates the treatment effect for individuals like those 
    in the treatment group."

stop.method.text <-
  "Set the stopping rule or rules used to optimize the GBM fit. The four stopping 
    rules are defined by two components: a balance metric for covariates (ES or KS) 
    and a rule for summarizing across the pretreatment covariates (mean or max). 
    ES refers to standard mean differences and KS for Kolmogrov Statistics."

outcome.type.text <- 
  "Determines whether a logistic or linear regression model will be used. Logistic 
    will be used for binary 0/1 outcomes and linear regression for continuous outcomes."

outcome.covariates.text <- 
  "Select the pretreatment covariates that should be used in the regression model as 
    additional control covariates on the right-hand side of the model."

outcome.stop.method.text <-
  "Sets which set of propensity score weights should be used based on the chosen stop 
    method. This should be selected based on which stop method produces optimal balance 
    in your sample. If the stop methods perform similarly, it can be selected based on 
    which method yields the largest effective sample size (ESS) for the treatment and control groups."
