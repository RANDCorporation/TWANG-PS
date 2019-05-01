library(shiny)
library(shinyjs)
library(bsplus)
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

# round to whole number or significant digits
myround <- function(x,d=3){
   ifelse( abs(x) < 10^d , signif(x,d) , round(x,0) )
}

# pop-over text
n.trees.text <- 
  "Total number of trees to fit. This is equivalent to the number of iterations 
    and the number of basis functions in the additive expansion. Default is 100."

interaction.depth.text <- 
  "Maximum depth of each tree (i.e., the highest level of variable interactions 
    allowed). A value of 1 implies an additive model, a value of 2 implies a model 
    with up to 2-way interactions, etc. Default is 1."

shrinkage.text <-
  "Parameter applied to each tree in the expansion. Also known as the learning 
    rate or step-size reduction; 0.001 to 0.1 usually work, but a smaller learning 
    rate typically requires more trees. Default is 0.1."

estimand.text <- 
  "The causal effect of interest. Options are 'ATE' (average treatment effect), 
    which attempts to estimate the change in the outcome if the treatment were 
    applied to the entire population versus if the control were applied to the 
    entire population, or 'ATT' (average treatment effect on the treated) which 
    attempts to estimate the analogous effect, averaging only over the treated population."

stop.method.text <-
  "A method or methods of measuring and summarizing balance across pretreatment
    variables. Current options are ks.mean, ks.max, es.mean, and es.max. ks refers 
    to the Kolmogorov-Smirnov statistic and es refers to standardized effect size. 
    These are summarized across the pretreatment variables by either the maximum 
    (.max) or the mean (.mean)."
