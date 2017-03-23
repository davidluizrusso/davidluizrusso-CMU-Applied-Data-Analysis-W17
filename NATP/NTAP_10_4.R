#----------------------------------- Overhead

# load packages
library(tidyverse)
library(caret)
library(ISLR)
library(MASS)
library(class)
library(glmnet)

# load data
data("USArrests")

#----------------------------------- Analysis

# means and variances of unscaled data
knitr::kable(apply(USArrests, 2, function(x) round(c(mean(x), var(x)), 4)))

# pairs plots for the variables in USArrests
pairs(USArrests)


