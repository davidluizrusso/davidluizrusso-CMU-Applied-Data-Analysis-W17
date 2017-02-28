#----------------------------------- Overhead

# load packages
library(tidyverse)
library(caret)
library(ISLR)
library(MASS)
library(class)
library(glmnet)

# load data
data(Smarket)

#----------------------------------- Pre-process data

# create training and testing data
Smarket_train <- dplyr::filter(Smarket, Year != 2005) %>% dplyr::select(-c(Year, Today))
Smarket_test <- dplyr::filter(Smarket, Year == 2005) %>% dplyr::select(-c(Year, Today))

# pre-process the training data
preProc_train <- preProcess(Smarket_train)
Smarket_train <- predict(preProc_train, Smarket_train)

# pre-process the testing data
preProc_test <- preProcess(Smarket_test)
Smarket_test <- predict(preProc_test, Smarket_test)

#----------------------------------- Exploratory data analysis

# Correlations
corrplot::corrplot(cor(Smarket_train[-7]),
                   type = "lower")

# Transformations
Smarket_train_var_plot <-
Smarket_train %>%
  dplyr::select(-Direction) %>%
  gather(key = Variable, value = value) %>%
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~Variable)

#----------------------------------- List all models

model_paster <- function(cans, p){
  
  combs <- as.data.frame(t(combn(cans, p)))
  mods <- apply(combs, 1, paste, collapse = " ~ ")
  mods <- as.data.frame(paste0("Direction ~ ", mods))
  names(mods) <- "model"
  mods
  
}

candidates <- c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")

all_models <- do.call(rbind,
                     lapply(seq_along(candidates),
                     function(x) model_paster(cans = candidates, p = x)))

#----------------------------------- Write model fitting functions for LR, LDA, and QDA


          

  



