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

# get KNN data sets
Smarket_train_x <- Smarket_train[, 1:6]
Smarket_test_x <- Smarket_test[, 1:6]
Smarket_train_y <- Smarket_test[, "Direction"]

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
  mods <- apply(combs, 1, paste, collapse = " + ")
  mods <- as.data.frame(paste0("Direction ~ ", mods))
  names(mods) <- "model"
  mods$model <- as.character(mods$model)
  mods
  
}

candidates <- c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")

all_models <- do.call(rbind,
                     lapply(seq_along(candidates),
                     function(x) model_paster(cans = candidates, p = x))) 

knn_models <- expand.grid(c(TRUE, FALSE),
                          c(TRUE, FALSE),
                          c(TRUE, FALSE),
                          c(TRUE, FALSE),
                          c(TRUE, FALSE),
                          c(TRUE, FALSE)
                          )

knn_models <- knn_models[-1, ]

# get KNN data sets
Smarket_train_x <- Smarket_train[, 1:6]
Smarket_test_x <- Smarket_test[, 1:6]
Smarket_train_y <- Smarket_train[, "Direction"]


knn_preds <- 
lapply(1:nrow(knn_models), function(x) knn(data.frame(Smarket_train_x[, as.logical(knn_models[x, ])]),
                                           data.frame(Smarket_test_x[, as.logical(knn_models[x, ])]),
                                           Smarket_train_y,
                                           k = 1))

#----------------------------------- Write model fitting functions for LR, LDA, and QDA

fit_glm <- function(formula, train_data, test_data, response_var){
  
  glm_candidate <- glm(as.formula(formula),
                       data = train_data,
                       family = binomial)
  
  glm.preds <- ifelse(predict(glm_candidate, newdata = test_data, type = "response") < 0.5, 
                      "Down",
                      "Up")
  
  acc <- round(mean(glm.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(glm.preds == "Up" & test_data[, response_var] == "Up")/
                    sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(glm.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = "logistic regression",
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}


fit_lda <- function(formula, train_data, test_data, response_var){
  
  lda_candidate <- lda(as.formula(formula),
                       data = train_data)
  
  lda.preds <- predict(lda_candidate, newdata = test_data)$class
  
  acc <- round(mean(lda.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(lda.preds == "Up" & test_data[, response_var] == "Up")/
                      sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(lda.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = "lda",
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}

fit_qda <- function(formula, train_data, test_data, response_var){
  
  qda_candidate <- qda(as.formula(formula),
                       data = train_data)
  
  qda.preds <- predict(qda_candidate, newdata = test_data)$class
  
  acc <- round(mean(qda.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(qda.preds == "Up" & test_data[, response_var] == "Up")/
                      sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(qda.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = "qda",
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}



glms <- do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_glm(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction"
                                                                     )
                       )
                )

ldas <- do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_lda(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction"
                )
                )
)

qdas <- do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_qda(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction"
                )
                )
)

all_fit_models <-
  do.call(rbind, list(glms, ldas, qdas)) %>%
  dplyr::arrange(desc(accuracy), desc(true.positive.rate), desc(true.negative.rate))


          

  



