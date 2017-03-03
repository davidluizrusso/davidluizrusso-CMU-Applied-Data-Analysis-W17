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
set.seed(1) # for tie-breakers
Smarket_train_x <- Smarket_train[, 1:6]
Smarket_test_x <- Smarket_test[, 1:6]
Smarket_train_y <- Smarket_train[, "Direction"]
Smarket_test_y <- Smarket_test[, "Direction"]

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

#----------------------------------- Fit model with all second order interactions
glm_all <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume +
                           Lag1*Lag2 + Lag1*Lag3 + Lag1*Lag4 + Lag1*Lag5 +
                           Lag1*Volume + Lag2*Lag3 + Lag2*Lag4 + Lag2*Lag5 +
                           Lag2*Volume + Lag3*Lag4 + Lag3*Lag5 + Lag3*Volume +
                           Lag4*Lag5 + Lag4*Volume + Lag5*Volume,
               data = Smarket_train,
               family = binomial)

broom::tidy(glm_all) %>% dplyr::filter(p.value <= 0.10)

# try all main effects and Lag1:Lag5 interaction

#----------------------------------- List all models

model_paster <- function(cans, p){
  
  combs <- as.data.frame(t(combn(cans, p)))
  mods <- apply(combs, 1, paste, collapse = " + ")
  mods <- as.data.frame(paste0("Direction ~ ", mods))
  names(mods) <- "model"
  mods$model <- as.character(mods$model)
  
  if(p > 1){
  expand <- ifelse(grepl("Lag1", mods$model) & grepl("Lag5", mods$model), TRUE, FALSE)
  extras <- data.frame(model = paste(mods[expand, ], "Lag1*Lag5", sep = " + "))
  mods <- rbind(mods, extras)} 
  
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

knn_models <- knn_models[-64, ]

#----------------------------------- Write model fitting functions for LR, LDA, and QDA

fit_glm <- function(formula, train_data, test_data, response_var, threshold){
  
  glm_candidate <- glm(as.formula(formula),
                       data = train_data,
                       family = binomial)
  
  glm.preds <- ifelse(predict(glm_candidate, newdata = test_data, type = "response") < threshold, 
                      "Down",
                      "Up")
  
  acc <- round(mean(glm.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(glm.preds == "Up" & test_data[, response_var] == "Up")/
                    sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(glm.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = paste("logistic regression: threshold = ", threshold),
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}


fit_lda <- function(formula, train_data, test_data, response_var, threshold){
  
  lda_candidate <- lda(as.formula(formula),
                       data = train_data)
  
  lda.preds <- ifelse(predict(lda_candidate, newdata = test_data)$posterior[, "Down"] >= threshold,
                      "Down",
                      "Up")
  
  
  
  acc <- round(mean(lda.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(lda.preds == "Up" & test_data[, response_var] == "Up")/
                      sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(lda.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = paste0("lda: threshold = ", threshold),
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}

fit_qda <- function(formula, train_data, test_data, response_var, threshold){
  
  qda_candidate <- qda(as.formula(formula),
                       data = train_data)
  
  qda.preds <- ifelse(predict(qda_candidate, newdata = test_data)$posterior[, "Down"] >= threshold,
                      "Down",
                      "Up")
  
  acc <- round(mean(qda.preds == test_data[, response_var]), 4)
  
  true_pos <- round(sum(qda.preds == "Up" & test_data[, response_var] == "Up")/
                      sum(test_data[, response_var] == "Up"), 4)
  
  true_neg <- round(sum(qda.preds == "Down" & test_data[, response_var] == "Down")/
                      sum(test_data[, response_var] == "Down"), 4)
  
  res <- data.frame(model = paste0("qda: threshold = ", threshold),
                    variables = formula,
                    accuracy = acc,
                    `true positive rate` = true_pos,
                    `true negative rate` = true_neg)
  
  res
  
}

knn_fitter <- function(trainX, testX, trainY, testY, K){
  
  knn_preds <- lapply(1:nrow(knn_models),
                      function(x) knn(data.frame(trainX[, as.logical(knn_models[x, ])]),
                                      data.frame(testX[, as.logical(knn_models[x, ])]),
                                      trainY,
                                      k = K)
  )
  
  knn_acc <- round(sapply(seq_along(knn_preds), function(x) mean(knn_preds[[x]] == testY)), 4)
  
  knn_true_pos <- round(sapply(seq_along(knn_preds),
                               function(x) sum(knn_preds[[x]] == "Up" & testY == "Up")/
                                 sum(testY == "Up")), 4)
  
  knn_true_neg <- round(sapply(seq_along(knn_preds),
                               function(x) sum(knn_preds[[x]] == "Down" & testY == "Down")/
                                 sum(testY == "Down")), 4)
  
  knn_names <-  sapply(1:nrow(knn_models),
                       function(x) paste(names(trainX)[as.logical(knn_models[x, ])], collapse = ", ")
  )
  
  res <- data.frame(model = paste("KNN: k = ", K),
                    variables = knn_names,
                    accuracy = knn_acc,
                    `true positive rate` = knn_true_pos,
                    `true negative rate` = knn_true_neg)
}

#----------------------------------- Fit models and store in sorted data frame
threshold_grids <- seq(from = 0.45, to = 0.55, by = 0.01)

glms <- 
do.call(rbind, lapply(threshold_grids, function(th)
  do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_glm(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction",
                                                                     threshold = th)))))

ldas <- 
do.call(rbind, lapply(threshold_grids, function(th)    
  do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_lda(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction",
                                                                     threshold = th)))))

qdas <- 
do.call(rbind, lapply(threshold_grids, function(th)  
  do.call(rbind,
                lapply(seq_along(t(all_models)), function(x) fit_qda(formula = all_models$model[x],
                                                                     train_data = Smarket_train,
                                                                     test_data = Smarket_test,
                                                                     response_var = "Direction",
                                                                     threshold = th)))))

k_grid <- seq(from = 1, to = 103, by = 3)

knns <- 
  do.call(rbind,
          lapply(k_grid, function(x) knn_fitter(trainX = Smarket_train_x,
                                                testX = Smarket_test_x,
                                                trainY = Smarket_train_y,
                                                testY = Smarket_test_y,
                                                K = x)))

all_fit_models <-
  do.call(rbind, list(glms, ldas, qdas, knns)) %>%
  dplyr::arrange(desc(accuracy), desc(true.positive.rate), desc(true.negative.rate))


          

  



