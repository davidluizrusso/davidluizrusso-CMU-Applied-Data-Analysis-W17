### Load packages and data
library(tidyverse)
library(MASS)
library(caret)
library(corrplot)
library(leaps)
library(car)



data(Boston)

### Examine structure of Boston data set
str(Boston)

###---------------------------------------------------------------------------
###---------------------------------------------------------------------------
###------- Analytic Approach -------------------------------------------------
###---------------------------------------------------------------------------
###---------------------------------------------------------------------------


### Determine the two variables with the highest correlation to medv
corMat <- cor(Boston)
corPairs <- as.data.frame(as.table(corMat))

sorted <- 
  corPairs %>%
    dplyr::filter(Var1 %in% "medv") %>%
      dplyr::mutate(absFreq = abs(Freq)) %>%
        dplyr::arrange(absFreq)

pairs(Boston[names(Boston) %in% c("medv", "lstat", "rm")])



###---------------------------------------------------------------------------
###---------------------------------------------------------------------------
###------- Results -----------------------------------------------------------
###---------------------------------------------------------------------------
###---------------------------------------------------------------------------

# create testing and validation data sets
train_index <- createDataPartition(Boston$medv, p = 0.75, list = FALSE)
trainset <- Boston[train_index, ]
testset <- Boston[-train_index, ]


##### perform best subsets analysis
regsubsets.out <-
  regsubsets(medv ~ .,
             data = trainset,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out

summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 1, main = "Adjusted R^2")

summary.out$adjr2

# the best compromise of R^2 and complexity is the model with 7 variables
# crim, nox, rm, dis, ptratio, black, lstat
lm.best_subsets <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + lstat, data = trainset)
postResample(pred = predict(lm.best_subsets, testset),  obs = testset$medv)

##### LASSO variable selection
lasso_grid <- expand.grid(fraction = seq(.05, 1, length = 20))
lasso_model <- train(medv ~ .,
                    data = trainset,
                    method = "lasso",
                    preProcess = c("center", "scale"),
                    tuneGrid = lasso_grid,
                    trControl = trainControl(method= "cv", selectionFunction = "oneSE"))
lasso_model
lasso_model$finalModel$beta.pure[8, ]
lasso_pred <- predict(lasso_model, testset)
postResample(pred = lasso_pred,  obs = testset$medv)

# crim, rm, dis, tax, ptratio, black, lstat
lm.lasso <- lm(medv ~ crim + rm + dis + tax + ptratio + black + lstat, data = trainset)
postResample(pred = predict(lm.lasso, testset),  obs = testset$medv)

# fine tune lm.best_subsets
lm.best_subsets <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + lstat, data = trainset)

## linearity considerations
boston_long <- 
  Boston[, c("medv", "zn", "chas", "nox", "rm", "dis", "ptratio", "lstat")] %>%
    tidyr::gather("var", "value", -1)

xyPlots <- boston_long %>%
            ggplot(aes(x = value, y = medv)) +
              geom_point() + 
                facet_wrap(~var, scales = "free")

# dis, lstat, nox all appear to have signs of non-linearity 
lm.best_subsets_poly <- lm(medv ~ zn + chas + poly(nox, 3) + rm + poly(dis, 3) + ptratio + poly(lstat, 3), data = trainset)

#lstat has a significant quadratic term
lm.best_subsets_poly <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + poly(lstat, 2), data = trainset)
postResample(pred = predict(lm.best_subsets_poly, testset),  obs = testset$medv)


                


###---------------------------------------------------------------------------
###---------------------------------------------------------------------------
###------- Error Analysis ----------------------------------------------------
###---------------------------------------------------------------------------
###---------------------------------------------------------------------------

# Multicollinearity
corrplot::corrplot(cor(Boston[, -4]),
                   order = "hclust",
                   type = "lower")

vif(lm.best_subsets)

## linearity considerations
boston_long <- 
  Boston[, c("medv", "zn", "chas", "nox", "rm", "dis", "ptratio", "lstat")] %>%
  tidyr::gather("var", "value", -1)

xyPlots <- boston_long %>%
  ggplot(aes(x = value, y = medv)) +
  geom_point() + 
  facet_wrap(~var, scales = "free")

# dis, lstat, nox all appear to have signs of non-linearity 
lm.best_subsets_poly <- lm(medv ~ zn + chas + poly(nox, 3) + rm + poly(dis, 3) + ptratio + poly(lstat, 3), data = trainset)

#lstat has a significant quadratic term
lm.best_subsets_poly <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + poly(lstat, 2), data = trainset)
postResample(pred = predict(lm.best_subsets_poly, testset),  obs = testset$medv)


