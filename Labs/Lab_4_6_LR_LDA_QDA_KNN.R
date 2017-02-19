## 4.6.1 The Stock Market Data
library(tidyverse)
library(ISLR)
library(MASS)
library(class)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])
plot(Smarket$Volume)

## 4.6.2 Logistic Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket,
               family = binomial)

summary(glm.fit)
coef(glm.fit)
broom::tidy(summary(glm.fit)$coef)
broom::tidy(summary(glm.fit)$coef[, 4])

glm.probs <- predict(glm.fit, type = "response")
contrasts(Smarket$Direction)

glm.pred <- rep("Down", nrow(Smarket))
glm.pred[glm.probs > 0.5] <- "Up"

table(Smarket$Direction, glm.pred)
mean(glm.pred == Smarket$Direction)

train <- Smarket$Year < 2005

Smarket_test <- Smarket[!train, ]
dim(Smarket_test)
Direction_test <- Smarket$Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket,
               family = binomial,
               subset = train)
glm.probs <- predict(glm.fit, Smarket_test, type = "response")

glm.pred <- rep("Down", nrow(Smarket_test))
glm.pred[glm.probs > 0.5] <- "Up"

table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)

glm.fit <- glm(Direction ~ Lag1 + Lag2,
               data = Smarket,
               family = binomial,
               subset = train)

glm.probs <- predict(glm.fit, Smarket_test, type = "response")
glm.pred <- ifelse(glm.probs > 0.50, "Up", "Down")
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)

predict(glm.fit, 
        newdata = data.frame(
          Lag1 = c(1.2, 1.5),
          Lag2 = c(1.1, -0.8)
        ),
        type = "response")


## 4.6.3 Linear Discriminant Analysis
lda.fit <- lda(Direction ~ Lag1 + Lag2,
               data = Smarket,
               subset = train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket_test)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction_test)
mean(lda.class == Direction_test)
sum(lda.pred$posterior[, 1] > 0.50) 
sum(lda.pred$posterior[, 1] < 0.50) 
lda.pred$posterior[1:20]
lda.class[1:20]
sum(lda.pred$posterior[, 1] > 0.9)

## 4.6.4 Quadratic Discriminant Analysis
qda.fit <- qda(Direction ~ Lag1 + Lag2,
               data = Smarket,
               subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket_test)$class
table(qda.class, Direction_test)
mean(qda.class == Direction_test)

## 4.6.5 K-Nearest Neighbors
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
train.Direction <- Smarket$Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction_test)
mean(knn.pred == Direction_test)

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction_test)
mean(knn.pred == Direction_test)

## 4.6.6 Application to Caravan Insurance Data
dim(Caravan)
summary(Caravan$Purchase)
348/5822

standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])

test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)

glm.fit <- glm(Purchase ~ .,
               data = Caravan,
               family = binomial,
               subset = -test)
glm.probs <- predict(glm.fit, Caravan[test, ], type = "response")
glm.pred <- rep("No", length(test))
glm.pred[glm.probs > 0.5] <- "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", length(test))
glm.pred[glm.probs > 0.25] <- "Yes"
table(glm.pred, test.Y)
