## 4.6.1 The Stock Market Data
library(tidyverse)
library(ISLR)
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
