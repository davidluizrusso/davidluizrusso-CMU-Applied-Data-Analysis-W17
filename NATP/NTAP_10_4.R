#----------------------------------- Overhead

# load packages
library(tidyverse)
library(caret)
library(ISLR)
library(MASS)
library(class)
library(glmnet)
library(ggbiplot)
library(factoextra)

# load data
data("USArrests")

#----------------------------------- Analysis

# pairs plots for the variables in USArrests
pairs(USArrests)

# means and variances of unscaled data
apply(USArrests, 2, function(x) round(c(mean(x), var(x)), 4))

# perform PCA
pr.out <- prcomp(USArrests, scale = TRUE)


#----------------------------------- Results

# analysis of percentage of variance explained
pr.var <- pr.out$sdev^2
pve = pr.var/sum(pr.var)

pca_plot_data <- data.frame(
  `principal component` = 1:4,
  `pve` = pve,
  `cpve` = cumsum(pve)
)

pca_plot <-
  pca_plot_data %>%
  tidyr::gather(key = measurement, value = value, -principal.component) %>%
  dplyr::mutate(measurement = factor(measurement, levels = c("pve", "cpve"))) %>%
  dplyr::mutate(measurement = fct_recode(measurement,
                                         "percentage of variation explained" = "pve",
                                         "cumulative percentage \n of variation explained" = "cpve")) %>%
  ggplot(aes(x = principal.component, y = value)) +
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Percentage") + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  facet_wrap(~measurement)

# biplot
factoextra::fviz_pca_biplot(pr.out)

# loadings
round(pr.out$rotation, 4)








