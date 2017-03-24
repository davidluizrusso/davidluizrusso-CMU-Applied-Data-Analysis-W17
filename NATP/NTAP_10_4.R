#----------------------------------- Overhead

# load packages
library(tidyverse)
library(forcats)
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

# scores
scores <- data.frame(pr.out$x)

scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 1))

# contingency table
score_analysis <- data.frame(pr.out$x) %>%
  dplyr::select(PC1, PC2) %>%
  dplyr::mutate(PC1_above_average = ifelse(PC1 < 0, TRUE, FALSE)) %>%
  dplyr::mutate(PC2_above_average = ifelse(PC2 < 0, TRUE, FALSE)) %>%
  dplyr::mutate(state = row.names(pr.out$x))

table(score_analysis$PC1_above_average, score_analysis$PC2_above_average)


#----------------------------------- Error Analysis


# perform PCA without scaling
pr.out_us <- prcomp(USArrests, scale = FALSE)

# analysis of percentage of variance explained
pr.var_us <- pr.out_us$sdev^2
pve_us = pr.var_us/sum(pr.var_us)

pca_plot_data_us <- data.frame(
  `principal component` = 1:4,
  `pve` = pve_us,
  `cpve` = cumsum(pve_us)
)

pca_plot_us <-
  pca_plot_data_us %>%
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
factoextra::fviz_pca_biplot(pr.out_us)






