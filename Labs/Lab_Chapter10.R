#----------------------------- Lab 10.4
library(ISLR)
data("USArrests")

states <- row.names(USArrests)
names(USArrests)

apply(USArrests, 2, mean)

apply(USArrests, 2, var)

pr.out <- prcomp(USArrests, scale = TRUE)

names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)

pr.out$rotation <- -pr.out$rotation
biplot(pr.out, scale = 0)

pr.out$sdev

pr.var <- pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1),
     type = "b")

plot(cumsum(pve),
     xlab = "Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1),
     type = "b")

a <- c(1, 2, 8, -3)
cumsum(a)


