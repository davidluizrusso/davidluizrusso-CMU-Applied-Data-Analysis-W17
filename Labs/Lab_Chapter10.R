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

#----------------------------- Lab 10.5

#10.5.1 K Means Clustering
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

km.out <- kmeans(x, 2, nstart = 20)

km.out$cluster

plot(x,
     col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)

set.seed(4)
kn.out <- kmeans(x, 3, nstart = 20)
kn.out

set.seed(3)
kn.out <- kmeans(x, 3, nstart = 1)
kn.out$tot.withinss
kn.out <- kmeans(x, 3, nstart = 20)
kn.out$tot.withinss

#10.5.2 Hierarchical Clustering
hc.complete <- hclust(dist(x), method = "complete")

#----------------------------- Lab 10.6.1

#----------------------------- Lab 10.6.2

