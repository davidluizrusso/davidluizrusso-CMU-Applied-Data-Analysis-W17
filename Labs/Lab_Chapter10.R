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

hc.average <- hclust(dist(x), method = "average")

hc.single <- hclust(dist(x), method = "single")

par(mfrow = c(1, 3))
plot(hc.complete,
     main = "Complete Linkage",
     xlab = "",
     sub = "",
     cex = 0.9)
plot(hc.average,
     main = "Average Linkage",
     xlab = "",
     sub = "",
     cex = 0.9)
plot(hc.single,
     main = "Single Linkage",
     xlab = "",
     sub = "",
     cex = 0.9)
dev.off()

lapply(list(hc.complete, hc.average, hc.single),
       cutree,
       2)

cutree(hc.single, 4)

xsc <- scale(x)
plot(hclust(dist(x), method = "complete"),
     main = "Hierarchical Clustering with \n Scaled Features")

x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"),
     main = "Complete Linkage with \n Correlation-Based Distance",
     xlab = "",
     sub = "")

#----------------------------- Lab 10.6

nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)

nci.labs[1:4]
table(nci.labs)

#10.6.1 PCA
pr.out <- prcomp(nci.data, scale = TRUE)

Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2],
     col = Cols(nci.labs),
     pch = 19,
     xlab = "Z1",
     ylab = "Z2")
plot(pr.out$x[, c(1, 3)],
     col = Cols(nci.labs),
     pch = 19,
     xlab = "Z1",
     ylab = "Z3")
dev.off()

summary(pr.out)
plot(pr.out)

pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve,
     type = "o",
     ylab = "PVE",
     xlab = "Principal Component",
     col = "blue")
plot(cumsum(pve),
     type = "o",
     ylab = "Cumulative PVE",
     xlab = "Principal Component",
     col = "brown3")
dev.off()

#10.6.2 Clustering 
sd.data <- scale(nci.data)

par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist),
     labels = nci.labs,
     main = "Complete Linkage",
     xlab = "",
     ylab = "")
plot(hclust(data.dist, method = "average"),
     labels = nci.labs,
     main = "Average Linkage",
     xlab = "",
     ylab = "")
plot(hclust(data.dist, method = "single"),
     labels = nci.labs,
     main = "Single Linkage",
     xlab = "",
     ylab = "")
dev.off()

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

cutree(hc.out, 4)

hc.out

set.seed(2)
kn.out <- kmeans(sd.data, 4, nstart = 20)
kn.clusters <- kn.out$cluster
table(kn.clusters, hc.clusters)

hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out,
     labels = nci.labs,
     main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
