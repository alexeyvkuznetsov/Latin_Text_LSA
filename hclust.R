
install.packages("pvclust")
library(pvclust)
data(lung)
lung[1:3,1:5]


result <- pvclust(lung, method.dist="cor", method.hclust="average", nboot=1000, parallel=TRUE)


plot(result)

pvrect(result, alpha=0.95)


# https://github.com/shimo-lab/pvclust
# https://www.rdocumentation.org/packages/pvclust/versions/2.2-0/topics/pvclust


# NOT RUN {
### example using Boston data in package MASS
data(Boston, package = "MASS")

## multiscale bootstrap resampling (non-parallel)
boston.pv <- pvclust(Boston, nboot=100, parallel=FALSE)

## CAUTION: nboot=100 may be too small for actual use.
##          We suggest nboot=1000 or larger.
##          plot/print functions will be useful for diagnostics.

## plot dendrogram with p-values
plot(boston.pv)

ask.bak <- par()$ask
par(ask=TRUE)

## highlight clusters with high au p-values
pvrect(boston.pv)

## print the result of multiscale bootstrap resampling
print(boston.pv, digits=3)

## plot diagnostic for curve fitting
msplot(boston.pv, edges=c(2,4,6,7))

par(ask=ask.bak)

## print clusters with high p-values
boston.pp <- pvpick(boston.pv)
boston.pp

### Using a custom distance measure

## Define a distance function which returns an object of class "dist".
## The function must have only one argument "x" (data matrix or data.frame).
cosine <- function(x) {
  x <- as.matrix(x)
  y <- t(x) %*% x
  res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  res <- as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}

result <- pvclust(Boston, method.dist=cosine, nboot=100)
plot(result)






result <- pvclust(lsaMatrix, method.dist=cosine, nboot=100)
plot(result)


# }
# NOT RUN {
### parallel computation
result.par <- pvclust(Boston, nboot=1000, parallel=TRUE)
plot(result.par)
# }
# NOT RUN {
# }

