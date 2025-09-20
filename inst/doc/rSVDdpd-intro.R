## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(rsvddpd)
library(microbenchmark)
library(matrixStats)
library(pcaMethods)

## -----------------------------------------------------------------------------
X <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
X

## -----------------------------------------------------------------------------
svd(X)

## -----------------------------------------------------------------------------
X[2, 2] <- 100
svd(X)

## -----------------------------------------------------------------------------
rSVDdpd(X, alpha = 0.3, nd = min(dim(X)))

## -----------------------------------------------------------------------------
rSVDdpd(X * 1e6, alpha  = 0.3, nd = min(dim(X)))
rSVDdpd(X * 1e-6, alpha = 0.3, nd = min(dim(X)))

## -----------------------------------------------------------------------------
Y <- X[, c(3, 1, 2)]
rSVDdpd(Y, alpha = 0.3, nd = min(dim(Y)))

## -----------------------------------------------------------------------------
crossprod(rSVDdpd(X, alpha = 0.3, nd = min(dim(X)))$u)

## -----------------------------------------------------------------------------
microbenchmark::microbenchmark(svd(X), 
                               rSVDdpd(X, alpha = 0, nd = min(dim(X))), 
                               rSVDdpd(X, alpha = 0.25, nd = min(dim(X))), 
                               rSVDdpd(X, alpha = 0.5, nd = min(dim(X))), 
                               rSVDdpd(X, alpha = 0.75, nd = min(dim(X))), 
                               rSVDdpd(X, alpha = 1, nd = min(dim(X))), times = 30)

## -----------------------------------------------------------------------------
U <- as.matrix(stats::contr.poly(10)[, 1:3])
V <- as.matrix(stats::contr.poly(4)[, 1:3])
trueSVD <- list(d = c(10, 5, 3), u = U, v = V)  # true svd of the data matrix

## ----results='hide'-----------------------------------------------------------
res <- simSVD(trueSVD, svdfun = svd, B = 100, seed = 2021, outlier = TRUE, out_value = 25, tau = 0.9)

## -----------------------------------------------------------------------------
res

## ----results='hide'-----------------------------------------------------------
res <- simSVD(trueSVD, svdfun = pcaMethods::robustSvd, B = 100, seed = 2021, outlier = TRUE, out_value = 25, tau = 0.9)

## -----------------------------------------------------------------------------
res

## ----results='hide'-----------------------------------------------------------
rSVDdpd_max <- function(X, alpha) {
  return(rSVDdpd(X, alpha = alpha, nd = min(dim(X))))
}
res <- simSVD(trueSVD, svdfun = rSVDdpd_max, B = 100, seed = 2021, outlier = TRUE, out_value = 25, tau = 0.9, alpha = 0.25)

## -----------------------------------------------------------------------------
res

## ----results='hide'-----------------------------------------------------------
res <- simSVD(trueSVD, svdfun = rSVDdpd_max, B = 100, seed = 2021, outlier = TRUE, out_value = 25, tau = 0.9, alpha = 0.75)

## -----------------------------------------------------------------------------
res

