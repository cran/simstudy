## ----chunkname, echo=-1-------------------------------------------------------
data.table::setDTthreads(2)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(data.table)

options(digits = 2)

## -----------------------------------------------------------------------------
library(simstudy)
library(ggplot2)
library(data.table)

coefs1 <- c(0.15, 0.25, 0.10, 0.30)

d1 <- defData(varname = "x1", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x2", formula = 0, variance = 1)
d1 <- defData(d1, varname = "b1", formula = 0.3, dist = "binary")
d1 <- defData(d1, varname = "b2", formula = 0.7, dist = "binary")

d1a <- defData(d1, varname = "y", 
  formula = "t(..coefs1) %*% c(x1, x2, b1, b2)",
  dist = "binary", link = "logit")

set.seed(48392)

dd <- genData(500000, d1a)
dd

## -----------------------------------------------------------------------------
dd[, mean(y)]

## -----------------------------------------------------------------------------
d1a <- defData(d1, varname = "y", 
  formula = "t(c(-0.66, ..coefs1)) %*% c(1, x1, x2, b1, b2)",
  dist = "binary", link = "logit")

genData(500000, d1a)[, mean(y)]

## -----------------------------------------------------------------------------
coefs2 <- c(0.20, 0.35, 0.20, 0.45)

d2 <- defData(varname = "x1", formula = 1, variance = 1)
d2 <- defData(d2, varname = "x2", formula = 3, variance = 1)
d2 <- defData(d2, varname = "b1", formula = 0.5, dist = "binary")
d2 <- defData(d2, varname = "b2", formula = 0.8, dist = "binary")

d2a <- defData(d2, varname = "y", 
  formula = "t(..coefs2) %*% c(x1, x2, b1, b2)",
  dist = "binary", link = "logit")

genData(500000, d2a)[, mean(y)]

## -----------------------------------------------------------------------------
d2a <- defData(d2, varname = "y", 
  formula = "t(c(-2.13, ..coefs2)) %*% c(1, x1, x2, b1, b2)",
  dist = "binary", link = "logit")

genData(500000, d1a)[, mean(y)]

## -----------------------------------------------------------------------------
logisticCoefs(defCovar = d1, coefs = coefs1, popPrev = 0.40)
logisticCoefs(defCovar = d2, coefs = coefs2, popPrev = 0.40)

## -----------------------------------------------------------------------------
d1a <- defData(d1, varname = "rx", formula = "1;1", dist = "trtAssign")
d1a <- defData(d1a, varname = "y",
  formula = "t(c(-0.40, ..coefs1)) %*% c(rx, x1, x2, b1, b2)",
  dist = "binary", link = "logit"
)

dd <- genData(500000, d1a)
dd[rx==1, mean(y)]/dd[rx==0, mean(y)]

## -----------------------------------------------------------------------------
d2a <- defData(d2, varname = "rx", formula = "1;1", dist = "trtAssign")
d2a <- defData(d2a, varname = "y",
  formula = "t(c(-0.40, ..coefs2)) %*% c(rx, x1, x2, b1, b2)",
  dist = "binary", link = "logit"
)

dd <- genData(500000, d2a)
dd[rx==1, mean(y)]/dd[rx==0, mean(y)]

## -----------------------------------------------------------------------------
C1 <- logisticCoefs(d1, coefs1, popPrev = 0.40, rr = 0.85)
C1

## -----------------------------------------------------------------------------
d1a <- defData(d1, varname = "rx", formula = "1;1", dist = "trtAssign")
d1a <- defData(d1a, varname = "y",
  formula = "t(..C1) %*% c(1, rx, x1, x2, b1, b2)",
  dist = "binary", link = "logit"
)

dd <- genData(500000, d1a)

## -----------------------------------------------------------------------------
dd[rx==0, mean(y)]
dd[rx==1, mean(y)]/dd[rx==0, mean(y)]

## -----------------------------------------------------------------------------
C1 <- logisticCoefs(d1, coefs1, popPrev = 0.40, rd = -0.15)
C1

## -----------------------------------------------------------------------------
d1a <- defData(d1, varname = "rx", formula = "1;1", dist = "trtAssign")
d1a <- defData(d1a, varname = "y",
  formula = "t(..C1) %*% c(1, rx, x1, x2, b1, b2)",
  dist = "binary", link = "logit"
)

dd <- genData(500000, d1a)

dd[rx==0, mean(y)]
dd[rx==1, mean(y)] - dd[rx==0, mean(y)]

## -----------------------------------------------------------------------------
C1 <- logisticCoefs(d1, coefs1, popPrev = 0.40, auc = 0.85)
C1

## -----------------------------------------------------------------------------
d1a <- defData(d1, varname = "y",
  formula = "t(..C1) %*% c(1, x1, x2, b1, b2)",
  dist = "binary", link = "logit"
)

dd <- genData(500000, d1a)

dd[, mean(y)]

fit <- rms::lrm(y ~ x1 + x2 + b1 + b2, data = dd)
fit$stats["C"]


