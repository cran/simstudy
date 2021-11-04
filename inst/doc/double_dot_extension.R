## ---- echo = FALSE, message = FALSE-------------------------------------------
options(digits = 3)

library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)
odds <- function (p)  p/(1 - p) # TODO temporary remove when added to package
plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = element_rect(fill = panelback),
    panel.grid = element_blank(),
    axis.ticks =  element_line(colour = "black"),
    panel.spacing =unit(0.25, "lines"),  # requires package grid
    panel.border = element_rect(fill = NA, colour="gray90"), 
    plot.title = element_text(size = 8,vjust=.5,hjust=0),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 8)
  )  
  
}


## -----------------------------------------------------------------------------
defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
defs <- defData(defs, varname = "z", formula = "4 + 3*x - 2*y", variance = 1, dist = "normal")

defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5", newvariance = 2)
defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "z", newdist = "poisson", newlink = "log")
defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "z", remove = TRUE)
defs

## -----------------------------------------------------------------------------
def <- defData(varname = "x", formula = 0, 
  variance = 5, dist = "normal")
def <- defData(def, varname = "y", formula = "..B0 + ..B1 * x", 
  variance = "..sigma2", dist = "normal")

def

## -----------------------------------------------------------------------------
B0 <- 4;
B1 <- 2;
sigma2 <- 9

set.seed(716251)

dd <- genData(100, def)

fit <- summary(lm(y ~ x, data = dd))

coef(fit)
fit$sigma

## -----------------------------------------------------------------------------
sigma2 <- 16

dd <- genData(100, def)
fit <- summary(lm(y ~ x, data = dd))

coef(fit)
fit$sigma

## ---- fig.width = 5-----------------------------------------------------------
sigma2s <- c(1, 2, 6, 9)

gen_data <- function(sigma2, d) {
  dd <- genData(200, d)
  dd$sigma2 <- sigma2
  dd
}

dd_4 <- lapply(sigma2s, function(s) gen_data(s, def))
dd_4 <- rbindlist(dd_4)

ggplot(data = dd_4, aes(x = x, y = y)) +
  geom_point(size = .5, color = "grey30") +
  facet_wrap(sigma2 ~ .) +
  theme(panel.grid = element_blank())

## -----------------------------------------------------------------------------
defblk <- defData(varname = "blksize", 
   formula = "..sizes[1] | .5 + ..sizes[2] | .5", dist = "mixture")

defblk

## -----------------------------------------------------------------------------
sizes <- c(2, 4)
genData(1000, defblk)

## -----------------------------------------------------------------------------
tau <- rgamma(3, 5, 2)
tau <- tau / sum(tau)
tau

d <- defData(varname = "a", formula = 3, variance = 4)
d <- defData(d, varname = "b", formula = 8, variance = 2)
d <- defData(d, varname = "c", formula = 11, variance = 6)
d <- defData(d, varname = "theta", formula = "..tau[1]*a + ..tau[2]*b + ..tau[3]*c", 
  dist = "nonrandom")

set.seed(1)
genData(4, d)

## -----------------------------------------------------------------------------
d <- updateDef(d, changevar = "theta", newformula = "t(..tau) %*% c(a, b, c)")

set.seed(1)
genData(4, d)

## -----------------------------------------------------------------------------
effect <- matrix(c(0, 4, 5, 7), nrow = 2)
effect

## -----------------------------------------------------------------------------
d1 <- defData(varname = "a", formula = ".5;.5", variance = "1;2", dist = "categorical")
d1 <- defData(d1, varname = "b", formula = ".5;.5", variance = "1;2", dist = "categorical")
d1 <- defData(d1, varname = "outcome", formula = "..effect[a, b]", dist="nonrandom")

## -----------------------------------------------------------------------------
dx <- genData(8, d1)
dx

## -----------------------------------------------------------------------------
d1 <- updateDef(d1, "outcome", newvariance = 9, newdist = "normal")
dx <- genData(1000, d1)

## ---- echo=FALSE--------------------------------------------------------------
dsum <- dx[, .(outcome=mean(outcome)), keyby = .(a, b)]

ggplot(data = dx, aes(x = factor(a), y = outcome)) +
  geom_jitter(aes(color = factor(b)), width = .2, alpha = .4, size = .2) +
  geom_point(data = dsum, size = 2, aes(color = factor(b))) + 
  geom_line(data = dsum, size = 1, aes(color = factor(b), group = factor(b))) +
  scale_color_manual(values = cbbPalette, name = "  b") +
  theme(panel.grid = element_blank()) +
  xlab ("a")

