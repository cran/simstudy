## ----chunkname, echo=-1-------------------------------------------------------
data.table::setDTthreads(2)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)
library(ordinal)

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


## ----options, echo = FALSE----------------------------------------------------
options(digits = 2)

## ----threshold, fig.width = 5.25, fig.height = 3.5, echo = FALSE--------------
# preliminary libraries and plotting defaults

library(ggplot2)
library(data.table)

my_theme <- function() {
  theme(panel.background = element_rect(fill = "grey90"), 
        panel.grid = element_blank(), 
        axis.ticks = element_line(colour = "black"), 
        panel.spacing = unit(0.25, "lines"), 
        plot.title = element_text(size = 12, vjust = 0.5, hjust = 0), 
        panel.border = element_rect(fill = NA, colour = "gray90"))
}

# create data points density curve 

x <- seq(-6, 6, length = 1000)
pdf <- dlogis(x, location = 0, scale = 1)
dt <- data.table(x, pdf)

# set thresholds for Group A

# P = c(0.31, 0.29, .20, 0.20)

thresholdA <- c(-0.8, 0.4, 1.4)

pdf <- dlogis(thresholdA)
grpA <- data.table(threshold = thresholdA, pdf)
aBreaks <- c(-6, grpA$threshold, 6)

# plot density with cut points

dt[, grpA := cut(x, breaks = aBreaks, labels = F, include.lowest = TRUE)]

p1 <- ggplot(data = dt, aes(x = x, y = pdf)) +
  geom_line() +
  geom_area(aes(x = x, y = pdf, group = grpA, fill = factor(grpA))) +
  geom_hline(yintercept = 0, color = "grey50") +
  annotate("text", x = -5, y = .28, label = "unexposed", size = 5) +
  scale_fill_manual(values = c("#d0d7d1", "#bbc5bc", "#a6b3a7", "#91a192", "#7c8f7d"),
                    labels = c("strongly disagree", "disagree", "agree", "strongly agree"),
                    name = "Frequency") +
  scale_x_continuous(breaks = thresholdA) +
  scale_y_continuous(limits = c(0, 0.3), name = "Density") +
  my_theme() +
  theme(legend.position = c(.85, .7),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(color = "grey90"))

p1

## ----plotB, fig.width = 5.25, fig.height = 3.5, echo = FALSE------------------

pA= plogis(c(thresholdA, Inf)) - plogis(c(-Inf, thresholdA))
probs <- data.frame(pA)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)")

probA <- data.frame(
           cprob = plogis(thresholdA), 
           codds = plogis(thresholdA)/(1-plogis(thresholdA)),
           lcodds = log(plogis(thresholdA)/(1-plogis(thresholdA)))
)
rownames(probA) <- c("P(Grp < 2)", "P(Grp < 3)", "P(Grp < 4)")

thresholdB <- thresholdA + 0.7

pdf <- dlogis(thresholdB)
grpB <- data.table(threshold = thresholdB, pdf)
bBreaks <- c(-6, grpB$threshold, 6)

pB = plogis(c(thresholdB, Inf)) - plogis(c(-Inf, thresholdB))
probs <- data.frame(pA, pB)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)")


# Plot density for group B

dt[, grpB := cut(x, breaks = bBreaks, labels = F, include.lowest = TRUE)]

p2 <- ggplot(data = dt, aes(x = x, y = pdf)) +
  geom_line() +
  geom_area(aes(x = x, y = pdf, group = grpB, fill = factor(grpB))) +
  geom_hline(yintercept = 0, color = "grey5") +
  geom_segment(data=grpA, 
               aes(x=threshold, xend = threshold, y=0, yend=pdf), 
               size = 0.3, lty = 2, color = "#857284") +
  annotate("text", x = -5, y = .28, label = "exposed", size = 5) +
  scale_fill_manual(values = c("#d0d7d1", "#bbc5bc", "#a6b3a7", "#91a192", "#7c8f7d"),
                    name = "Frequency") +
  scale_x_continuous(breaks = thresholdB) +
  scale_y_continuous(limits = c(0.0, 0.3), name = "Density") +
  my_theme() +
  theme(legend.position = "none")

p2

## ----acuts--------------------------------------------------------------------
baseprobs <- c(0.31, 0.29, .20, 0.20)

defA <- defData(varname = "exposed", formula = "1;1", dist = "trtAssign")
defA <- defData(defA, varname = "z", formula = "-0.7*exposed", dist = "nonrandom")

set.seed(130)

dT_1_cat <- genData(25000, defA)

dX <- genOrdCat(dT_1_cat, adjVar = "z", baseprobs, catVar = "r")

## ----ordinal------------------------------------------------------------------
library(ordinal)
clmFit <- clm(r ~ exposed, data = dX)
summary(clmFit)

## -----------------------------------------------------------------------------
(logOdds.unexp <- log(odds(cumsum(dX[exposed == 0, prop.table(table(r))])))[1:3])

## -----------------------------------------------------------------------------
(logOdds.expos <- log(odds(cumsum(dX[exposed == 1, prop.table(table(r))])))[1:3])

## -----------------------------------------------------------------------------
logOdds.expos - logOdds.unexp

## ----echo=FALSE, fig.width=6, fig.height=3.5----------------------------------
getCumProbs <- function(coefs) {
  
  cumprob0 <- data.table(
    cumprob = c(1/(1 + exp(-coefs[which(rownames(coefs) != "exposed")])), 1),
    r = factor(1 : nrow(coefs)),
    exposed = 0
  )
  
  cumprob1 <- data.table(
    cumprob = c(1/(1 + exp(-coefs[which(rownames(coefs) != "exposed")] + 
                             coefs["exposed", 1])), 1),
    r = factor(1 : nrow(coefs)),
    exposed = 1
  )
  
  rbind(cumprob0, cumprob1)[]
 
}

fitPlot <- function(dx) {
  
  clmFit <- clm(r ~ exposed, data = dx)
  coefs <- coef(summary(clmFit))
  cumModProbs <- getCumProbs(coefs)

  cumObsProbs <- dx[, .N, keyby = .(exposed, r)]
  cumObsProbs[, cumprob := cumsum(N)/sum(N) , keyby = exposed]
  
  ggplot(data = cumObsProbs, aes(x = r, y = cumprob, color = factor(exposed))) +
    geom_line(data = cumModProbs, alpha = 1, aes(group=exposed)) +
    geom_point(size = 1.25) +
    ylab("cumulative probability") +
    xlab("ordinal category") +
    theme(panel.grid = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(values = c("#7c8e8f", "#8f7c8e"), labels = c("Not exposed", "Exposed"))
}

fitPlot(dX)

## ----plotNP, fig.width = 5.25, fig.height = 3.5, echo = FALSE-----------------

pA <- plogis(c(thresholdA, Inf)) - plogis(c(-Inf, thresholdA))
probs <- data.frame(pA)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)")

probA <- data.frame(
  cprob = plogis(thresholdA), 
  codds = plogis(thresholdA)/(1-plogis(thresholdA)),
  lcodds = log(plogis(thresholdA)/(1-plogis(thresholdA)))
)

rownames(probA) <- c("P(Grp < 2)", "P(Grp < 3)", "P(Grp < 4)")

thresholdB <- thresholdA + c(0.4, 0.0, 1.7)

pdf <- dlogis(thresholdB)
grpB <- data.table(threshold = thresholdB, pdf)
bBreaks <- c(-6, grpB$threshold, 6)

pB = plogis(c(thresholdB, Inf)) - plogis(c(-Inf, thresholdB))
probs <- data.frame(pA, pB)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)")


# Plot density for group B

dt[, grpB := cut(x, breaks = bBreaks, labels = F, include.lowest = TRUE)]

p3 <- ggplot(data = dt, aes(x = x, y = pdf)) +
  geom_line() +
  geom_area(aes(x = x, y = pdf, group = grpB, fill = factor(grpB))) +
  geom_hline(yintercept = 0, color = "grey5") +
  geom_segment(data=grpA, 
               aes(x=threshold, xend = threshold, y=0, yend=pdf), 
               size = 0.3, lty = 2, color = "#857284") +
  annotate("text", x = -4, y = .28, label = "non-proportional", size = 5) +
  scale_fill_manual(values = c("#d0d7d1", "#bbc5bc", "#a6b3a7", "#91a192", "#7c8f7d"),
                    name = "Frequency") +
  scale_x_continuous(breaks = thresholdB) +
  scale_y_continuous(limits = c(0.0, 0.3), name = "Density") +
  my_theme() +
  theme(legend.position = "none")

p3

## -----------------------------------------------------------------------------
baseprobs <- c(0.31, 0.29, .20, 0.20)
npAdj <- c(-0.4, 0.0, -1.7, 0)

dX <- genOrdCat(dT_1_cat, baseprobs = baseprobs, adjVar = "z",
                catVar = "r", npVar = "exposed", npAdj = npAdj)

## -----------------------------------------------------------------------------
(logOdds.unexp <- log(odds(cumsum(dX[exposed == 0, prop.table(table(r))])))[1:3])

## -----------------------------------------------------------------------------
(logOdds.expos <- log(odds(cumsum(dX[exposed == 1, prop.table(table(r))])))[1:3])

## -----------------------------------------------------------------------------
logOdds.expos - logOdds.unexp

## ----echo=FALSE, fig.width=6, fig.height=3.5, warning=FALSE-------------------
fitPlot(dX)

## -----------------------------------------------------------------------------
baseprobs <- matrix(c(0.2, 0.1, 0.7,
                      0.7, 0.2, 0.1,
                      0.5, 0.2, 0.3,
                      0.4, 0.2, 0.4,
                      0.6, 0.2, 0.2), 
                    nrow = 5, byrow = TRUE)

# generate the data

set.seed(333)                     
dT_5_cat <- genData(10000)

dX <- genOrdCat(dT_5_cat, adjVar = NULL, baseprobs = baseprobs, 
                   prefix = "q", rho = 0.15, corstr = "cs", asFactor = FALSE)

## -----------------------------------------------------------------------------
round(dX[, cor(cbind(q1, q2, q3, q4, q5))], 2)

## -----------------------------------------------------------------------------
dM <- melt(dX, id.vars = "id")
dProp <- dM[ , prop.table(table(value)), by = variable]
dProp[, response := rep(seq(3), 5)]

# observed probabilities
dcast(dProp, variable ~ response, value.var = "V1", fill = 0)

# specified probabilities
baseprobs

## -----------------------------------------------------------------------------
dX <- genOrdCat(dT_5_cat, adjVar = NULL, baseprobs = baseprobs, 
                   prefix = "q", rho = 0.40, corstr = "ar1", asFactor = FALSE)

# correlation
round(dX[, cor(cbind(q1, q2, q3, q4, q5))], 2)

dM <- melt(dX, id.vars = "id")
dProp <- dM[ , prop.table(table(value)), by = variable]
dProp[, response := rep(seq(3), 5)]

# probabilities
dcast(dProp, variable ~ response, value.var = "V1", fill = 0)

