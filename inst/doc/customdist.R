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


## -----------------------------------------------------------------------------
zeroBeta <- function(n, a, b, p0) {
  betas <- rbeta(n, a, b)
  is.zero <- rbinom(n, 1, p0)
  betas*!(is.zero)
}

## -----------------------------------------------------------------------------
def <- defData(
  varname = "zb", 
  formula = "zeroBeta", 
  variance = "a = 0.75, b = 0.75, p0 = 0.02", 
  dist = "custom"
)

## -----------------------------------------------------------------------------
set.seed(1234)
dd <- genData(100000, def)

## ----echo = FALSE-------------------------------------------------------------
dd

## ----fig.width = 6, fig.height = 3, echo = FALSE------------------------------
ggplot(data = dd, aes(x = zb)) +
  geom_histogram(binwidth = 0.01, boundary = 0, fill = "grey60") +
  theme(panel.grid = element_blank()) 

## -----------------------------------------------------------------------------
rnormt <- function(n, min, max, mu, s) {
  
  F.a <- pnorm(min, mean = mu, sd = s)
  F.b <- pnorm(max, mean = mu, sd = s)
  
  u <- runif(n, min = F.a, max = F.b)
  qnorm(u, mean = mu, sd = s)
  
}

## -----------------------------------------------------------------------------
def <-
  defData(
    varname = "limit", 
    formula = "1/4;1/2;1/4",
    dist = "categorical"
  ) |>
  defData(
    varname = "tn", 
    formula = "rnormt", 
    variance = "min = -limit, max = limit, mu = ..M, s = 1.5",
    dist = "custom"
  )

## -----------------------------------------------------------------------------
mus <- c(-1, 0, 1)
dd <-lapply(mus, function(M) genData(100000, def))

## ----echo=FALSE---------------------------------------------------------------
lapply(dd, function(D) head(D))

## ----fig.width = 8, fig.height = 6, echo = FALSE------------------------------
pfunc <- function(dx, i) {
  ggplot(data = dx, aes(x = tn)) +
    geom_histogram(aes(fill = factor(limit)), binwidth = 0.05, boundary = 0, alpha = .8) +
    facet_grid( ~ limit) +
    theme(panel.grid = element_blank(),
          legend.position = "none") +
    scale_fill_manual(values = plotcolors) +
    scale_x_continuous(breaks = seq(-3, 3, by =1)) +
    scale_y_continuous(limits = c(0, 1000)) +
    ggtitle(paste("mu =", mus[i]))
}

plist <- lapply(seq_along(dd), function(a) pfunc(dd[[a]], a))
grid.arrange(grobs = plist, nrow = 3)

