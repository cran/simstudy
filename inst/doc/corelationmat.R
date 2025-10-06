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

## ----message=FALSE------------------------------------------------------------
library(simstudy)
library(data.table)
set.seed(37265)

genCorMat(4)

## ----message=FALSE------------------------------------------------------------
R <- genCorMat(4, cors = c(0.6, 0.4, 0.2, 0.5, 0.3, 0.8))
R

## -----------------------------------------------------------------------------
dd <- genCorGen(n = 1000, nvars = 4, corMatrix = R, params1 = c(3, 5, 8, 9), 
  dist = "poisson", wide = TRUE)

head(dd)

## -----------------------------------------------------------------------------
round(cor(as.matrix(dd[, -1])), 1)

## ----echo=FALSE---------------------------------------------------------------
matform <- "
R = \\left (
\\begin{matrix} 
1.0 & \\rho & \\rho & \\rho \\\\
\\rho & 1.0 & \\rho & \\rho \\\\
\\rho & \\rho & 1.0 & \\rho \\\\
\\rho & \\rho & \\rho & 1.0
\\end{matrix}
\\right )
"

katex::katex_html(matform, include_css = TRUE)

## -----------------------------------------------------------------------------
genCorMat(nvars = 4, rho = 0.6, corstr = "cs")

## ----echo=FALSE---------------------------------------------------------------
matform <- "
R = \\left (
\\begin{matrix} 
1.0 & \\rho & \\rho^2 & \\rho^3 \\\\
\\rho & 1.0 & \\rho & \\rho^2 \\\\
\\rho^2 & \\rho & 1.0 & \\rho \\\\
\\rho^3 & \\rho^2 & \\rho & 1.0
\\end{matrix}
\\right )
"

katex::katex_html(matform)

## -----------------------------------------------------------------------------
genCorMat(nvars = 4, rho = 0.6, corstr = "ar1")

## ----echo=FALSE---------------------------------------------------------------
matform <- "\\footnotesize{

R = \\left ( \\begin{array}{c|c|c|c}

\\begin{matrix} 
1.0 & 0.6 \\\\
0.6 & 1.0
\\end{matrix} &
  
\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
0.0 & 0.0 \\\\
0.0 & 0.0 \\\\
0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
1.0 & 0.7 & 0.7  \\\\
0.7 & 1.0 & 0.7  \\\\
0.7 & 0.7 & 1.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
0.0 & 0.0 \\\\
0.0 & 0.0 \\\\
0.0 & 0.0 \\\\
0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
1.0 & 0.5 & 0.5 & 0.5 \\\\
0.5 & 1.0 & 0.5 & 0.5 \\\\
0.5 & 0.5 & 1.0 & 0.5 \\\\
0.5 & 0.5 & 0.5 & 1.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
0.0 & 0.0 \\\\
0.0 & 0.0 \\\\
0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
0.0 & 0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 & 0.0 \\\\
0.0 & 0.0 & 0.0 & 0.0
\\end{matrix} &

\\begin{matrix} 
1.0 & 0.4 & 0.4  \\\\
0.4 & 1.0 & 0.4  \\\\
0.4 & 0.4 & 1.0
\\end{matrix} \\\\

\\end{array} \\right ) }"

katex::katex_html(matform)

## -----------------------------------------------------------------------------
RC <- genCorMat(nvars = c(2, 3, 4, 3), rho = c(0.6, 0.7, 0.5, 0.4), 
  corstr = "cs", nclusters = 4)

RC

## -----------------------------------------------------------------------------
d1 <- defData(varname = "n", formula = "c(2, 3, 4, 3)", dist = "nonrandom")
d1 <- defData(d1, varname = "lambda", formula = "c(6, 7, 9, 8)", dist = "nonrandom")

ds <- genData(4, d1, id = "site")
dc <- genCluster(dtClust = ds, cLevelVar = "site", numIndsVar = "n", "id")

## -----------------------------------------------------------------------------
dd <- addCorGen(dc, idvar = "site", param1 = "lambda", corMatrix = RC,
          dist = "poisson", cnames = "y", method = "copula")

dd

## ----eval=FALSE---------------------------------------------------------------
# replicate <- function(R, dc) {
#   reps <- lapply(1:5000, function(x)
#   addCorGen(dc, idvar = "site", param1 = "lambda", corMatrix = R,
#     dist = "poisson", cnames = "y", method = "copula")
#   )
# 
#   drep <- data.table::rbindlist(reps, idcol = "rep")
#   drep[, seq := 1:.N, keyby = rep]
#   dmat <- as.matrix(dcast(drep, rep ~ seq, value.var = "y")[, -1])
#   round(cor(dmat), 1)
# }
# 
# replicate(R = RC, dc = dc)
# 

## ----eval=TRUE----------------------------------------------------------------
d1 <- defData(varname = "n", formula = 20, dist = "noZeroPoisson")
d1 <- defData(d1, varname = "mu", formula = 10, variance = 8, dist = "normal")
d1 <- defData(d1, varname = "s2", formula = 4, dist = "nonrandom")

ds <- genData(100, d1, id = "site")
dc <- genCluster(dtClust = ds, cLevelVar = "site", numIndsVar = "n", "id")

n <- dc[, .N, keyby = site][, N]
nsites <- length(n)
rho <- rbeta(nsites, 25, 15)

RM <- genCorMat(nvars = n, rho = rho, corstr = "cs", nclusters = nsites)

## -----------------------------------------------------------------------------
lapply(RM[c(1, 38, 97)], function(x) x[1:3, 1:3])
lapply(RM[c(1, 38, 97)], function(x) dim(x))

## -----------------------------------------------------------------------------
dd <- addCorGen(dc, idvar = "site", param1 = "mu", param2 = "s2",
                corMatrix = RM, dist = "normal", cnames = "y", method = "copula")

dd

## ----echo=FALSE---------------------------------------------------------------
library(katex)
matform <- "\\footnotesize{

R = \\left ( \\begin{array}{c|c|c}

\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b
\\end{matrix} &
  
  \\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b 
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_b & \\rho_b \\\\
\\rho_b & \\rho_b 
\\end{matrix} &
  
\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1 
\\end{matrix}

\\end{array} \\right ) }"

katex_html(matform)

## ----echo=FALSE---------------------------------------------------------------
matform <- "\\footnotesize{

R = \\left ( \\begin{array}{c|c|c}

\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
\\begin{matrix} 
\\rho_w r & \\rho_w r \\\\
\\rho_w r & \\rho_w r
\\end{matrix} &
  
\\begin{matrix} 
\\rho_w r^2 & \\rho_w r^2 \\\\
\\rho_w r^2 & \\rho_w r^2
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_w r & \\rho_w r \\\\
\\rho_w r & \\rho_w r
\\end{matrix}&
  
  \\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_w r & \\rho_w r \\\\
\\rho_w r & \\rho_w r
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_w r^2 & \\rho_w r^2 \\\\
\\rho_w r^2 & \\rho_w r^2
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_w r & \\rho_w r \\\\
\\rho_w r & \\rho_w r
\\end{matrix} &
  
\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1 
\\end{matrix}

\\end{array} \\right ) }"

katex_html(matform)

## ----echo=FALSE---------------------------------------------------------------
matform <- "\\footnotesize{

R = \\left ( \\begin{array}{c|c|c}

\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a
\\end{matrix} &
  
  \\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a 
\\end{matrix} &
  
  \\begin{matrix} 
\\rho_a & \\rho_b \\\\
\\rho_b & \\rho_a 
\\end{matrix} &
  
\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1 
\\end{matrix}

\\end{array} \\right ) }"

katex_html(matform)

## ----echo=FALSE---------------------------------------------------------------
matform <- "\\footnotesize{

R = \\left ( \\begin{array}{c|c|c}

\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
\\begin{matrix} 
r & \\rho_w r \\\\
\\rho_w r & r
\\end{matrix} &
  
\\begin{matrix} 
r^2 & \\rho_w r^2 \\\\
\\rho_w r^2 & r^2
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
r & \\rho_w r \\\\
\\rho_w r & r
\\end{matrix}&
  
  \\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1
\\end{matrix} &
  
  \\begin{matrix} 
r & \\rho_w r \\\\
\\rho_w r & r
\\end{matrix} \\\\

\\hline

\\begin{matrix} 
r^2 & \\rho_w r^2 \\\\
\\rho_w r^2 & r^2
\\end{matrix} &
  
  \\begin{matrix} 
r & \\rho_w r \\\\
\\rho_w r & r
\\end{matrix} &
  
\\begin{matrix} 
1 & \\rho_w \\\\
\\rho_w & 1 
\\end{matrix}

\\end{array} \\right ) }"

katex_html(matform)

## -----------------------------------------------------------------------------
library(simstudy)
library(data.table)

R_XE <- blockExchangeMat(ninds = 2 , nperiods = 3, rho_w = 0.5,
  rho_b = 0.3, pattern = "xsection")

R_XE

## -----------------------------------------------------------------------------
dd <- genCorGen(n = 5000, nvars = 6, corMatrix = R_XE,
  dist = "poisson", params1 = 7, wide = TRUE)

round(cor(as.matrix(dd[, -1])), 2)

## -----------------------------------------------------------------------------
R_XD <- blockDecayMat(ninds = 2 , nperiods = 3, rho_w = 0.5,
  r = 0.8, pattern = "xsection")

R_XD

dd <- genCorGen(n = 5000, nvars = 6, corMatrix = R_XD,
  dist = "poisson", params1 = 7, wide = TRUE)

## ----echo=FALSE---------------------------------------------------------------
round(cor(as.matrix(dd[, -1])), 2)

## -----------------------------------------------------------------------------
R_CE <- blockExchangeMat(ninds = 2 , nperiods = 3, rho_w = 0.5, 
  rho_b = 0.3, rho_a = 0.4, pattern = "cohort")

R_CE

dd <- genCorGen(n = 5000, nvars = 6, corMatrix = R_CE,
  dist = "poisson", params1 = 7, wide = TRUE)

## ----echo=FALSE---------------------------------------------------------------
round(cor(as.matrix(dd[, -1])), 2)

## -----------------------------------------------------------------------------
R_CD <- blockDecayMat(ninds = 2 , nperiods = 3, rho_w = 0.5, 
  r = 0.8, pattern = "cohort")

R_CD

dd <- genCorGen(n = 5000, nvars = 6, corMatrix = R_CD,
  dist = "poisson", params1 = 7, wide = TRUE)

## ----echo=FALSE---------------------------------------------------------------
round(cor(as.matrix(dd[, -1])), 2)

## -----------------------------------------------------------------------------
defC <- defData(varname = "lambda", formula = "sample(5:10, 1)", dist = "nonrandom")
defP <- defDataAdd(varname = "n", formula = "2;4", dist="uniformInt")

dc <- genData(n = 10, dtDefs = defC, id = "site")
dc <- addPeriods(dtName = dc, nPeriods = 3, 
                 idvars = "site", perName = "period")
dc <- addColumns(defP, dc)

dd <- genCluster(dtClust = dc, cLevelVar = "timeID", 
  numIndsVar = "n", level1ID = "id")


## -----------------------------------------------------------------------------
dc[site %in% c(1, 3, 7), .(site, period, n)]

## -----------------------------------------------------------------------------
r <- round(rbeta(10, 6, 2), 2)
r[c(1, 3, 7)]

## -----------------------------------------------------------------------------
N <- dd[, .N, keyby = .(site, period)][, N]

R <- blockDecayMat(ninds = N , nperiods = 3, rho_w = 0.6, r = r, nclusters = 10)

lapply(R, function(x) round(x,2))[c(1, 3, 7)]

## ----eval=FALSE---------------------------------------------------------------
# reps <- lapply(1:5000,
#   function(x) addCorGen(dd, idvar = "site", corMatrix = R,
#     dist = "poisson", param1 = "lambda", cnames = "y")
# )
# 
# drep <- data.table::rbindlist(reps, idcol = "rep")
# 
# empir_corr <- function(cluster) {
#   dcrep <- drep[site == cluster, ]
#   dcrep[, seq := 1:.N, keyby = rep]
#   dmat <- as.matrix(dcast(dcrep, rep ~ seq, value.var = "y")[, -1])
# 
#   return(round(cor(dmat), 2))
# }
# 
# 
# empir_corr(cluster = 1)
# empir_corr(cluster = 3)
# empir_corr(cluster = 7)

