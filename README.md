
<!-- README.md is generated from README.Rmd. Please edit that file -->
The simstudy package is collection of functions that allow users to generate simulated data sets in order to explore modeling techniques or better understand data generating processes. The user specifies a set of relationships between covariates, and generates data based on these specifications. The final data sets can represent data from randomized control trials, repeated measure (longitudinal) designs, and cluster randomized trials. Missingness can be generated using various mechanisms (MCAR, MAR, NMAR).

Here is some simple sample code, much more in the vignette:

``` r
library(simstudy)
```

    ## Loading required package: data.table

``` r
def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dt <- genData(250, def)

dt <- trtAssign(dt, nTrt = 4, grpName = "grp", balanced = TRUE)

dt
```

    ##       id grp         x        y
    ##   1:   1   3 10.393817 7.805703
    ##   2:   2   1 10.235161 5.705590
    ##   3:   3   1 11.517813 8.210183
    ##   4:   4   1 12.068125 8.618601
    ##   5:   5   1 10.078817 5.780655
    ##  ---                           
    ## 246: 246   4 11.419577 8.442363
    ## 247: 247   3 10.567231 9.808930
    ## 248: 248   1 10.451896 7.720858
    ## 249: 249   3  7.633381 6.861638
    ## 250: 250   2  9.347781 6.094965
