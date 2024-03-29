% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_correlated_data.R
\name{genCorGen}
\alias{genCorGen}
\title{Create multivariate (correlated) data - for general distributions}
\usage{
genCorGen(
  n,
  nvars,
  params1,
  params2 = NULL,
  dist,
  rho,
  corstr,
  corMatrix = NULL,
  wide = FALSE,
  cnames = NULL,
  method = "copula",
  idname = "id"
)
}
\arguments{
\item{n}{Number of observations}

\item{nvars}{Number of variables}

\item{params1}{A single vector specifying the mean of the distribution. The vector is of
length 1 if the mean is the same across all observations, otherwise the vector is of length
nvars. In the case of the uniform distribution the vector specifies the minimum.}

\item{params2}{A single vector specifying a possible second parameter for the distribution.
For the normal distribution, this will be the variance; for the gamma distribution, this
will be the dispersion; and for the uniform distribution, this will be the maximum. The
vector is of length 1 if the mean is the same across all observations, otherwise the vector
is of length nvars.}

\item{dist}{A string indicating "binary", "poisson" or "gamma", "normal", or "uniform".}

\item{rho}{Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.}

\item{corstr}{Correlation structure of the variance-covariance matrix
defined by sigma and rho. Options include "cs" for a compound symmetry structure
and "ar1" for an autoregressive structure.}

\item{corMatrix}{Correlation matrix can be entered directly. It must be symmetrical and
positive semi-definite. It is not a required field; if a matrix is not provided, then a
structure and correlation coefficient rho must be specified.}

\item{wide}{The layout of the returned file - if wide = TRUE, all new correlated
variables will be returned in a single record, if wide = FALSE, each new variable
will be its own record (i.e. the data will be in long form). Defaults to FALSE.}

\item{cnames}{Explicit column names. A single string with names separated
by commas. If no string is provided, the default names will be V#, where #
represents the column.}

\item{method}{Two methods are available to generate correlated data. (1) "copula" uses
the multivariate Gaussian copula method that is applied to all other distributions; this
applies to all available distributions. (2) "ep" uses an algorithm developed by
Emrich and Piedmonte (1991).}

\item{idname}{Character value that specifies the name of the id variable.}
}
\value{
data.table with added column(s) of correlated data
}
\description{
Create multivariate (correlated) data - for general distributions
}
\examples{
set.seed(23432)
lambda <- c(8, 10, 12)

genCorGen(100, nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs")
genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs")
genCorGen(100, nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)

genCorGen(100,
  nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs",
  cnames = "new_var"
)
genCorGen(100,
  nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs",
  wide = TRUE, cnames = "a, b, c"
)
}
\references{
Emrich LJ, Piedmonte MR. A Method for Generating High-Dimensional
Multivariate Binary Variates. The American Statistician 1991;45:302-4.
}
\concept{correlated}
