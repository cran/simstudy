% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{genSynthetic}
\alias{genSynthetic}
\title{Generate synthetic data}
\usage{
genSynthetic(dtFrom, n = nrow(dtFrom), vars = NULL, id = "id")
}
\arguments{
\item{dtFrom}{Data table that contains the source data}

\item{n}{Number of samples to draw from the source data. The default
is number of records that are in the source data file.}

\item{vars}{A vector of string names specifying the fields that will be
sampled. The default is that all variables will be selected.}

\item{id}{A string specifying the field that serves as the record id. The
default field is "id".}
}
\value{
A data table with the generated data
}
\description{
Synthetic data is generated from an existing data set
}
\examples{
### Create fake "real" data set

d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
d <- defData(d, varname = "b", formula = 5, dist = "poisson")
d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
d <- defData(d, varname = "d", formula = "a + b + 3*c", variance = 2, dist = "normal")

A <- genData(100, d, id = "index")

### Create synthetic data set from "observed" data set A:

def <- defDataAdd(varname = "x", formula = "2*b + 2*d", variance = 2)

S <- genSynthetic(dtFrom = A, n = 120, vars = c("b", "d"), id = "index")
S <- addColumns(def, S)

}
\concept{generate_data}
