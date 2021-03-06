% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_data.R
\name{trtStepWedge}
\alias{trtStepWedge}
\title{Assign treatment for stepped-wedge design}
\usage{
trtStepWedge(
  dtName,
  clustID,
  nWaves,
  lenWaves,
  startPer,
  perName = "period",
  grpName = "rx",
  lag = 0,
  xrName = "xr"
)
}
\arguments{
\item{dtName}{data table}

\item{clustID}{string representing name of column of cluster level ids}

\item{nWaves}{number of treatment waves}

\item{lenWaves}{the number of periods between waves}

\item{startPer}{the starting period of the first wave}

\item{perName}{string representing name of column of time periods}

\item{grpName}{string representing variable name for treatment or
exposure group}

\item{lag}{integer representing length of transition period}

\item{xrName}{string representing name of the field that
indicates whether the cluster status is in transition status}
}
\value{
A data.table with the added treatment assignment
}
\description{
Assign treatment for stepped-wedge design
}
\examples{
defc <- defData(
  varname = "ceffect", formula = 0, variance = 0.10,
  dist = "normal", id = "cluster"
)
defc <- defData(defc, "m", formula = 10, dist = "nonrandom")

# Will generate 3 waves of 4 clusters each - starting 2, 5, and 8

dc <- genData(12, defc)
dp <- addPeriods(dc, 12, "cluster")
dp <- trtStepWedge(dp, "cluster",
  nWaves = 3,
  lenWaves = 3, startPer = 2
)
dp

dp <- addPeriods(dc, 12, "cluster")
dp <- trtStepWedge(dp, "cluster",
  nWaves = 2,
  lenWaves = 1, startPer = 4, lag = 3
)
dp
}
\seealso{
\code{\link{trtObserve} \link{trtAssign}}
}
\concept{group_data}
