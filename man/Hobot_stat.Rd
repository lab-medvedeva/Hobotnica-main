\name{Hobot_stat}
\alias{Hobot_stat}
\title{Hobotnica algorithm}
\usage{
Hobot_stat(distMatrix, annotation)
}
\description{
Calculates Hobotnica score on dataset given grouping
}
\examples{
data(iris)
library(amap)
distMatrix = Dist(t(iris[, -5]))
annotation = iris[, 5]
Hobot_stat(distMatrix, annotation)
}
