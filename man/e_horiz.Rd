% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_cluster_utils.R
\name{e_horiz}
\alias{e_horiz}
\title{Horizontal representation for centers of clusters.}
\usage{
e_horiz(centros, colores = NULL)
}
\arguments{
\item{centros}{a data.frame object with the centers of the clusters.}

\item{colores}{a vector of color for each cluster.}
}
\value{
echarts4r plot
}
\description{
Horizontal representation for centers of clusters.
}
\examples{
clusters <- factor(kmeans(iris[, -5], 3)$cluster)
c <- calc.centros(iris[, -5], clusters)
e_horiz(c$real, c("steelblue", "pink", "forestgreen"))

}
\author{
Diego Jimenez <diego.jimenez@promidat.com>
}
