% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_kmedias_utils.R
\name{e_silhouette}
\alias{e_silhouette}
\title{Silhouette plot}
\usage{
e_silhouette(data, max.clusters)
}
\arguments{
\item{data}{a data.frame object.}

\item{max.clusters}{a numeric value specifying the number of times to generate the model.}
}
\value{
echarts4r plot
}
\description{
Silhouette plot
}
\examples{
e_silhouette(iris[, -5], 10)

}
\author{
Diego Jimenez <diego.jimenez@promidat.com>
}
