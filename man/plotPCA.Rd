% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_demo.r
\name{plotPCA}
\alias{plotPCA}
\title{Plot PCA for a data frame}
\usage{
plotPCA(data, confidence = 95, nonDatCols = c("Sample"), ggTitle = NULL, ...)
}
\arguments{
\item{data:}{Data.frame}

\item{confidence:}{Int/Float - Hotellings confidence (usually 95\%)}
}
\value{
ggplot object of PCA scores
}
\description{
Plot PCA for a data frame
}
