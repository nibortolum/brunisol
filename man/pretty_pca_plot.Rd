% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pretty_pca_plot.r
\name{pretty_pca_plot}
\alias{pretty_pca_plot}
\title{Pretty PCA Plot}
\usage{
pretty_pca_plot(obj, dates, type = "default", ...)
}
\arguments{
\item{obj}{A PCA object computed with prcomp or princomp}

\item{dates}{a vector of dates}

\item{type}{`default` or `ggplot` to decide which kind of graph you want to do}

\item{...}{optional parameters to be passed to the plot function}
}
\value{
a beautiful plot
}
\description{
This function takes a PCA object and a dates vector. The dates are processed and split in day, months, and years.
    The pca is then plotted and points are connected through time, to see the evolution of the lake communities.
}
\examples{

year <- c(rep(2014,12), rep(2015, 12), rep(2016, 12))
month <- rep(1:12, 3)
day <- sample(1:28, 36, replace = TRUE)
my_date <- paste(year, month, day, sep="-")


data <- matrix(ncol = 10,
               nrow = 36,
               data = rnorm(360))

data.pca <- prcomp(data)
pretty_pca_plot(data.pca, my_date, main = "test plot")

}
