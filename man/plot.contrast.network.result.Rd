% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contrast.network.R
\name{plot.contrast.network.result}
\alias{plot.contrast.network.result}
\title{Plot traceplot and posterior density of the result using contrast data}
\usage{
\method{plot}{contrast.network.result}(x, ...)
}
\arguments{
\item{x}{Result object created by \code{\link{contrast.network.run}} function}

\item{...}{Additional arguments affecting the plot produced}
}
\value{
None
}
\description{
This function uses plotting function in coda package to plot mcmc.list object
}
\examples{
network <- with(parkinsons_contrast, {
 contrast.network.data(Outcomes, Treat, SE, na, V)
})
\donttest{
result <- contrast.network.run(network)
plot(result)
}
}
