% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contrast.network.R
\name{calculate.contrast.deviance}
\alias{calculate.contrast.deviance}
\title{Find deviance statistics such as DIC and pD.}
\usage{
calculate.contrast.deviance(result)
}
\arguments{
\item{result}{Object created by \code{\link{contrast.network.run}} function}
}
\value{
\item{Dbar}{Overall residual deviance}
\item{pD}{Sum of leverage_arm (i.e. total leverage)}
\item{DIC}{Deviance information criteria (sum of Dbar and pD)}
\item{resdev_study}{Posterior mean of the residual deviance in each study}
\item{devtilda_study}{Deviance at the posterior mean of the fitted values}
\item{leverage_study}{Difference between resdev_study and devtilda_study for each trial}
}
\description{
Calculates deviance statistics. This function automatically called in \code{\link{contrast.network.run}} and the deviance statistics are stored after sampling is finished.
}
\examples{
network <- with(parkinsons_contrast, {
 contrast.network.data(Outcomes, Treat, SE, na, V)
})
\donttest{
result <- contrast.network.run(network)
calculate.contrast.deviance(result)
}
}
\references{
A.J. Franchini, S. Dias, A.E. Ades, J.P. Jansen, N.J. Welton (2012), \emph{Accounting for correlation in network meta-analysis with multi-arm trials}, Research Synthesis Methods 3(2):142-160. \doi{10.1002/jrsm.1049}

S. Dias, A.J. Sutton, A.E. Ades, and N.J. Welton (2013a), \emph{A Generalized Linear Modeling Framework for Pairwise and Network Meta-analysis of Randomized Controlled Trials}, Medical Decision Making 33(5):607-617. \doi{10.1177/0272989X12458724}
}
