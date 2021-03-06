% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differences.R
\name{differences}
\alias{differences}
\title{A function that takes two means, sd's, n's and gives lower and upper bounds for estimated difference and cohen d}
\usage{
differences(mean1, mean2, sd1, sd2, n1, n2, alpha = 0.95)
}
\arguments{
\item{mean1}{mean 1}

\item{mean2}{mean 2}

\item{sd1}{sd 1}

\item{sd2}{sd 2}

\item{n1}{sample size 1}

\item{n2}{sample size 2}

\item{alpha}{alpha level}
}
\description{
This function will give you a confidence interval around a difference and its associated cohen's d
}
\details{
formula for d = (mu1 - mu2) / sigma
where sigma = sqrt ( a / b ) = pooled standard deviation
where a = (n-1) x s1 x s1 + (n-2) x s2 x s2
where b = n1 + n2 - 2

also: t = ( mean(x1) - mean(x2) ) / (sigma  * sqrt(1/n1 + 1/n2))
so t = d * 1 / sqrt(1/n1 + 1/n2)
so d = t * sqrt(1/n1 + 1/n2)
}
\examples{
differences(mean1, mean2, sd1, sd2, n1, n2)
}
\keyword{effect}
\keyword{size}
