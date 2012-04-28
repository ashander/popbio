\name{var2}
\alias{var2}
\title{Calculate a variance matrix}
\usage{
  var2(x)
}
\arguments{
  \item{x}{A list of two or more matrices}
}
\value{
  A matrix containing variances
}
\description{
  Calculates the variances from a list of matrices
}
\details{
  Returns a matrix containing variances from a list of
  matrices using a combination of \code{\link{unlist}} and
  \code{\link{apply}}.
}
\examples{
data(hudsonia)
var2(hudsonia)
}
\author{
  Chris Stubben
}
\keyword{survey}

