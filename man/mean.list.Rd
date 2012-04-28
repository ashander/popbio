\name{mean.list}
\alias{mean.list}
\title{Calculate mean matrix}
\usage{
  mean.list(x, ...)
}
\arguments{
  \item{x}{A list of two or more matrices}

  \item{\dots}{Additional arguments passed to
  \code{\link{rowMeans}}}
}
\value{
  The mean matrix
}
\description{
  Calculates mean matrix from a list of matrices
}
\details{
  Returns the mean matrix from a list of matrices using a
  combination of \code{\link{unlist}} and
  \code{\link{rowMeans}}.  See example for details.
}
\note{
  S3 method for the \code{\link{mean}} of a list of
  matrices.
}
\examples{
data(hudsonia)
mean(hudsonia)
## or
x <- matrix(unlist(hudsonia), ncol=length(hudsonia) )
matrix(rowMeans(x), 6, 6)
}
\author{
  Chris Stubben
}
\seealso{
  \code{\link{var2}}
}
\keyword{survey}

