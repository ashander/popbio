\name{net.reproductive.rate}
\alias{net.reproductive.rate}
\title{Net reproductive rate}
\usage{
  net.reproductive.rate(A, ...)
}
\arguments{
  \item{A}{projection matrix}

  \item{\dots}{additional items are passed to
  \code{\link{splitA}} and are used to split A into T and F
  matrices}
}
\value{
  Net reproductive rate.  If the transition matrix is
  singular, then NA is returned.
}
\description{
  Calculates the net reproductive rate of a stage
  classified matrix using the dominant eigenvalue of the
  matrix R.
}
\details{
  see section 5.3.4 in Caswell (2001).
}
\note{
  Previous versions required separate T and F matrices as
  input
}
\examples{
data(whale)
net.reproductive.rate(whale)
## fertilities in last column
data(teasel)
net.reproductive.rate(teasel, r=1:6, c=6)
## Plot 3 from Calathea - values are not the same as p. 105 in Caswell.
data(calathea)
sapply(calathea[9:12], net.reproductive.rate)
}
\author{
  Chris Stubben
}
\references{
  Caswell, H. 2001. Matrix population models: construction,
  analysis, and interpretation, Second edition. Sinauer,
  Sunderland, Massachusetts, USA.
}
\seealso{
  see \code{\link{fundamental.matrix}} and
  \code{\link{generation.time}} for other age-specific
  traits
}
\keyword{survey}

