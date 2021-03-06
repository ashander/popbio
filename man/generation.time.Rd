\name{generation.time}
\alias{generation.time}
\title{Generation time}
\usage{
  generation.time(A, ...)
}
\arguments{
  \item{A}{projection matrix}

  \item{\dots}{additional items are passed to
  \code{\link{splitA}} and are used to split A into T and F
  matrices}
}
\value{
  Generation time. If the transition matrix is singular,
  then NA is returned.
}
\description{
  Calculates the generation time of a stage-classified
  matrix
}
\details{
  see section 5.3.5 in Caswell (2001).
}
\note{
  Previous versions required separate T and F matrices as
  input
}
\examples{
data(whale)
generation.time(whale)
## fertilities in last column
data(teasel)
generation.time(teasel, r=1:6, c=6)
## Plot 3 from Calathea
data(calathea)
sapply(calathea[9:12], generation.time)
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
  \code{\link{net.reproductive.rate}} for other
  age-specific traits
}
\keyword{survey}

