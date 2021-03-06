\name{lambda}
\alias{lambda}
\title{Population growth rate}
\usage{
  lambda(A)
}
\arguments{
  \item{A}{A projection matrix}
}
\value{
  The dominant eigenvalue
}
\description{
  Calculates the population growth rate of a projection
  matrix
}
\details{
  see section 4.4 in Caswell (2001)
}
\note{
  The built-in \code{\link{eigen}} function returns
  eigenvalues in descreasing order of magnitude or modulus.
  The dominant eigenvalue of imprimitive matrices with
  \emph{d} eigenvalues of equal modulus is the one with the
  largest real part (\code{which.max(Re(eigen(A)$values))}
  ).
}
\examples{
A<-matrix(c(0,0,2,.3,0,0,0,.6,0), nrow=3,byrow=TRUE)
lambda(A)
# third
Re(eigen(A)$values)

data(hudsonia)
sapply(hudsonia, lambda)
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
  \code{\link{eigen}} and \code{\link{pop.projection}}
}
\keyword{survey}

