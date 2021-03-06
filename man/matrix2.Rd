\name{matrix2}
\alias{matrix2}
\title{Square matrices}
\usage{
  matrix2(x, stages, byrow = TRUE)
}
\arguments{
  \item{x}{a vector of matrix elements}

  \item{stages}{a vector of row names (also assigned to
  columns)}

  \item{byrow}{fill matrix by rows , default TRUE}
}
\value{
  a square matrix
}
\description{
  Create a square matrix from a given set of values
}
\examples{
#Centaurea corymbosa from Freville 2004
ceco<-c(0,0,5.905,0.368,0.639, 0.025, 0.001, 0.152, 0.051)
stages <- c("seedling", "vegetative", "flowering")
# shortcut for
#matrix(ceco, nrow=3, byrow=TRUE,  dimnames=list(stages,stages))
matrix2(ceco, stages)
}
\author{
  Chris Stubben
}
\seealso{
  \code{\link{matrix}}
}
\keyword{methods}

