\docType{data}
\name{whale}
\alias{whale}
\title{Projection matrix for killer whale}
\format{A projection matrix.}
\source{
  Projection matrix from Example 5.1 in Caswell (2001)
}
\description{
  Projection matrix for killer whales
}
\examples{
data(whale)
whale
splitA(whale)
lambda(whale)
sensitivity(whale)
# plot sensitivity
matplot2(sensitivity(whale), type='b', legend='topleft', ltitle='Fate',
main='Killer Whale sensitivity')
}
\references{
  Caswell, H. 2001. Matrix population models: construction,
  analysis, and interpretation, Second edition. Sinauer,
  Sunderland, Massachusetts, USA.
}
\keyword{datasets}

