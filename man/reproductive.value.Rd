\name{reproductive.value}
\alias{reproductive.value}
\title{Stable stage distribution}
\usage{
  reproductive.value(A)
}
\arguments{
  \item{A}{A projection matrix}
}
\value{
  A vector containing the reproductive values scaled so
  v[1]=1
}
\description{
  Calculates the reproductive values of a projection matrix
}
\details{
  see section 4.5 in Caswell (2001).
}
\examples{
data(teasel)
v<-reproductive.value(teasel)
v
dotchart(log10(v), pch=16, xlab="Reproductive value (log10)")
}
\author{
  Chris Stubben
}
\references{
  Caswell, H. 2001. Matrix population models: construction,
  analysis, and interpretation, Second edition. Sinauer,
  Sunderland, Massachusetts, USA.
}
\keyword{survey}

