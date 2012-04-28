\name{stable.stage}
\alias{stable.stage}
\title{Stable stage distribution}
\usage{
  stable.stage(A)
}
\arguments{
  \item{A}{A projection matrix}
}
\value{
  A vector containing the stable stage distribution
}
\description{
  Calculates the stable stage distribution of a projection
  matrix
}
\details{
  see section 4.5 in Caswell (2001).
}
\examples{
data(teasel)
w<-stable.stage(teasel)
w
barplot( w,  col="green", ylim=c(0,1), las=1,
      ylab="Stable stage proportion", xlab="Stage class", main="Teasel")
box()
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

