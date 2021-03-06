\name{hudmxdef}
\alias{hudmxdef}
\title{Matrix definition program for Hudsonia vital rates}
\source{
  \url{http://www.sinauer.com/PVA/hudmxdef.m}
}
\usage{
  hudmxdef(vrs)
}
\arguments{
  \item{vrs}{Vital rate means in \code{\link{hudvrs}}}
}
\value{
  A projection matrix
}
\description{
  Creates a projection matrix from \emph{Hudsonia} vital
  rates (survival, growth, and reproduction).  Growth rates
  are defined as a set of binomial choices as in Table 8.4
  B in Morris and Doak (2002).
}
\examples{
data(hudvrs)
hudmxdef(hudvrs$mean)
}
\author{
  Original MATLAB code by Morris and Doak (2002)
}
\references{
  Morris, W. F., and D. F. Doak. 2002. Quantitative
  conservation biology: Theory and practice of population
  viability analysis. Sinauer, Sunderland, Massachusetts,
  USA.
}
\seealso{
  \code{\link{vitalsim}}
}
\keyword{survey}

