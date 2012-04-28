\docType{data}
\name{hudcorrs}
\alias{hudcorrs}
\title{Correlation matrices for Hudsonia vital rates}
\format{A list with 2 correlation matrices, corrin (within year correlation)
and corrout (between year correlation).}
\source{
  The correlation matrices in
  \url{http://www.sinauer.com/PVA/hudcorrs.mat} include
  some correlations>1.  A corrected set of correlations was
  sent by the D. Doak on 8/4/2007.
}
\description{
  Within year and between year correlation matrices from
  \emph{Hudsonia montana} vital rates.  Correlations were
  calculated from first 13 growth and survival rates only,
  since fertility rates vary little.
}
\examples{
data(hudcorrs)
hudcorrs$corrin
}
\author{
  Original dataset from Morris and Doak (2002)
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
\keyword{datasets}

