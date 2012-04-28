\docType{data}
\name{woodpecker}
\alias{woodpecker}
\title{Survirvorship data for adult and juvenile Acorn Woodpeckers}
\format{A data frame with 18 observations on the following 4 variables.
\describe{ \item{list("rate")}{ Adult or juvenile stage} \item{list("year")}{
Year} \item{list("start")}{ Total number of starting individuals}
\item{list("surv")}{ Number surviving to spring} }}
\source{
  Stacey, P.B., and M. Taper. 1992. Environmentl variation
  and the persistence of small populations. Ecological
  Applications 2: 18-29.
}
\description{
  Number of juvenile and adult Acorn Woodpeckers and
  survival in the Water Canyon, New Mexico population,
  reconstructed from Stacey and Taper (1992).
}
\examples{
data(woodpecker)
woodpecker
with(subset(woodpecker, rate=='adult'),
plot(year, start, type='o', pch=16,
ylab="Number of adults", xlab="Year",
main="Acorn Woodpeckers in Water Canyon"))
##stage-specific survival rate
x<-aggregate(list(Nstart=woodpecker$start, Nsurv=woodpecker$surv),
list(stage=woodpecker$rate), sum)
x$survival<-x[,3]/x[,2]
x
}
\references{
  Akcakaya, H. R. 2002. Estimating the variance of survival
  rates and fecundities. Animal Conservation 5: 333-336.

  Kendall, B. E. 1998. Estimating the magnitude of
  environmental stochasticity in survivorship data.
  Ecological Applications 8(1): 184-193.
}
\seealso{
  \code{\link{Kendall}} and \code{\link{varEst}}
}
\keyword{datasets}

