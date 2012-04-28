\docType{data}
\name{aq.census}
\alias{aq.census}
\title{Annual census data for Aquilegia in the southwestern US}
\format{A data frame with 2853 observations on the following 8 variables.
\describe{ \item{list("plot")}{Plot number} \item{list("year")}{Year of
census} \item{list("plant")}{Plant id number} \item{list("status")}{Plant
status recorded in field: dead, dormant, recruit0 (with cotyledons only),
recruit1, flowering or vegetative. } \item{list("rose")}{Total number of
rosettes} \item{list("leaf")}{Total number of leaves}
\item{list("infl")}{Total number of infloresences or flowering stalks}
\item{list("fruits")}{Total number of mature fruits} }}
\source{
  Data set owners: Brook Milligan, Chris Stubben, Allan
  Strand
}
\description{
  Demography census data from \emph{Aquilegia chrysantha}
  in Fillmore Canyon, Organ Mountains, New Mexico,
  1996-2003.
}
\details{
  This sample data set includes census data from 10 of the
  15 total demography plots established in 1995.  Please
  contact the data set owners to access the complete census
  data from 1995-2006.
}
\examples{
data(aq.census)
sv<-table(aq.census$status, aq.census$year)
sv
stage.vector.plot(sv[-1,], prop=FALSE)
}
\seealso{
  \code{\link{aq.trans}} for annual transitions with stage
  and fate in same row
}
\keyword{datasets}

