\docType{data}
\name{monkeyflower}
\alias{monkeyflower}
\title{Projection matrices for monkeyflower}
\format{A data frame with 32 projection matrices, arranged with one matrix
per row \describe{ \item{list("species")}{M. cardinalis or M. lewisii}
\item{list("site")}{Study site} \item{list("year")}{Start year of projection
interval or pooled for all three years} \item{list("a11")}{matrix element
a11; seed to seed transition or seed bank survival} \item{list("a12")}{matrix
element a12; small nr to seed - fertility} \item{list("a13")}{matrix element
a13; large nr to seed - fertility} \item{list("a14")}{matrix element a14;
reprod to seed - fertility} \item{list("a21")}{matrix element a21; seed to
small nr - growth} \item{list("a22")}{matrix element a22; small nr to small
nr -stasis} \item{list("a23")}{matrix element a23; large nr to small nr -
regress} \item{list("a24")}{matrix element a24; reprod to small nr - regress}
\item{list("a31")}{matrix element a31; seed to large nr - growth }
\item{list("a32")}{matrix element a32; small nr to large nr - growth }
\item{list("a33")}{matrix element a33; large nr to large nr - stasis }
\item{list("a34")}{matrix element a34; reprod to large nr - regress }
\item{list("a41")}{matrix element a41; seed to reprod - growth }
\item{list("a42")}{matrix element a42; small nr to reprod - growth }
\item{list("a43")}{matrix element a43; large nr to reprod - growth }
\item{list("a44")}{matrix element a44; reprod to reprod - stasis } }}
\source{
  \url{http://www.esapubs.org/archive/ecol/E087/126/appendix-E.htm}
}
\description{
  Pooled and annual projection matrices of central and
  marginal populations of monkeyflowers (\emph{Mimulus
  cardinalis} and \emph{M. lewisii})
}
\details{
  Matrix constructed using a post-breeding census with four
  stage classes: Seeds, small non-reproductive, large
  non-reproductive, and reproductive.
}
\examples{
data(monkeyflower)
## convert M. cardinalis rows to list of 16 matrices
A <- subset(monkeyflower, species=="cardinalis")
# use as.matrix to convert data.frame to numeric matrix
A<-split( as.matrix(A[, 4:19]),  paste(A$site, A$year))
stages<-c("seed", "sm.nr", "lg.nr", "repro")
## convert to list of 16 matrices
A<-lapply(A, matrix, nrow=4, byrow=TRUE, dimnames=list(stages,stages))
A[8]
image2(A[[8]], round=8, mar=c(1,3,4.5,1))
title( paste("M. cardinalis - ", names(A[8])), line=2.5)


## plot like figure 1A
x<- matrix(sapply(A, lambda), ncol=4)
colnames(x)<-c("BU",  "CA", "RP", "WA")
rownames(x)<-c(2000:2002, "pooled")
x<-x[,c(1,3,4,2)]
colrs<-gray(0:3 / 3)[c(1,3,2,4)]
barplot(x, beside=TRUE, las=1, col=colrs, ylim=c(0,2),
ylab="Population growth rate", main="Mimulus cardinalis")
box()
abline(h=1, lwd=.5)
legend(1,1.95, rownames(x), fill=colrs, bty='n')
}
\references{
  Amy Lauren Angert. 2006. Demography of central and
  marginal populations of monkeyflowers (\emph{Mimulus
  cardinalis} and \emph{M. lewisii}). Ecology 87:2014-2025.
}
\keyword{datasets}

