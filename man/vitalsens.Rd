\name{vitalsens}
\alias{vitalsens}
\title{Vital rate sensitivities and elasticities}
\usage{
  vitalsens(elements, vitalrates)
}
\arguments{
  \item{elements}{An object of mode
  \code{\link{expression}} with all matrix elements
  represented by zeros or symbolic vital rates.}

  \item{vitalrates}{A list of vital rates with
  \code{\link{names}} matching expressions in elements
  above.}
}
\value{
  A dataframe with vital rate estimates, sensitivities, and
  elasticities.
}
\description{
  Calculates deterministic sensitivities and elasticities
  of lambda to lower-level vital rates using partial
  derivatives
}
\details{
  Vital rate sensitivities and elasticities are discussed
  in example 9.3 and 9.6 in Caswell (2001). Also see
  Chapter 9 and Box 9.1 for Matlab code in Morris and Doak
  (2002).
}
\note{
  The element expressions should return the actual matrix
  element estimates after evaluating the variables using
  \code{\link{eval}} below.

  \code{A<-sapply(elements, eval, vitalrates, NULL)}

  In addition, these expressions should be arranged by rows
  so the following returns the projection matrix.

  \code{matrix(A, nrow=sqrt(length(elements)), byrow=TRUE)}
}
\examples{
## emperor goose in Morris and Doak 2002.

goose.vr<-list( Ss0=0.1357,  Ss1=0.8926,  Sf2=0.6388,  Sf3= 0.8943)
goose.el<-expression(
0,  0,  Sf2*Ss1,Sf3*Ss1,
Ss0,0,  0,      0,
0,  Ss1,0,      0,
0,  0,  Ss1,    Ss1)


## first plot effects of changing vital rates -- Figure 9.1
n<-length(goose.vr)
vr<-seq(0,1,.1)
vrsen<-matrix(numeric(n*length(vr)), ncol=n, dimnames=list(vr, names(goose.vr)))
for (h in 1:n)
{
  goose.vr2<-list(  Ss0=0.1357,  Ss1=0.8926,  Sf2=0.6388,  Sf3= 0.8943)
  for (i in 1:length(vr))
  {
     goose.vr2[[h]]<-vr[i]
     A<-matrix(sapply(goose.el, eval,goose.vr2 , NULL), nrow=sqrt(length(goose.el)), byrow=TRUE)
     vrsen[i,h] <- max(Re(eigen(A)$values))
  }
}
matplot(rownames(vrsen), vrsen, type='l', lwd=2, las=1,
ylab="Goose population growth", xlab="Value of vital rate",
main="Effects of changing goose vital rates")
vrn<-expression(s[0], s["">=1], f[2], f["">=3])
legend(.8, .4, vrn, lty=1:4, lwd=2, col=1:4, cex=1.2)

## then calculate sensitivities  -- Table 9.1

x<-vitalsens(goose.el, goose.vr)
x
sum(x$elasticity)


barplot(t(x[,2:3]), beside=TRUE, legend=TRUE, las=1, xlab="Vital rate",
main="Goose vital rate sensitivity and elasticity")
abline(h=0)


## Table 7 endangered lesser kestral in Hiraldo et al 1996
kest.vr<- list(b = 0.9321, co = 0.3847, ca = 0.925, so = 0.3409, sa = 0.7107)
kest.el <- expression( co*b*so, ca*b*so, sa, sa)
x<-vitalsens(kest.el, kest.vr)
x
sum(x$elasticity)

barplot(t(x[,2:3]), beside=TRUE, las=1, xlab="Vital rate",
main="Kestral vital rate sensitivity and elasticity")
legend(1,1, rownames(t(x[,2:3])), fill=grey.colors(2))
abline(h=0)
}
\author{
  Chris Stubben.  Based on code posted by Simon Blomberg to
  R-help mailing list.
}
\references{
  Caswell, H. 2001. Matrix population models. Construction,
  Analysis and interpretation. 2nd ed. Sinauer, Sunderland,
  Massachusetts.

  Morris, W. F., and D. F. Doak. 2002. Quantitative
  conservation biology: Theory and practice of population
  viability analysis. Sinauer, Sunderland, Massachusetts.
}
\keyword{survey}

