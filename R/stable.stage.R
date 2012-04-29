#'Stable stage distribution
#'
#'Calculates the stable stage distribution of a projection matrix
#'
#'see section 4.5 in Caswell (2001).
#'
#'@param A A projection matrix
#'@return A vector containing the stable stage distribution
#'@author Chris Stubben
#'@references Caswell, H. 2001. Matrix population models: construction,
#'analysis, and interpretation, Second edition. Sinauer, Sunderland,
#'Massachusetts, USA.
#'

#'@keywords survey
#'@examples
#'
#'
#'data(teasel)
#'w<-stable.stage(teasel)
#'w
#'barplot( w,  col="green", ylim=c(0,1), las=1,
#'       ylab="Stable stage proportion", xlab="Stage class", main="Teasel")
#'box()
#'
#'@export
stable.stage<-function(A)
{
   ev <- eigen(A)
   lmax <- which.max(Re(ev$values))
   W <- ev$vectors
   w <- abs(Re(W[, lmax]))
   names(w) <- colnames(A)
   w/sum(w)
}
