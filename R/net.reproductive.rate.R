#'Net reproductive rate
#'
#'Calculates the net reproductive rate of a stage classified matrix using the
#'dominant eigenvalue of the matrix R.
#'
#'see section 5.3.4 in Caswell (2001).
#'
#'@param A projection matrix
#'@param \dots additional items are passed to \code{\link{splitA}} and are used
#'to split A into T and F matrices
#'@return Net reproductive rate.  If the transition matrix is singular, then NA
#'is returned.
#'@note Previous versions required separate T and F matrices as input
#'@author Chris Stubben
#'@seealso see \code{\link{fundamental.matrix}} and
#'\code{\link{generation.time}} for other age-specific traits
#'@references Caswell, H. 2001. Matrix population models: construction,
#'analysis, and interpretation, Second edition. Sinauer, Sunderland,
#'Massachusetts, USA.
#'@keywords survey
#'@examples
#'
#'data(whale)
#'net.reproductive.rate(whale)
#'## fertilities in last column
#'data(teasel)
#'net.reproductive.rate(teasel, r=1:6, c=6)
#'## Plot 3 from Calathea - values are not the same as p. 105 in Caswell.
#'data(calathea)
#'sapply(calathea[9:12], net.reproductive.rate)
#'
#'@export
net.reproductive.rate<-function(A, ...)
{
   if(!is.matrix(A)){stop("A projection matrix is required")}
   A1<-splitA(A, ...)
   Tmat<-A1[[1]]
   Fmat<-A1[[2]]
   s <- length(diag(Tmat))
   N <- try(solve(diag(s) - Tmat), silent=TRUE)
   if(class(N)=="try-error"){r<-NA}
   else{
     R <- Fmat %*% N
     r<-lambda(R)
   }
   r
}
