#'Calculate a variance matrix
#'
#'Calculates the variances from a list of matrices
#'
#'Returns a matrix containing variances from a list of matrices using a
#'combination of \code{\link{unlist}} and \code{\link{apply}}.
#'
#'@param x A list of two or more matrices
#'@return A matrix containing variances
#'@author Chris Stubben
#'@keywords survey
#'@examples
#'
#'data(hudsonia)
#'var2(hudsonia)
#'
#'@export
var2 <- function(x)
{
    if(!all(sapply(x, is.matrix)))
        stop("'x' must be a list containing matrices")
   dims <- sapply(x, dim)
   n <- dims[1, 1]
   p <- dims[2, 1]
   if(!all(n == dims[1, ]) || !all(p == dims[2, ]))
        stop("the matrices must have the same dimensions")
   mat <- matrix(unlist(x), n * p, length(x))
   mm<-matrix(apply(mat, 1, var), n, p)
   dimnames(mm)<-dimnames(x[[1]])
   mm
}

