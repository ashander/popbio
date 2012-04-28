#'Generate random lognormal values for fertility rates
#'
#'Converts standard normal random values to lognormals with defined means and
#'variances
#'
#'
#'@param n number of observations
#'@param mean mean value of the fertility rate
#'@param var variance of the vital rate (not standard deviation)
#'@return A vector of random lognormal values.
#'@note This function could probably be replaced with built-in functions for
#'the Log Normal Distribution \code{\link{rlnorm} }
#'@author Original Matlab code by Morris and Doak (2002: 281). Adapted to R by
#'Patrick Nantel, 20 June 2005.
#'@seealso \code{\link{stretchbetaval} }
#'@references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#'biology: Theory and practice of population viability analysis. Sinauer,
#'Sunderland, Massachusetts, USA.
#'@source converted Matlab code from Box 8.4 in Morris and Doak (2002)
#'@keywords survey
#'@examples
#'
#'lnorms(1)
#'
#' # Generate lognormal random fertilities
#' # for a population of 1000 mature individuals with mean fertility of
#' # 3 and inter-individual variance in fertility of 1.5.
#' 
#' rndfert  <- lnorms(1000, 3,1.5)
#'summary(rndfert)
#' hist(rndfert,40, main="Lognormal random fertilities", 
#'xlab="Fertility rate", col="blue")
#'
#'
#' 
lnorms <- function(n, mean=2,var=1)
{
    nmeans  <- log(mean) - 0.5 * log(var/mean^2 + 1)
    nvars   <- log(var/mean^2 + 1)
    normals <- rnorm(n) * sqrt(nvars) + nmeans
    lns     <- exp(normals)
    lns   
} 

