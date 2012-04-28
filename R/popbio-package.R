

#'Introduction to the popbio Package
#'
#'\code{Popbio} is a package for the construction and analysis of matrix
#'population models.  First, the package consists of the translation of
#'\code{Matlab} code found in Caswell (2001) or Morris and Doak (2002).  A list
#'of converted functions within each book can be accessed using
#'\code{help(Caswell)} and \code{help(Morris)} within R, or by following the
#'links to \link{02.Caswell} and \link{03.Morris} from the help content pages.
#'
#'Second, the \code{popbio} package includes functions to estimate vital rates
#'and construct projection matrices from raw census data typically collected in
#'plant demography studies. In these studies, vital rates can often be
#'estimated directly from annual censuses of tagged individuals using
#'transition frequency tables.  To estimate vital rates in animal demography
#'using capture-recapture methods, try the \code{Rcapture} or \code{mra}
#'package instead.
#'
#'Finally, the package includes plotting methods and sample datasets consisting
#'of either published projection matrices or annual census data from demography
#'studies.  Three sample demonstrations illustrate some of the package
#'capabilities (\code{Caswell, fillmore and stage.classify}).  A description of
#'the package in the Journal of Statistical Software is available at
#'\url{http://www.jstatsoft.org/v22/i11}.
#'
#'@name Introduction
#'@aliases 01.Introduction popbio
#'@author Chris Stubben
#'@references To cite the popbio package in publications, type
#'\code{citation('popbio')}.  For details on matrix population models, see
#'
#'Caswell, H. 2001. Matrix population models: Construction, analysis, and
#'interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.
#'
#'Morris, W. F., and D. F. Doak. 2002. Quantitative conservation biology:
#'Theory and practice of population viability analysis. Sinauer, Sunderland,
#'Massachusetts, USA.
#'@keywords documentation
NULL





#'Converted Matlab functions from Caswell (2001)
#'

#'
#'\bold{Chapter 2}.  Age-classified matrix models \describe{ \item{ }{ section
#'2.2.  Projection of population growth rates.
#'}\item{list(list("pop.projection"))}{ section 2.2.  Projection of population
#'growth rates.  }\item{ }{ section 2.2.  Projection of population growth
#'rates.  } }
#'
#'\bold{Chapter 4}.  Stage-classified matrix models \describe{ \item{ }{
#'section 4.4. Returns the dominant eigenvalue }\item{list(list("lambda"))}{
#'section 4.4. Returns the dominant eigenvalue }\item{ }{ section 4.4. Returns
#'the dominant eigenvalue } \item{ }{ section 4.5. Returns the stable stage
#'distribution (right eigenvector) }\item{list(list("stable.stage"))}{ section
#'4.5. Returns the stable stage distribution (right eigenvector) }\item{ }{
#'section 4.5. Returns the stable stage distribution (right eigenvector) }
#'\item{ }{ section 4.6. Returns the reproductive value (left eigenvector)
#'}\item{list(list("reproductive.value"))}{ section 4.6. Returns the
#'reproductive value (left eigenvector) }\item{ }{ section 4.6. Returns the
#'reproductive value (left eigenvector) } \item{ }{ section 4.7. Returns the
#'damping ratio }\item{list(list("damping.ratio"))}{ section 4.7. Returns the
#'damping ratio }\item{ }{ section 4.7. Returns the damping ratio } \item{ }{
#'section 4.8.  Computes eigenvalues and vectors, including the dominant
#'eigenvalue , stable stage distribution, reproductive value, damping ratio,
#'sensitivities, and elasticities.  Since version 2.0, these are now included
#'as separate functions as well }\item{list(list("eigen.analysis"))}{ section
#'4.8.  Computes eigenvalues and vectors, including the dominant eigenvalue ,
#'stable stage distribution, reproductive value, damping ratio, sensitivities,
#'and elasticities.  Since version 2.0, these are now included as separate
#'functions as well }\item{ }{ section 4.8.  Computes eigenvalues and vectors,
#'including the dominant eigenvalue , stable stage distribution, reproductive
#'value, damping ratio, sensitivities, and elasticities.  Since version 2.0,
#'these are now included as separate functions as well } }
#'
#'\bold{Chapter 5}.  Events in the Life Cycle \describe{ \item{ }{ section
#'5.3.1.  Calculate age-specific survival from a stage classified matrix using
#'the fundamental matrix N }\item{list(list("fundamental.matrix"))}{ section
#'5.3.1.  Calculate age-specific survival from a stage classified matrix using
#'the fundamental matrix N }\item{ }{ section 5.3.1.  Calculate age-specific
#'survival from a stage classified matrix using the fundamental matrix N }
#'\item{ }{ section 5.3.4.  Calculate the net reproductive rate of a stage
#'classified matrix using the dominant eigenvalue of the matrix R.
#'}\item{list(list("net.reproductive.rate"))}{ section 5.3.4.  Calculate the
#'net reproductive rate of a stage classified matrix using the dominant
#'eigenvalue of the matrix R.  }\item{ }{ section 5.3.4.  Calculate the net
#'reproductive rate of a stage classified matrix using the dominant eigenvalue
#'of the matrix R.  } \item{ }{ section 5.3.5.  Calculate the generation time
#'of a stage-classified matrix }\item{list(list("generation.time"))}{ section
#'5.3.5.  Calculate the generation time of a stage-classified matrix }\item{ }{
#'section 5.3.5.  Calculate the generation time of a stage-classified matrix }
#'} Age-specific survivorship and fertility curves in Fig 5.1 and 5.2 are now
#'included in \code{demo(Caswell)}.
#'
#'\bold{Chapter 6}. Parameter estimation \describe{ \item{ }{ section 6.1.1.
#'Estimate vital rates and construct a projection matrix using transtion
#'frequency tables}\item{list(list("projection.matrix"))}{ section 6.1.1.
#'Estimate vital rates and construct a projection matrix using transtion
#'frequency tables}\item{ }{ section 6.1.1.  Estimate vital rates and construct
#'a projection matrix using transtion frequency tables} \item{ }{ section
#'6.2.2.  Construct a projection matrix from a time series of individuals per
#'stage using Wood's quadratic programming method.  Requires \code{quadprog}
#'library.}\item{list(list("QPmat"))}{ section 6.2.2.  Construct a projection
#'matrix from a time series of individuals per stage using Wood's quadratic
#'programming method.  Requires \code{quadprog} library.}\item{ }{ section
#'6.2.2.  Construct a projection matrix from a time series of individuals per
#'stage using Wood's quadratic programming method.  Requires \code{quadprog}
#'library.} }
#'
#'\bold{Chapter 9}.  Sensitivity analysis \describe{ \item{ }{ section 9.1.
#'Calculate sensitivities. }\item{list(list("sensitivity"))}{ section 9.1.
#'Calculate sensitivities. }\item{ }{ section 9.1. Calculate sensitivities. }
#'\item{ }{ section 9.2. Calculate elasticities.
#'}\item{list(list("elasticity"))}{ section 9.2. Calculate elasticities.
#'}\item{ }{ section 9.2. Calculate elasticities. } } See the \code{secder}
#'function in the \code{demogR} package for second derivatives of eigenvalues
#'described in section 9.7
#'
#'\bold{Chapter 10}.  Life Table Response Experiments \describe{ \item{ }{
#'section 10.1 and 10.2.  Fixed designs in LTREs.  See \code{demo(Caswell)} for
#'variance decomposition in random design (Fig 10.10).
#'}\item{list(list("LTRE"))}{ section 10.1 and 10.2.  Fixed designs in LTREs.
#'See \code{demo(Caswell)} for variance decomposition in random design (Fig
#'10.10). }\item{ }{ section 10.1 and 10.2.  Fixed designs in LTREs.  See
#'\code{demo(Caswell)} for variance decomposition in random design (Fig 10.10).
#'} }
#'
#'\bold{Chapter 12}.  Statistical inference \describe{ \item{ }{ section
#'12.1.4.  Resample observed census transitions in a stage-fate data frame
#'}\item{list(list("boot.transitions"))}{ section 12.1.4.  Resample observed
#'census transitions in a stage-fate data frame }\item{ }{ section 12.1.4.
#'Resample observed census transitions in a stage-fate data frame } \item{ }{
#'section 12.1.5.2.  Resample transitions in a projction matrix from a
#'multinomial distribution (and fertilites from a log
#'normal)}\item{list(list("resample"))}{ section 12.1.5.2.  Resample
#'transitions in a projction matrix from a multinomial distribution (and
#'fertilites from a log normal)}\item{ }{ section 12.1.5.2.  Resample
#'transitions in a projction matrix from a multinomial distribution (and
#'fertilites from a log normal)} }
#'
#'\bold{Chapter 14}.  Environmental stochasticity \describe{ \item{ }{ section
#'14.3.  Calculate the log stochastic growth rate by simulation and
#'Tuljapukar's approximation }\item{list(list("stoch.growth.rate"))}{ section
#'14.3.  Calculate the log stochastic growth rate by simulation and
#'Tuljapukar's approximation }\item{ }{ section 14.3.  Calculate the log
#'stochastic growth rate by simulation and Tuljapukar's approximation } \item{
#'}{ section 14.5.3.  Project stochastic growth from a sequence of matrices in
#'a uniform and nonuniform environment }\item{list(list("stoch.projection"))}{
#'section 14.5.3.  Project stochastic growth from a sequence of matrices in a
#'uniform and nonuniform environment }\item{ }{ section 14.5.3.  Project
#'stochastic growth from a sequence of matrices in a uniform and nonuniform
#'environment } } See the \code{stoch.sens} function in the \code{demogR}
#'package for senstivity and elasticity of log stochastic growth rate described
#'in section 14.4.
#'
#'\bold{Chapter 15}.  Demographic stochasticity \describe{ \item{ }{ section
#'15.1.3.  Incorporate demographic stochasticity into population projections.
#'The example uses the \link{whale} dataset to create a plot like figure 15.3.
#'}\item{list(list("multiresultm"))}{ section 15.1.3.  Incorporate demographic
#'stochasticity into population projections.  The example uses the \link{whale}
#'dataset to create a plot like figure 15.3. }\item{ }{ section 15.1.3.
#'Incorporate demographic stochasticity into population projections.  The
#'example uses the \link{whale} dataset to create a plot like figure 15.3. } }
#'
#'@name Caswell
#'@aliases 02.Caswell Caswell
#'@author Chris Stubben
#'@keywords documentation
NULL





#'Converted Matlab functions from Morris and Doak (2002)
#'
#'\bold{Chapter 3} \describe{ \item{ }{ Table 3.1. Grizzly bear population
#'counts.  The example includes code to calculate mean, variance and confidence
#'intervals using regression and other procedures
#'}\item{list(list("grizzly"))}{ Table 3.1. Grizzly bear population counts.
#'The example includes code to calculate mean, variance and confidence
#'intervals using regression and other procedures }\item{ }{ Table 3.1. Grizzly
#'bear population counts.  The example includes code to calculate mean,
#'variance and confidence intervals using regression and other procedures }
#'\item{ }{ Box 3.3. Count-based extinction time cumulative distribution
#'function }\item{list(list("extCDF"))}{ Box 3.3. Count-based extinction time
#'cumulative distribution function }\item{ }{ Box 3.3. Count-based extinction
#'time cumulative distribution function } \item{ }{ Box 3.4. Count-based
#'extinction probabilities with bootstrap confidence intervals
#'}\item{list(list("countCDFxt"))}{ Box 3.4. Count-based extinction
#'probabilities with bootstrap confidence intervals }\item{ }{ Box 3.4.
#'Count-based extinction probabilities with bootstrap confidence intervals } }
#'
#'\bold{Chapter 7} \describe{ \item{ }{ Box 7.3.  Project stochastic growth
#'from a sequence of matrices }\item{list(list("stoch.projection"))}{ Box 7.3.
#'Project stochastic growth from a sequence of matrices }\item{ }{ Box 7.3.
#'Project stochastic growth from a sequence of matrices } \item{ }{ Box 7.4.
#'Calculate the log stochastic growth rate by Tuljapukar's approximation and by
#'simulation }\item{list(list("stoch.growth.rate"))}{ Box 7.4.  Calculate the
#'log stochastic growth rate by Tuljapukar's approximation and by simulation
#'}\item{ }{ Box 7.4.  Calculate the log stochastic growth rate by Tuljapukar's
#'approximation and by simulation } \item{ }{ Box 7.5.  Estimate
#'quasi-extinction threshold }\item{list(list("stoch.quasi.ext"))}{ Box 7.5.
#'Estimate quasi-extinction threshold }\item{ }{ Box 7.5.  Estimate
#'quasi-extinction threshold } }
#'
#'\bold{Chapter 8} \describe{ \item{ }{ Box 8.2. Kendall's method to correct
#'for sampling variation }\item{list(list("Kendall"))}{ Box 8.2. Kendall's
#'method to correct for sampling variation }\item{ }{ Box 8.2. Kendall's method
#'to correct for sampling variation } \item{ }{ Box 8.3. Generate
#'beta-distributed random numbers }\item{list(list("betaval"))}{ Box 8.3.
#'Generate beta-distributed random numbers }\item{ }{ Box 8.3. Generate
#'beta-distributed random numbers } \item{ }{ Box 8.4.  Generate random
#'lognormal values }\item{list(list("lnorms"))}{ Box 8.4.  Generate random
#'lognormal values }\item{ }{ Box 8.4.  Generate random lognormal values }
#'\item{ }{ Box 8.5.  Generate stretched beta-distributed random numbers
#'}\item{list(list("stretchbetaval"))}{ Box 8.5.  Generate stretched
#'beta-distributed random numbers }\item{ }{ Box 8.5.  Generate stretched
#'beta-distributed random numbers } \item{ }{ Box 8.10.  Calculate stochastic
#'growth rate and extinction time CDF using vital rates
#'}\item{list(list("vitalsim"))}{ Box 8.10.  Calculate stochastic growth rate
#'and extinction time CDF using vital rates }\item{ }{ Box 8.10.  Calculate
#'stochastic growth rate and extinction time CDF using vital rates } \item{ }{
#'Box 8.11.  Incorporate demographic stochasticity into population projections
#'}\item{list(list("multiresultm"))}{ Box 8.11.  Incorporate demographic
#'stochasticity into population projections }\item{ }{ Box 8.11.  Incorporate
#'demographic stochasticity into population projections } }
#'
#'\bold{Chapter 9} \describe{ \item{ }{ Box 9.1. Vital rate sensitivity and
#'elasticity}\item{list(list("vitalsens"))}{ Box 9.1. Vital rate sensitivity
#'and elasticity}\item{ }{ Box 9.1. Vital rate sensitivity and elasticity} }
#'
#'@name Morris
#'@aliases 03.Morris Morris
#'@keywords documentation
NULL





#'Annual census data for Aquilegia in the southwestern US
#'
#'Demography census data from \emph{Aquilegia chrysantha} in Fillmore Canyon,
#'Organ Mountains, New Mexico, 1996-2003.
#'
#'This sample data set includes census data from 10 of the 15 total demography
#'plots established in 1995.  Please contact the data set owners to access the
#'complete census data from 1995-2006.
#'
#'@name aq.census
#'@docType data
#'@format A data frame with 2853 observations on the following 8 variables.
#'\describe{ \item{list("plot")}{Plot number} \item{list("year")}{Year of
#'census} \item{list("plant")}{Plant id number} \item{list("status")}{Plant
#'status recorded in field: dead, dormant, recruit0 (with cotyledons only),
#'recruit1, flowering or vegetative. } \item{list("rose")}{Total number of
#'rosettes} \item{list("leaf")}{Total number of leaves}
#'\item{list("infl")}{Total number of infloresences or flowering stalks}
#'\item{list("fruits")}{Total number of mature fruits} }
#'@seealso \code{\link{aq.trans}} for annual transitions with stage and fate in
#'same row
#'@source Data set owners: Brook Milligan, Chris Stubben, Allan Strand
#'@keywords datasets
#'@examples
#'
#'data(aq.census)
#'sv<-table(aq.census$status, aq.census$year)
#'sv
#'stage.vector.plot(sv[-1,], prop=FALSE)
#'
NULL





#'Annual transition data for Aquilegia in the southwestern US
#'
#'Transition data listing stages and fates from \emph{Aquilegia chrysantha} in
#'Fillmore Canyon, Organ Mountains, New Mexico, 1996-2003.
#'
#'The five stage classes include seeds in the seed bank, new recruits or
#'seedlings, small vegetative plants with 1 rosette, large vegetative plants
#'with 2 or more rosettes, and flowering plants.  Stage classes were assigned
#'to census plants using a combination of status and size data recorded in the
#'field.  See \code{demo(stage.classify)} for more details.
#'
#'@name aq.trans
#'@docType data
#'@format A data frame with 1637 observations on the following 9 variables.
#'\describe{ \item{list("plot")}{Plot number} \item{list("year")}{Staring year
#'of census} \item{list("plant")}{Plant id number} \item{list("stage")}{Initial
#'stage class with ordered factor levels \code{seed} < \code{recruit} <
#'\code{small} < \code{large} < \code{flower}.  } \item{list("leaf")}{Total
#'number of leaves} \item{list("rose")}{Total number of rosettes}
#'\item{list("fruits")}{Total number of mature fruits}
#'\item{list("fate")}{Final stage class or fate with levels \code{seed} <
#'\code{recruit} < \code{small} < \code{large} < \code{flower} < \code{dead}}
#'\item{list("rose2")}{Final number of rosettes} }
#'@seealso \code{\link{aq.census}}
#'@source Data set owners: Brook Milligan, Chris Stubben, Allan Strand
#'@keywords datasets
#'@examples
#'
#'data(aq.trans)
#'head(aq.trans,3)
#'
#'sv<-table(aq.trans$stage, aq.trans$year)
#'addmargins(sv)
#'stage.vector.plot(sv[-1,], prop=FALSE, main="Aquilegia stage vectors")
#'
#'## plot proportions with barplot
#'## use xpd to draw legend outside plot boundaries
#'op<-par(mar=c(5,4,4,1), xpd=TRUE)
#'x<-barplot(prop.table(sv[-1,],2), las=1,
#'xlab="Year", ylab="Proportion in stage class", 
#'col=rainbow(4), ylim=c(0,1), xaxt='n', space=.5)
#'yrs<-substr(colnames(sv),3,4)
#'axis(1,x, yrs)
#'legend(2.7,1.25, rev(rownames(sv)[-1]), fill=rev(rainbow(4)), bty='n', ncol=2)
#'par(op)
#'
#'
#'
NULL





#'Projection matrices for a tropical understory herb
#'
#'Projection matrices for a tropical understory herb (\emph{Calathea
#'ovandensis}) for plots 1-4 and years 1982-1985 and the pooled matrix
#'
#'A projection matrix constructed using a post-breeding census with 8 size
#'classes: seed, seedling, juvenile, pre-reproductive, and 4 reproductive
#'classes divided by leaf area.
#'
#'@name calathea
#'@docType data
#'@format A list of 17 matrices ordered by plot then year, with the pooled
#'matrix last.
#'@references Horvitz, C.C. and D.W. Schemske. 1995.  Spatiotemporal variation
#'in demographic transitions of a tropical understory herb: Projection matrix
#'analysis.  Ecological Monographs 65:155-192.
#'@source Table 7 in Horvitz and Schemske (1995).  The pooled matrix is from
#'Table 8.
#'@keywords datasets
#'@examples
#'
#'data(calathea)
#'## Single matrix
#'calathea[[11]]
#'image2(calathea[[11]], text.cex=.8)
#'title( paste("Calathea", names(calathea[11])), line=3)
#'
#'## MEAN matrix (exclude pooled matrix)
#'mean(calathea[-17])
#'
#'## all plot 1
#'calathea[1:4]
#'## all 1982 matrices 
#'calathea[ grep("1982", names(calathea)) ]
#'# OR
#'# calathea[seq(1,16,4)]
#'# split(calathea, 1:4)[[1]]
#'
#'## Growth rates -see Figure 7
#'x<-sapply(calathea[-17], lambda)
#'x<-matrix(x, nrow=4, byrow=TRUE, dimnames= list(paste("plot", 1:4), 1982:1985))
#'x
#'matplot2(x, type='b', ylab='Growth rate', main='Calathea growth rates')
#'
#'
#'
NULL





#'Population sizes of grizzly bears in Yellowstone from 1959-1997
#'
#'Estimated number of adult female grizzly bears in the Greater Yellowstone
#'population from 1959-1997.
#'
#'The grizzly bear data set is used in count based PVAs in chapter 3 in Morris
#'and Doak 2002.
#'
#'@name grizzly
#'@docType data
#'@format A data frame with 39 observations on the following 2 variables.
#'\describe{ \item{list("year")}{ Year of census} \item{list("N")}{ Estimated
#'number of female grizzlies} }
#'@references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#'biology: Theory and practice of population viability analysis. Sinauer,
#'Sunderland, Massachusetts, USA.
#'@source Table 3.1 in Morris and Doak 2002. Original data from Eberhardt et
#'al. 1986 and Haroldson 1999. Additional details on the Interagency Grizzly
#'Bear Study Team is available at
#'\url{http://nrmsc.usgs.gov/research/igbst-home.htm}.
#'@keywords datasets
#'@examples
#'
#'data(grizzly)
#'attach(grizzly)
#'## plot like Fig 3.6 (p. 66)
#'plot(year, N, type='o', pch=16, las=1, xlab="Year", 
#'ylab="Adult females", main="Yellowstone grizzly bears")
#'## calcualte  log(Nt+1/Nt)
#'nt<-length(N)  ## number transitions
#'logN<-log(N[-1]/N[-nt])
#'## Mean and var
#'c(mean=mean(logN), var=var(logN))
#'## or using linear regression
#'## transformation for unequal variances (p. 68)
#'x<-sqrt(year[-1]-year[-length(year)])
#'y<-logN/x
#'mod<-lm(y~0 + x )
#'## plot like Fig 3.7
#'plot(x,y, xlim=c(0,1.2), ylim=c(-.3,.3), pch=16, las=1,
#'xlab=expression((t[t+1]-t[i])^{1/2}),
#'ylab=expression(log(N[t+1]/N[t]) / (t[t+1]-t[i])^{1/2}) ,
#'main=expression(paste("Estimating ", mu, " and ", sigma^2, " using regression")))
#'abline(mod)
#'## MEAN (slope)
#'mu<- coef(mod)
#'## VAR (mean square in analysis of variance table)
#'sig2<-anova(mod)[["Mean Sq"]][2] 
#'c(mean= mu , var= sig2)
#'## Confidence interval for mean  (page 72)
#'confint(mod,1)
#'## Confidence interval for sigma 2  (equation 3.13)
#'df1<-length(logN)-1
#'df1*sig2 /qchisq(c(.975, .025), df= df1)
#'## test for outliers using dffits (p.74)
#'dffits(mod)[dffits(mod)> 2*sqrt(1/38) ]
#'## plot like  fig 3.11
#'plot(N[-nt], logN, pch=16, xlim=c(20,100), ylim=c(-.3, .3),las=1,
#'xlab="Number of females in year T",
#'ylab=expression(log(N[t+1]/N[t])),
#'main="Grizzly log population growth rates")
#'cor(N[-nt], logN) 
#'abline(lm(logN ~ N[-nt]), lty=3 )
#'detach(grizzly)
#'
#'
NULL





#'Correlation matrices for Hudsonia vital rates
#'
#'Within year and between year correlation matrices from \emph{Hudsonia
#'montana} vital rates.  Correlations were calculated from first 13 growth and
#'survival rates only, since fertility rates vary little.
#'
#'
#'@name hudcorrs
#'@docType data
#'@format A list with 2 correlation matrices, corrin (within year correlation)
#'and corrout (between year correlation).
#'@author Original dataset from Morris and Doak (2002)
#'@seealso \code{\link{vitalsim}}
#'@references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#'biology: Theory and practice of population viability analysis. Sinauer,
#'Sunderland, Massachusetts, USA.
#'@source The correlation matrices in
#'\url{http://www.sinauer.com/PVA/hudcorrs.mat} include some correlations>1.  A
#'corrected set of correlations was sent by the D. Doak on 8/4/2007.
#'@keywords datasets
#'@examples
#'
#'data(hudcorrs)
#'hudcorrs$corrin
#'
NULL





#'Projection matrices for mountain golden heather
#'
#'Projection matrices for the mountain golden heather (\emph{Hudsonia montana})
#'for the years 1985 through 1988
#'
#'A projection matrix with 6 size classes: seeds, seedlings, and 4 size classes
#'divided by plant area.
#'
#'@name hudsonia
#'@docType data
#'@format A list of 4 matrices from 1985-1988
#'@source Table 6.7 in Morris and Doak (2002).  The original data is from Frost
#'(1990).
#'
#'Morris, W. F., and D. F. Doak. 2002. Quantitative conservation biology:
#'Theory and practice of population viability analysis. Sinauer, Sunderland,
#'Massachusetts, USA.
#'@keywords datasets
#'@examples
#'
#'data(hudsonia)
#'sapply(hudsonia, lambda)
#'
#'## mean matrix 
#'x<-mean(hudsonia)
#'image2(x, mar=c(1,4,5.5,1))
#'title("Hudsonia mean matrix", line=2.5)
#'lambda(x)
#'# variance
#'var2(hudsonia)
#'
NULL





#'Best Kendall estimates of Hudsonia vital rate means and variances
#'
#'Best Kendall estimates of vital rate means (9 growth, 4 survival, and 11
#'fertility rates) for \emph{Hudsonia montana}.
#'
#'
#'@name hudvrs
#'@docType data
#'@format A data frame with 24 observations on the following 2 variables.
#'\describe{ \item{list("mean")}{ vital rate means} \item{list("var")}{ vital
#'rate variances} }
#'@references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#'biology: Theory and practice of population viability analysis. Sinauer,
#'Sunderland, Massachusetts, USA.
#'@source Data listed in Box 8.10 for the \code{\link{vitalsim}} function.  See
#'also Table 8.5 in Morris and Doak (2002).
#'@keywords datasets
#'@examples
#'
#'data(hudvrs)
#'hudvrs
#'
#'hudmxdef(hudvrs$mean)
#'
NULL





#'Projection matrices for monkeyflower
#'
#'Pooled and annual projection matrices of central and marginal populations of
#'monkeyflowers (\emph{Mimulus cardinalis} and \emph{M. lewisii})
#'
#'Matrix constructed using a post-breeding census with four stage classes:
#'Seeds, small non-reproductive, large non-reproductive, and reproductive.
#'
#'@name monkeyflower
#'@docType data
#'@format A data frame with 32 projection matrices, arranged with one matrix
#'per row \describe{ \item{list("species")}{M. cardinalis or M. lewisii}
#'\item{list("site")}{Study site} \item{list("year")}{Start year of projection
#'interval or pooled for all three years} \item{list("a11")}{matrix element
#'a11; seed to seed transition or seed bank survival} \item{list("a12")}{matrix
#'element a12; small nr to seed - fertility} \item{list("a13")}{matrix element
#'a13; large nr to seed - fertility} \item{list("a14")}{matrix element a14;
#'reprod to seed - fertility} \item{list("a21")}{matrix element a21; seed to
#'small nr - growth} \item{list("a22")}{matrix element a22; small nr to small
#'nr -stasis} \item{list("a23")}{matrix element a23; large nr to small nr -
#'regress} \item{list("a24")}{matrix element a24; reprod to small nr - regress}
#'\item{list("a31")}{matrix element a31; seed to large nr - growth }
#'\item{list("a32")}{matrix element a32; small nr to large nr - growth }
#'\item{list("a33")}{matrix element a33; large nr to large nr - stasis }
#'\item{list("a34")}{matrix element a34; reprod to large nr - regress }
#'\item{list("a41")}{matrix element a41; seed to reprod - growth }
#'\item{list("a42")}{matrix element a42; small nr to reprod - growth }
#'\item{list("a43")}{matrix element a43; large nr to reprod - growth }
#'\item{list("a44")}{matrix element a44; reprod to reprod - stasis } }
#'@references Amy Lauren Angert. 2006. Demography of central and marginal
#'populations of monkeyflowers (\emph{Mimulus cardinalis} and \emph{M.
#'lewisii}). Ecology 87:2014-2025.
#'@source \url{http://www.esapubs.org/archive/ecol/E087/126/appendix-E.htm}
#'@keywords datasets
#'@examples
#'
#'data(monkeyflower)
#'## convert M. cardinalis rows to list of 16 matrices
#'A <- subset(monkeyflower, species=="cardinalis")
#'# use as.matrix to convert data.frame to numeric matrix
#'A<-split( as.matrix(A[, 4:19]),  paste(A$site, A$year))
#'stages<-c("seed", "sm.nr", "lg.nr", "repro")
#'## convert to list of 16 matrices
#'A<-lapply(A, matrix, nrow=4, byrow=TRUE, dimnames=list(stages,stages))
#'A[8]
#'image2(A[[8]], round=8, mar=c(1,3,4.5,1))
#'title( paste("M. cardinalis - ", names(A[8])), line=2.5)
#'
#'
#'## plot like figure 1A
#'x<- matrix(sapply(A, lambda), ncol=4)
#'colnames(x)<-c("BU",  "CA", "RP", "WA")
#'rownames(x)<-c(2000:2002, "pooled")
#'x<-x[,c(1,3,4,2)]
#'colrs<-gray(0:3 / 3)[c(1,3,2,4)]
#'barplot(x, beside=TRUE, las=1, col=colrs, ylim=c(0,2),
#'ylab="Population growth rate", main="Mimulus cardinalis")
#'box()
#'abline(h=1, lwd=.5)
#'legend(1,1.95, rownames(x), fill=colrs, bty='n')
#'
#'
NULL





#'Population densities for the sugarbeet cyst nematode
#'
#'A time-series of population vectors for the sugarbeet cyst nematode
#'\emph{Heterodera schachtii}. Individuals were classified into three stages
#'(J2, J3+J4, and adult) and densities (per 60 cc of soil) were averaged over
#'four replicates, measured every two days, for 10 days. .
#'
#'
#'@name nematode
#'@docType data
#'@format A matrix listing densities from 3 stage classes over 6 time periods
#'@seealso \code{\link{QPmat}}
#'@references Caswell, H. 2001. Matrix population models. Construction,
#'Analysis and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts.
#'@source Used in Example 6.3 in Caswell (2001).
#'@keywords datasets
#'@examples
#'
#'data(nematode)
#'stage.vector.plot(nematode, prop=FALSE, log='y', ylim=c(.3,200), 
#' xlab="Time", ylab="Nematode density")
#'
NULL





#'Projection matrix for teasel
#'
#'Projection matrix for the plant teasel
#'
#'
#'@name teasel
#'@docType data
#'@format A projection matrix
#'@references Caswell, H. 2001. Matrix population models: construction,
#'analysis, and interpretation, Second edition. Sinauer, Sunderland,
#'Massachusetts, USA.
#'@source Example 5.2
#'@keywords datasets
#'@examples
#'
#'data(teasel)
#'image2(teasel, mar=c(1,3.5,5,1) , box.offset=.1)
#' title("Teasel projection matrix", line=2.5)
#'# fertilities for a monocarpic plant in a prebreeding census in last column
#'splitA(teasel,  r=1:6, c=6)
#'lambda(teasel)
#'
#'
#'
#'
NULL





#'Census data for hypothetical plant
#'
#'Three years of census data for a hypothetical plant with three stage classes.
#'
#'
#'@name test.census
#'@docType data
#'@format A data frame with 41 census observations on the following variables
#'\describe{ \item{list("plant")}{Plant id number} \item{list("year")}{Year of
#'census} \item{list("stage")}{Stage class: seedling, vegetative, or
#'reproductive} \item{list("fruits")}{Total number of fruits} }
#'@keywords datasets
#'@examples
#'
#'data(test.census)
#'stages <- c("seedling", "vegetative", "reproductive")
#'
#'## Cross-tabulate stage vectors and order rows by stage
#'sv<- table(test.census$stage, test.census$year)[stages,]
#'sv
#'stage.vector.plot(sv)
#'
#'## set xaxt='n' to avoid fractions of a year (2002.5)
#'stage.vector.plot(sv, prop=FALSE, xaxt="n", las=1)
#' axis(1, 2001:2003, c(2001, 2002, 2003))
#'
#'
#'## Convert census data to state-fate transition table using reshape
#'reshape(test.census, direction="wide", idvar="plant", timevar="year")
#'
#'## Convert census data  to state-fate transition table using merge
#'trans <- subset(merge(test.census, test.census, by="plant", sort=FALSE), 
#'               year.x==year.y-1)
#'trans
#'
#'## Format column and row names
#'trans<-trans[,c(1:4,6)]
#'colnames(trans)[2:5] <- c("year", "stage", "fruits", "fate")
#'rownames(trans) <- 1:nrow(trans)
#'## Order stage and fate columns
#'trans$stage <- ordered(trans$stage, levels = stages)
#'trans$fate  <- ordered(trans$fate,  levels = c(stages,"dead"))
#'
#'## Select transitions for 2001-2002 and count offspring (seedlings)
#'trans01 <- subset(trans, year==2001)
#'seedlings<-nrow(subset(test.census, year==2002 & stage=="seedling"))
#'
#'## Add individual fertilities using "anonymous reproduction"  based on the 
#'## proportional reproductive outputs of flowering plants and the total number 
#'## of seedlings at the end of the projection interval
#'trans01$seedling<-trans01$fruits/sum(trans01$fruits) * seedlings
#'trans01
#'
#'##  Create transition frequency table  and build T matrix
#'tf<-table( trans01$fate, trans01$stage )
#'tf
#'## remove "dead" fate from matrix
#'## T.mat<-prop.table(tf,2)[-4,]
#'T.mat<-prop.table(tf,2)[stages,]
#'T.mat
#'
#'## Summarize stage-specific fertility rates and build F matrix
#'fert<-tapply(trans01$seedling, trans01$stage, mean)
#'fert
#'F.mat<-T.mat*0
#'F.mat[1,]<- fert
#'F.mat
#'
#'## The final projection matrix is just
#'T.mat+F.mat
#'
#'## OR use projection matrix function - 
#'projection.matrix(trans01)
#'
NULL





#'Projection matrices for desert tortoise
#'
#'Projection matrices for the desert tortoise \emph{Gopherus agassizii}
#'
#'
#'@name tortoise
#'@docType data
#'@format A list of 4 projeciton matrices with 4 different fertility estimates
#'(low, medium low, medium high, and high)
#'@references Caswell, H. 2001. Matrix population models: construction,
#'analysis, and interpretation, Second edition. Sinauer, Sunderland,
#'Massachusetts, USA.
#'
#'Doak, D., P. Kareiva, and B. Kleptetka. 1994. Modeling population viability
#'for the desert tortoise in the Western Mojave Desert. Ecological Applications
#'4:446-460.
#'@source Table 5 in Doak et al (1994).  Used by Caswell (2001) in chapter 9 on
#'sensitivity analysis.
#'@keywords datasets
#'@examples
#'
#'data(tortoise)
#'A<-tortoise[["med.high"]]
#'# log color scale not needed
#'image2(A, mar=c(1,3.5, 5,1), log=FALSE, box.off=.1)
#'title("Tortoise projection matrix", line=3)
#'
#'splitA(A)
#'lambda(A)
#'sapply(tortoise, lambda)
#'
#'
#'
#'
NULL





#'Projection matrix for killer whale
#'
#'Projection matrix for killer whales
#'
#'
#'@name whale
#'@docType data
#'@format A projection matrix.
#'@references Caswell, H. 2001. Matrix population models: construction,
#'analysis, and interpretation, Second edition. Sinauer, Sunderland,
#'Massachusetts, USA.
#'@source Projection matrix from Example 5.1 in Caswell (2001)
#'@keywords datasets
#'@examples
#'
#'data(whale)
#'whale
#'splitA(whale)
#'lambda(whale)
#'sensitivity(whale)
#'# plot sensitivity 
#'matplot2(sensitivity(whale), type='b', legend='topleft', ltitle='Fate',
#'main='Killer Whale sensitivity')
#'
#'
#'
NULL





#'Survirvorship data for adult and juvenile Acorn Woodpeckers
#'
#'Number of juvenile and adult Acorn Woodpeckers and survival in the Water
#'Canyon, New Mexico population, reconstructed from Stacey and Taper (1992).
#'
#'
#'@name woodpecker
#'@docType data
#'@format A data frame with 18 observations on the following 4 variables.
#'\describe{ \item{list("rate")}{ Adult or juvenile stage} \item{list("year")}{
#'Year} \item{list("start")}{ Total number of starting individuals}
#'\item{list("surv")}{ Number surviving to spring} }
#'@seealso \code{\link{Kendall}} and \code{\link{varEst}}
#'@references Akcakaya, H. R. 2002. Estimating the variance of survival rates
#'and fecundities. Animal Conservation 5: 333-336.
#'
#'Kendall, B. E. 1998. Estimating the magnitude of environmental stochasticity
#'in survivorship data. Ecological Applications 8(1): 184-193.
#'@source Stacey, P.B., and M. Taper. 1992. Environmentl variation and the
#'persistence of small populations. Ecological Applications 2: 18-29.
#'@keywords datasets
#'@examples
#'
#'data(woodpecker)
#'woodpecker
#'with(subset(woodpecker, rate=='adult'), 
#' plot(year, start, type='o', pch=16, 
#' ylab="Number of adults", xlab="Year",
#' main="Acorn Woodpeckers in Water Canyon"))
#'##stage-specific survival rate
#'x<-aggregate(list(Nstart=woodpecker$start, Nsurv=woodpecker$surv),
#'list(stage=woodpecker$rate), sum)
#'x$survival<-x[,3]/x[,2]
#'x
#'
NULL



