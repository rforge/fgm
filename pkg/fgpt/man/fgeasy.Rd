\name{fgeasy}
\alias{fgeasy}
\alias{summary.fg}
\alias{plot.fg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single step version of the Floating Grid Permutation Technique
}
\description{
The Floating Grid Permutation Technique is a spatially restricted permutation technique. Please read the reference mentioned below and/or the vignette before using this function. \code{fgeasy} can be used to perform single step permutation tests. This function can be used for detecting spatial autocorrelation for univariate observations (using Moran's I), pairwise observations (using a correlation coefficient) or multivariate observations for both single and pairwise sets of multivariate observations (using mean distance or relatedness).
}
\usage{
fgeasy(xy, group=1, marks, iter=999, ratio=1, scale.seq=seq(from=0, to=max(dist(xy)), 
  length.out=21)[2:21], bootstrap=FALSE, pairwise=FALSE, correlate=FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
Geographical locations of observations.
}
  \item{group}{
Group membership for the observations. \code{group} is optional. 
}
  \item{marks}{
Should be a vector containing all observations, a  two-column matrix  containing two paired observations or a squared matrix containing relationships or distances between all observations (such as genetic relatedness or distance). The latter can be used to perform multivariate analyses. Note that the squared matrix does not need to be symmetrical: for instance, the genetic relatedness between paired individuals can be investigated by using males as columns and females as rows. \code{NA}s are allowed.
}
  \item{iter}{
Number of iterations for every grid cell size. Default is 999, though it is adviseable to perform 9999 interations for the final results.
}
  \item{ratio}{
The ratio between the sides of the grid cells. Default is 1.
}
  \item{scale.seq}{
Sequence of all grid cell sizes that will be tested. The number, order and values of this sequence are completely free, as long as they are numeric. Default is 20 equaly spaced out grid cell sizes from slightly larger than 0 to the maximum distance between any two individuals.
}
  \item{bootstrap}{
\code{TRUE} if observations should to be drawn with replacement, \code{FALSE} if not. Default is \code{FALSE}.
}
  \item{pairwise}{
\code{TRUE} if observations consist of two paired marks per observation, \code{FALSE} if not. Default is \code{FALSE}. If \code{pairwise==FALSE} Moran's I is used as test statistic.
}
  \item{correlate}{
If one wants to compare two paired observations correlations between the observations can be used. The argument \code{correlate} can be used to pass on the peferred method of correlation as available in \code{cor.test}; so \code{"pearson"}, \code{"kendall"} or \code{"spearman"}. If \code{marks} is not a two-column matrix \code{correlate} should be \code{FALSE}. Default is \code{FALSE}.
}
}

\value{
\code{fgeasy} returns an object of \code{class} "fg", which can be accessed with the functions \code{summary} and \code{plot}.
}
\references{
Reinder Radersma & Ben C. Sheldon, 2014. A new permutation technique to explore and control for spatial autocorrelation. Manuscript submitted to MEE
}
\author{
Reinder Radersma
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cor.test}}
}
\examples{
## A simple univariate example
##
## Produce 49 geographical locations in a regular grid.
loc.x <- rep(1:7,7)
loc.y <- rep(1:7, each=7)

## Produce 49 observations with negative spatial autocorrelation.
marks <- c(rep(c(1,2), 24),1)+rnorm(49,0,0.1)

fg1 <- fgeasy(xy=cbind(loc.x,loc.y), marks=marks, iter=99)
summary(fg1)
plot(fg1)

## An example for two paired marks
##
## Produce 20 geographical locations.
loc.x <- 1:20
loc.y <- runif(20,0,5)

## Produce 2 x 20 phenotypes.
type1 <- 11:30+runif(20,0,5)
type2 <- 11:30+runif(20,0,5)

fg2 <- fgeasy(xy=cbind(loc.x,loc.y), marks=cbind(type1,type2), iter=99, 
        pairwise=TRUE, correlate="pearson")
summary(fg2)
plot(fg2)
  }
