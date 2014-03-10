\name{fgeasy}
\alias{fgeasy}
\alias{summary.fg}
\alias{plot.fg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single step version of the Floating Grid Permutation Technique
}
\description{
The Floating Grid Permutation Technique is a spatially restricted permutation technique. Please read the reference mentioned below and/or the vignette before using this function. \code{fgeasy} can be used to perform single step permutation tests.
}
\usage{
fgeasy(xy, group=1, marks, iter=999, ratio=1, scale.seq=seq(from=0, to=max(dist(xy)), length.out=21)[2:21], bootstrap=FALSE, correlate=FALSE)
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
Should be a vector containing all observations, a  two-column matrix  containing two paired observations or a squared matrix containing relationships or distances between all observations (such as genetic relatedness or distance).The latter can be used to perform multivariate analyses. Note that the squared matrix does not need to be symmetrical: for instance, the genetic relatedness between paired individuals can be investigated by using males as columns and females as rows. \code{NA}s are allowed.
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
  \item{correlate}{
If one wants to compare two paired observations correlations between the observations can be used. The argument \code{correlate} can be used to pass on the peferred method of correlation as available in \code{cor.test}; so \code{"pearson"}, \code{"kendall"} or \code{"spearman"}. If \code{marks} is not a two-column matrix \code{correlate} should be \code{FALSE}. Default is \code{FALSE}.
}
}
\details{

}
\value{
\code{fgeasy} returns an object of \code{class} "fg", which can be accessed with the functions \code{summary} and \code{plot}.
}
\references{
Reinder Radersma & Ben C. Sheldon, 2014. A new permutation test to explore and control for spatial autocorrelation in point pattern data. Manuscript submitted to MEE
}
\author{
Reinder Radersma
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cor.test}}
}
\examples{

## Produce 20 geographical locations.
loc.x <- 1:20
loc.y <- runif(20,0,5)

## Produce 2 x 20 phenotypes.
type1 <- 11:30+runif(20,0,5)
type2 <- 11:30+runif(20,0,5)

fg <- fgeasy(xy=cbind(loc.x,loc.y), marks=cbind(type1,type2), iter=99, correlate="pearson")
summary(fg)
plot(fg)
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
