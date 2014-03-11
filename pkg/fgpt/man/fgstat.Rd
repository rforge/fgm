\name{fgstat}
\alias{fgstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate statistics for FGPT
}
\description{
Calculates a set of values for a particular statistic or sets of observations, typically for observed values and multiple sets of randomized observations.
}
\usage{
fgstat(rand,marks,FUN=mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rand}{
A list for which the elements are either sets of randomized variables or randomized index values. \code{rand} can, but not necessarily is, the output of the function \code{fgperm}.
}
  \item{marks}{
Should either be left empty, be a vector or a matrix. When \code{rand} contains randomized variables \code{marks} should be left empty. If the randomization procedure is for testing one variable, \code{marks} should be a vector for which the row numbers correspond to the index values used in \code{rand}. If the randomization procedure is for testing two variables (for instance the distance between them), \code{marks} should be a matrix for which the row numbers correspond to the index values used in \code{rand}. 
}
  \item{FUN}{
Any function used to calculate the statistic of interest (e.g. \code{mean}, \code{median}, \code{var}, \code{sd}). Default for \code{FUN} is \code{mean}. 
}
  \item{...}{
Optional arguments to \code{FUN}. A particular useful one if dealing with missing values and using one of the functions from \code{base} is \code{na.rm=TRUE}.
}
}
\details{
\code{fgstat} is designed to calculate statistics for spatial explicit data for which randomized data sets are generated with \code{fgperm}. 
}
\value{
\code{fgstat} returns a vector of statistics. If \code{rand} is the output of \code{fgperm} and \code{add.obs=TRUE}, the first value is the statistic for the observed data and the rest for randomizations.
}
\references{
Reinder Radersma & Ben C. Sheldon, 2014. A new permutation test for dealing with and exploring spatial autocorrelation submitted to MEE
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
}
\examples{

#### Example for fgrand

## 200 random geographical locations
xy <- array(runif(400,0,2), dim=c(200,2))

## run fgperm to produce 99 randomizations for scale 1
test <- fgperm(xy, scale=1, iter=99, add.obs=TRUE) 

## run fgperm to produce 99 bootstraps for scale 1
test <- fgperm(xy, scale=1, iter=99, FUN=function(x){x[sample.int(length(x),replace=TRUE)]}, add.obs=TRUE) 

## 200 times 200 random distances (e.g. genetic relatedness between mated pairs)
trait <- array(rnorm(200*200,0.6,0.1), dim=c(200,200))
## make the observed pairs more alike
diag(trait) <- diag(trait)+0.02

## make two rows and two colums empty
trait[,3] <- NA
trait[,50] <- NA
trait[6,] <- NA
trait[12,] <- NA

## calculate means; will give NAs because there are missing values
calc <- fgstat(test,trait,mean)

## calculate means
calc <- fgstat(test,trait,mean, na.rm=TRUE)

## plot means
hist(calc)
abline(v=calc[1], col="red", lwd=2)
  

  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}