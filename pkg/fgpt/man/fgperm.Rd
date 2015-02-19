\name{fgperm}
\alias{fgperm}
\alias{fgploc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Floating Grid Permutation Technique
}
\description{
The \code{fgperm} function produces sets of permuted observations or indices using the Floating Grid Permutation Technique, which is a method for spatially restricted permutations. \code{fgploc} offers additional functionality to manipulate observations within grid cells, for instance observations could be scaled with grid cells.  
}
\usage{
fgperm(xy,z=1:dim(xy)[1], scale, group=1, iter=999, ratio=1, FUN=fyshuffle, ..., 
  add.obs=FALSE, as.matrix=FALSE)
fgploc(xy, scale, group=1, iter=999, ratio=1, FUN=fyshuffle, FUN.mani, ..., 
  marks, pass.rand=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
Two-column matrix with the geographical locations of observations.
}
  \item{z}{
Vector with the observations. If left empty \code{z} will be a vector of indices from 1 to the total number of geographical locations. Those indices can be later be used for calculating statistics with \code{fgstat}.
}
  \item{scale}{
Value indicating the spatial scale of the permutations. \code{scale} should be positive.
}
  \item{group}{
Optional group membership of observations. \code{group} can be used when observations are for instance collected over multiple years.
}
  \item{iter}{
Number of iterations for every grid cell size. Default is 999. Note that in order to produce a probability an observation is assigned to any of the geographical locations is a negative function of the distance between its original and assigned location many iterations are needed.
}
  \item{ratio}{
The ratio between the sides of the grid cells. Default is 1.
}
  \item{FUN}{
Function to perform randomizations. Note that the function must be able to randomize one value, which is for instance a issue if using \code{sample}. For solutions see the Details and Examples of \code{sample}. The default is the Fisher-Yates shuffle \code{fyshuffle}.
}
  \item{FUN.mani}{
Function to perform manipulations of the observations within grid cells. This functionality should be used together with \code{cal.stat} to calculate specific statistics.
}
  \item{...}{
Optional arguments to \code{FUN} and \code{FUN.mani}.
}
  \item{marks}{
Should either be left empty, be a vector or a matrix. When \code{z} contains observations \code{marks} should be left empty. If the randomization procedure is for testing one variable, \code{marks} should be a vector for which the row numbers correspond to the index values in \code{z}. If the randomization procedure is for testing two variables (for instance the distance between them), \code{marks} should be a matrix for which the row numbers correspond to the index values in \code{z}.
}

  \item{add.obs}{
If \code{TRUE} the first set in the output will be the observed values.
}
  \item{as.matrix}{
If \code{TRUE} the output will be a matrix. If \code{FALSE} the output will be a list, which is needed when analyzing the data with \code{cal.stat}.
}
  \item{pass.rand}{
If \code{TRUE} The sets of randomized observations are also passed on to the output. Default is \code{FALSE}.
}
}
\details{
Before using those functions please read the reference or vignette. Alternatively use the more user-friendly function \code{fgeasy}. If there are missing values for the observations, leave \code{z} empty and enter the observations as \code{marks} in the \code{fgstat} function.
}
\value{
\code{fgperm} returns a \code{list} or a \code{matrix}, depending on the setting of argument \code{as.matrix}.
}
\references{
Reinder Radersma & Ben C. Sheldon, 2015. A new permutation technique to explore and control for spatial autocorrelation. Submitted
}
\author{
Reinder Radersma
}

\examples{

## 200 random geographical locations
xy <- array(runif(400,0,2), dim=c(200,2))

## run fgperm to produce 99 randomizations for scale 1
test <- fgperm(xy, scale=1, iter=99, add.obs=TRUE) 

## run fgperm to produce 99 bootstraps for scale 1
test <- fgperm(xy, scale=1, iter=99, FUN=function(x){
          x[sample.int(length(x),replace=TRUE)]}, add.obs=TRUE) 

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
\keyword{ distribution }
