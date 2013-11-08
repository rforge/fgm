\name{fgm}
\alias{fgm}
\alias{summary.fgm}
\alias{plot.fgm}
\alias{mcfgm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Floating Grid Method
}
\description{
The Floating Grid Method is a spatially explicit permutation test for dyadic interactions. Please read the reference mentioned below before using this function.
}
\usage{
fgm(xy1, xy2 = xy1, group1 = 1, group2 = group1, trait.table, pairs = cbind(1:dim(xy1)[1], 1:dim(xy1)[1]), iter = 999, ratio = 1, scale.seq = seq(from = 0, to = max(dist(rbind(xy1, xy2))), length.out = 21)[2:21], two.types = TRUE, replacement = FALSE, self.loop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy1}{
Geographical locations of individuals of type 1.
}
  \item{xy2}{
Geographical locations of individuals of type 2. As default the values of \code{xy1} are taken.
}
  \item{group1}{
Group membership of individuals of type 1. \code{group1} is optional. 
}
  \item{group2}{
Group membership of individuals of type 2. \code{group2} is optional and as default the values of \code{group1} are taken.
}
  \item{trait.table}{
\code{n} by \code{m} table for which \code{n} is the number of individuals of type 1 and \code{m} the number of individuals od type 2. This table contains all possible values for the trait of interest.  
}
  \item{pairs}{
\code{n} by 2 table for which \code{n} is the total number of observed dyadic interactions. The first column contains the individuals of type 1 and the second column the individuals of type 2 involved in the dyadic interactions. \code{pairs} is optional and as default the columns and rows of \code{trait.table} are taken respectively as the first and second column of the \code{pairs} table. 
}
  \item{iter}{
Number of iterations for every grid cell size. Default is 999.
}
  \item{ratio}{
The ratio between the sides of the grid cells. Default is 1.
}
  \item{scale.seq}{
Sequence of all grid cell sizes that will be tested. The number, order and values of this sequence are completely free, as long as they are numeric. Default is 20 equaly spaced out grid cell sizes from slightly larger than 0 to the maximum distance between any two individuals. Use the \code{explore.fgm} function to make a informed decision on the right sequence of grid cell sizes.
}
  \item{two.types}{
\code{TRUE} if the individuals are of two different types, \code{FALSE} if not. Default is \code{TRUE}.
}
  \item{replacement}{
\code{TRUE} if individuals of type 2 need to be drawn with replacement, \code{FALSE} if not. Default is \code{FALSE}.
}
  \item{self.loop}{
\code{TRUE} if two individuals of differnt types that share a geographical location are allowed to interact, \code{FALSE} if not. Default is \code{TRUE}.
}
}
\details{
\code{mcfgm} is the parallel computing version of \code{fgm}. \code{mcfgm} do not work under Windows.
}
\value{
\code{fgm} returns an object of \code{class} "fgm", which can be accessed with the following functions: \code{summary}, \code{plot} and \code{performance.plots}.
}
\references{
Reinder Radersma & Ben C. Sheldon, 2013. The Floating Grid Method: a new method for spatially-explicit assessment of dyadic interactions. Manuscript submitted to MEE
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

## Produce 20 geographical locations.
loc.x <- 1:20
loc.y <- runif(20,0,5)

## Produce 2 x 20 phenotypes.
type1 <- 11:30+runif(20,0,5)
type2 <- 11:30+runif(20,0,5)

## Take the difference between  
## all individuals of type 1 and all individuals of type 2.
trait.table <- array(rep(type1,each=20)-rep(type2,20),dim=c(20,20))

fg <- fgm(xy1=cbind(loc.x,loc.y),trait.table=trait.table, iter=99)
summary(fg)
plot(fg)
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
