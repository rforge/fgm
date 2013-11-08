\name{FGM-package}
\alias{FGM-package}
\docType{package}
\title{
Floating Grid Method
}
\description{
FGM is a permutation test for exploring randomness of dyadic interactions at a range of spatial scales.
}
\details{
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Reinder Radersma & Ben Sheldon
}
\references{
Reinder Radersma & Ben C. Sheldon, 2013. The Floating Grid Method: a new method for spatially-explicit assessment of non-random mating. Manuscript submitted to MEE 
}
\keyword{ package }
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