\name{performance.plots}
\alias{performance.plots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
performance.plots(fgm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fgm}{
%%     ~~Describe \code{fgm} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fgm) 
{
    par.mfrow <- par("mfrow")
    par.mar <- par("mar")
    par(mfrow = c(2, 2))
    par(mar = c(5, 5, 2, 2))
    plot(fgm$test.scale, fgm$no.cells, pch = 16, xlab = "grid cell size", 
        ylab = "# cells", main = "Number of randomized cells", 
        col = "#00000020")
    plot(fgm$test.scale, fgm$no.pairs, pch = 16, xlab = "grid cell size", 
        ylab = "# pairs", main = "Number of accomplished pairs", 
        col = "#00000020")
    plot(fgm$test.scale, fgm$no.mates[, 1], pch = 16, xlab = "grid cell size", 
        ylab = "mean(# options)", main = "Number of individuals in partner pool for sex 1", 
        col = "#00000020")
    plot(fgm$test.scale, fgm$no.mates[, 2], pch = 16, xlab = "grid cell size", 
        ylab = "mean(# options)", main = "Number of individuals in partner pool for sex 2", 
        col = "#00000020")
    par(mfrow = par.mfrow)
    par(mar = par.mar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
