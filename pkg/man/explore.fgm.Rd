\name{explore.fgm}
\alias{explore.fgm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
explore.fgm(xy1, xy2 = xy1, group1 = 1, group2 = group1, trait.table, pairs = cbind(1:dim(xy1)[1], 1:dim(xy1)[1]), iter = 10000, ratio = 1, two.types = TRUE, replacement = FALSE, self.loop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy1}{
%%     ~~Describe \code{xy1} here~~
}
  \item{xy2}{
%%     ~~Describe \code{xy2} here~~
}
  \item{group1}{
%%     ~~Describe \code{group1} here~~
}
  \item{group2}{
%%     ~~Describe \code{group2} here~~
}
  \item{trait.table}{
%%     ~~Describe \code{trait.table} here~~
}
  \item{pairs}{
%%     ~~Describe \code{pairs} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
}
  \item{two.types}{
%%     ~~Describe \code{two.types} here~~
}
  \item{replacement}{
%%     ~~Describe \code{replacement} here~~
}
  \item{self.loop}{
%%     ~~Describe \code{self.loop} here~~
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
function (xy1, xy2 = xy1, group1 = 1, group2 = group1, trait.table, 
    pairs = cbind(1:dim(xy1)[1], 1:dim(xy1)[1]), iter = 10000, 
    ratio = 1, two.types = TRUE, replacement = FALSE, self.loop = FALSE) 
{
    output <- fgm(xy1 = xy1, xy2 = xy2, group1 = group1, group2 = group2, 
        trait.table = trait.table, pairs = pairs, iter = 1, ratio = ratio, 
        scale.seq = runif(iter, 0, max(dist(cbind(xy1, xy2)))), 
        two.types = two.types, replacement = replacement, self.loop = self.loop)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
