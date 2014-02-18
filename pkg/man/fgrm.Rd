\name{fgrm}
\alias{fgrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgrm(xy, z = 1:dim(xy)[1], scale, group = 1, iter = 999, ratio = 1, FUN = fyshuffle, FUN.mani, ..., marks, pass.rand = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{FUN.mani}{
%%     ~~Describe \code{FUN.mani} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{marks}{
%%     ~~Describe \code{marks} here~~
}
  \item{pass.rand}{
%%     ~~Describe \code{pass.rand} here~~
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
function (xy, z = 1:dim(xy)[1], scale, group = 1, iter = 999, 
    ratio = 1, FUN = fyshuffle, FUN.mani, ..., marks, pass.rand = FALSE) 
{
    runrand <- function(t) {
        m <- array(NA, dim = c(dim(xy)[1], 9))
        m[, 1] <- z
        m[, 2] <- group
        theta <- runif(1, 0, pi)
        m[, 3] <- (xy[, 1] * cos(theta) + xy[, 2] * sin(theta))/(scale * 
            sqrt(ratio))
        m[, 4] <- (xy[, 2] * cos(theta) - xy[, 1] * sin(theta))/(scale/sqrt(ratio))
        m[, 5] <- runif(1, min(m[, 3]) - 1, min(m[, 3]))
        m[, 6] <- runif(1, min(m[, 4]) - 1, min(m[, 4]))
        m[, 7:8] <- ceiling(m[, 3:4] - m[, 5:6])
        cells <- unique(m[, c(2, 7, 8)])
        m[, 9] <- sapply(1:dim(m)[1], function(i, x, cells) {
            which(cells[, 1] == x[i, 1] & cells[, 2] == x[i, 
                2] & cells[, 3] == x[i, 3])
        }, cells = cells, x = m[, c(2, 7, 8)])
        permutes <- ave(m[, 1], m[, 9], FUN = FUN)
        if (is.vector(marks)) {
            stats <- ave(permutes, m[, 9], FUN = function(x) {
                FUN.mani(marks[x])
            })
        }
        else {
            stats <- ave(permutes, m[, 9], FUN = function(x) {
                FUN.mani(diag(marks[x, 1:dim(marks)[1]]))
            })
        }
        if (pass.rand) {
            return(list(permutes = permutes, stats = stats))
        }
        else {
            return(stats)
        }
    }
    output <- lapply(2:iter, runrand)
    if (add.obs == TRUE) {
        output <- c(observed = list(z), output)
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
