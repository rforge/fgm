\name{run.perm}
\alias{run.perm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.perm(t, scale, xy1, xy2, group1, group2, trait.table, pairs, replacement, self.loop, ratio)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
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
  \item{replacement}{
%%     ~~Describe \code{replacement} here~~
}
  \item{self.loop}{
%%     ~~Describe \code{self.loop} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
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
function (t, scale, xy1, xy2, group1, group2, trait.table, pairs, 
    replacement, self.loop, ratio) 
{
    m1 <- array(NA, dim = c(dim(xy1)[1], 10))
    m2 <- array(NA, dim = c(dim(xy2)[1], 10))
    m1[, 1] <- 1:dim(m1)[1]
    m2[, 1] <- 1:dim(m2)[1]
    m1[pairs[, 1], 2] <- 1:dim(pairs)[1]
    m2[pairs[, 2], 2] <- 1:dim(pairs)[1]
    m1[, 3] <- group1
    m2[, 3] <- group2
    theta <- runif(1, 0, 0.5 * pi)
    m1[, 4] <- (xy1[, 1] * cos(theta) + xy1[, 2] * sin(theta))/(scale * 
        sqrt(ratio))
    m1[, 5] <- (xy1[, 2] * cos(theta) - xy1[, 1] * sin(theta))/(scale/sqrt(ratio))
    m2[, 4] <- (xy2[, 1] * cos(theta) + xy2[, 2] * sin(theta))/(scale * 
        sqrt(ratio))
    m2[, 5] <- (xy2[, 2] * cos(theta) - xy2[, 1] * sin(theta))/(scale/sqrt(ratio))
    m1[, 6] <- m2[, 6] <- runif(1, min(m1[, 4], m2[, 4]) - 1, 
        min(m1[, 4], m2[, 4]))
    m1[, 7] <- m2[, 7] <- runif(1, min(m1[, 5], m2[, 5]) - 1, 
        min(m1[, 5], m2[, 5]))
    m1[, 8:9] <- ceiling(m1[, 4:5] - m1[, 6:7])
    m2[, 8:9] <- ceiling(m2[, 4:5] - m2[, 6:7])
    cells <- unique(rbind(m1[, c(3, 8, 9)], m2[, c(3, 8, 9)]))
    m1[, 10] <- sapply(m1[, 1], function(i, x, cells) {
        which(cells[, 1] == x[i, 1] & cells[, 2] == x[i, 2] & 
            cells[, 3] == x[i, 3])
    }, cells = cells, x = m1[, c(3, 8, 9)])
    m2[, 10] <- sapply(m2[, 1], function(i, x, cells) {
        which(cells[, 1] == x[i, 1] & cells[, 2] == x[i, 2] & 
            cells[, 3] == x[i, 3])
    }, cells = cells, x = m2[, c(3, 8, 9)])
    no.pot.mates <- cbind(tapply(m1[, 10], factor(m1[, 10], levels = 1:dim(cells)[1]), 
        length), tapply(m2[, 10], factor(m2[, 10], levels = 1:dim(cells)[1]), 
        length))
    no.pot.mates[which(is.na(no.pot.mates[, 1])), 1] <- 0
    no.pot.mates[which(is.na(no.pot.mates[, 2])), 2] <- 0
    ind1 <- ind2 <- list()
    if (replacement == FALSE & self.loop == TRUE) {
        for (i in 1:dim(cells)[1]) {
            locind1 <- m1[m1[, 10] == i, 1]
            locind2 <- m2[m2[, 10] == i, 1]
            if (length(locind1) == length(locind2)) {
                ind1[[i]] <- locind1
                ind2[[i]] <- fyshuffle(locind2)
            }
            else {
                min.length <- min(c(length(locind1), length(locind2)))
                ind1[[i]] <- fyshuffle(locind1)[0:min.length]
                ind2[[i]] <- fyshuffle(locind2)[0:min.length]
            }
        }
    }
    else if (replacement == FALSE & self.loop == FALSE) {
        for (i in 1:dim(cells)[1]) {
            locind1 <- m1[m1[, 10] == i, 1]
            locind2 <- m2[m2[, 10] == i, 1]
            ind1[[i]] <- fyshuffle(locind1)[0:1]
            locind2 <- locind2[which(m2[locind2, 2] != m1[ind1[[i]], 
                2])]
            ind2[[i]] <- fyshuffle(locind2)[0:1]
        }
    }
    else if (replacement == TRUE & self.loop == TRUE) {
        for (i in 1:dim(cells)[1]) {
            ind1[[i]] <- m1[m1[, 10] == i, 1]
            ind2[[i]] <- sample(m2[m2[, 10] == i, 1], length(m1[m1[, 
                10] == i, 1]), replace = TRUE)
        }
    }
    else {
        for (i in 1:dim(cells)[1]) {
            ind1[[i]] <- m1[m1[, 10] == i, 1]
            locind2 <- array(NA, dim = length(m1[m1[, 10] == 
                i, 1]))
            for (j in 1:length(locind2)) {
                locind2[j] <- fyshuffle(m2[m2[m2[, 2] != m1[ind1[[i]][j], 
                  2], 10] == i, 1])[1]
            }
            ind2[[i]] <- locind2
        }
    }
    ind1 <- unlist(ind1)
    ind2 <- unlist(ind2)
    gm <- ifelse(length(ind1) < 1, NA, mean(diag(trait.table[ind1, 
        ind2]), na.rm = TRUE))
    gv <- ifelse(length(ind1) < 2, NA, var(diag(trait.table[ind1, 
        ind2]), na.rm = TRUE))
    lv <- ifelse(length(ind1) < 2, NA, mean(tapply(diag(trait.table[ind1, 
        ind2]), factor(m1[ind1, 10], levels = 1:dim(cells)[1]), 
        var) - tapply(diag(trait.table[pairs[, 1], pairs[, 2]]), 
        factor(m1[pairs[, 1], 10], levels = 1:dim(cells)[1]), 
        var), na.rm = TRUE))
    nc <- dim(cells)[1]
    np <- length(ind1)
    nm1 <- mean(no.pot.mates[no.pot.mates[, 1] > 0, 2])
    nm2 <- mean(no.pot.mates[no.pot.mates[, 2] > 0, 1])
    perm.output <- list(gm, gv, lv, nc, np, nm1, nm2)
    return(perm.output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
