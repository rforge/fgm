\name{summary.fgm}
\alias{summary.fgm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.fgm(fgm)
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
    if (class(fgm) != "fgm") {
        stop("Object needs to be of class \"fgm\"")
    }
    p.val <- function(perm, obsv) {
        return(min((sum(perm >= obsv) + 1)/(sum(is.na(perm) == 
            FALSE) + 1) * 2, (sum(perm <= obsv) + 1)/(sum(is.na(perm) == 
            FALSE) + 1) * 2, 1))
    }
    fail <- function(perm) {
        return(sum(is.na(perm))/length(perm) * 100)
    }
    gm.table <- cbind(sapply(fgm$perm, "[", 1), fgm$obsv$m, sapply(sapply(fgm$perm, 
        "[", 2), mean, na.rm = TRUE), sapply(sapply(fgm$perm, 
        "[", 2), quantile, probs = 0.025, na.rm = TRUE), sapply(sapply(fgm$perm, 
        "[", 2), quantile, na.rm = TRUE, probs = 0.975), sapply(sapply(fgm$perm, 
        "[", 2), p.val, obsv = fgm$obsv$m), sapply(sapply(fgm$perm, 
        "[", 2), fail))
    colnames(gm.table) <- c("cell_size", "observed", "mean", 
        "lower_limit", "upper_limit", "P_value", "\% failures")
    rownames(gm.table) <- rep("", dim(gm.table)[1])
    gv.table <- cbind(sapply(fgm$perm, "[", 1), fgm$obsv$v, sapply(sapply(fgm$perm, 
        "[", 3), mean, na.rm = TRUE), sapply(sapply(fgm$perm, 
        "[", 3), quantile, probs = 0.025, na.rm = TRUE), sapply(sapply(fgm$perm, 
        "[", 3), quantile, na.rm = TRUE, probs = 0.975), sapply(sapply(fgm$perm, 
        "[", 3), p.val, obsv = fgm$obsv$v), sapply(sapply(fgm$perm, 
        "[", 3), fail))
    colnames(gv.table) <- c("cell_size", "observed", "mean", 
        "lower_limit", "upper_limit", "P_value", "\% failures")
    rownames(gv.table) <- rep("", dim(gv.table)[1])
    lv.table <- cbind(sapply(fgm$perm, "[", 1), 0, sapply(sapply(fgm$perm, 
        "[", 4), mean, na.rm = TRUE), sapply(sapply(fgm$perm, 
        "[", 4), quantile, na.rm = TRUE, probs = 0.025), sapply(sapply(fgm$perm, 
        "[", 4), quantile, na.rm = TRUE, probs = 0.975), sapply(sapply(fgm$perm, 
        "[", 4), p.val, obsv = 0), sapply(sapply(fgm$perm, "[", 
        4), fail))
    colnames(lv.table) <- c("cell_size", "observed", "mean", 
        "lower_limit", "upper_limit", "P_value", "\% failures")
    rownames(lv.table) <- rep("", dim(lv.table)[1])
    cat("\n")
    cat("======================", "\n")
    cat(" Floating Grid Method", "\n")
    cat("======================", "\n")
    cat("\n")
    cat(paste("Number of scales:", length(fgm$perm)), "\n")
    cat(paste("Number of iterations per scale:", fgm$iter), "\n")
    cat("\n")
    cat("--------------------------", "\n")
    cat(" Results for global mean:", "\n")
    cat("--------------------------", "\n")
    cat("\n")
    print(gm.table, row.names = FALSE)
    cat("\n")
    cat(" Results for global variance:", "\n")
    cat("------------------------------", "\n")
    cat("\n")
    print(gv.table, row.names = FALSE)
    cat("\n")
    cat(" Results for local variance:", "\n")
    cat("-----------------------------", "\n")
    cat("\n")
    print(lv.table, row.names = FALSE)
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
