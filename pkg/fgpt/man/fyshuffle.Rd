\name{fyshuffle}
\alias{fyshuffle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fisher-Yates shuffle
}
\description{
Function to shuffle vectors according to the Fisher-Yates procedure
}
\usage{
fyshuffle(seq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq}{
Vector containing the sequence which needs to be shuffled. This vector can be of any type and it is allowed to have one element.
}
}
\details{
Other than the \code{sample} function \code{fyshuffle} treats a single value as a vector with one element and will therefore return this element as the shuffled version of the original vector (which are similar).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Durstenfeld (1964) Communications of the ACM 7(7):420
}
\author{
Reinder Radersma
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sample}}
}
\examples{
x <- 1:10
fyshuffle(x)

y <- c("a","b","c","d","e","f")
fyshuffle(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line