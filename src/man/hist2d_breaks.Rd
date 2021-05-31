\name{hist2d_breaks}
\alias{hist2d_breaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute And Plot A 2-Dimensional Histogram using Breaks
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hist2d_breaks(x, y = NULL, nbins = 200, same.scale = FALSE, na.rm = TRUE, show = TRUE, col = c("black", heat.colors(12)), FUN = base::length, xlab, ylab, x.breaks, y.breaks, ...)
}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{x}{
%%     ~~Describe \code{x} here~~
%}
%  \item{y}{
%%     ~~Describe \code{y} here~~
%}
%  \item{nbins}{
%%     ~~Describe \code{nbins} here~~
%}
%  \item{same.scale}{
%%     ~~Describe \code{same.scale} here~~
%}
 % \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
%}
%  \item{show}{
%%     ~~Describe \code{show} here~~
%}
%  \item{col}{
%%     ~~Describe \code{col} here~~
%}
%  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
%}
%  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
%}
%  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
%}
%  \item{x.breaks}{
%%     ~~Describe \code{x.breaks} here~~
%}
%  \item{y.breaks}{
%%     ~~Describe \code{y.breaks} here~~
%}
%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%}
%}
\details{
This is an adapted version of hist2d from package gplots which allows for x.breaks and y.breaks to be specified. Please see hist2d documentation.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist2d}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y = NULL, nbins = 200, same.scale = FALSE, na.rm = TRUE,
    show = TRUE, col = c("black", heat.colors(12)), FUN = base::length,
    xlab, ylab, x.breaks, y.breaks, ...)
{
    if (is.null(y)) {
        if (ncol(x) != 2)
            stop("If y is ommitted, x must be a 2 column matirx")
        y <- x[, 2]
        x <- x[, 1]
    }
    if (length(nbins) == 1)
        nbins <- rep(nbins, 2)
    nas <- is.na(x) | is.na(y)
    if (na.rm) {
        x <- x[!nas]
        y <- y[!nas]
    }
    else stop("missinig values not permitted if na.rm=FALSE")
    if (same.scale) {
        x.cuts = x.breaks
        y.cuts = x.breaks
    }
    else {
        x.cuts <- x.breaks
        y.cuts <- y.breaks
    }
    index.x <- cut(x, x.cuts, include.lowest = TRUE)
    index.y <- cut(y, y.cuts, include.lowest = TRUE)
    m <- tapply(x, list(index.x, index.y), FUN)
    if (identical(FUN, base::length))
        m[is.na(m)] <- 0
    if (missing(xlab))
        xlab <- deparse(substitute(xlab))
    if (missing(ylab))
        ylab <- deparse(substitute(ylab))
    if (show) {
        if (legend) {
            image.plot(x.cuts, y.cuts, m, col = col, xlab = xlab,
                ylab = ylab, ...)
        }
        else {
            image(x.cuts, y.cuts, m, col = col, xlab = xlab,
                ylab = ylab, ...)
        }
    }
    midpoints <- function(x) (x[-1] + x[-length(x)])/2
    retval <- list()
    retval$counts <- m
    retval$counts_rel <- m/max(m)
    retval$x.breaks = x.cuts
    retval$y.breaks = y.cuts
    retval$x = midpoints(x.cuts)
    retval$y = midpoints(y.cuts)
    retval$nobs = length(x)
    retval$bins = c(length(x.cuts), length(y.cuts))
    retval$call <- match.call()
    class(retval) <- "hist2d"
    retval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
