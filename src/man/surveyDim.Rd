\name{surveyDim}
\alias{surveyDim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimates Survey Site Dimensions
%%  ~~function to do ... ~~
}
\description{
Based on the haversine equation, this function will compute the latitudinal and longitudinal distances of a survey site given the lat/lon data of a survey track and compile the tracks together. This allows to accuratley set bin widths.
}
\usage{
srvyDetails <- surveyDim(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
        Any number of lat/lon track arrays which were imported from CSV/GPX            data using importSurvey.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
srvyDim returns a list which consists of:%%  If it is a LIST, use
  \item{varargin }{Lat/lon array of track data.}
  \item{lat_dist }{Latitudinal distance in meters.}
  \item{lon_dist }{Longitudinal distance in meters.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicholas Salonen
}
\note{
It is not necessary to pass waypoint data as arguments.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{importSurvey}}
}
\examples{
srvydetailsSINGLE <- surveyDim(trackcsv)
srvydetailsMULTI <- surveyDim(trackgpx1, trackgpx2, ..., trackgpxN)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ haversine }% use one of  RShowDoc("KEYWORDS")
\keyword{ lat/lon }% __ONLY ONE__ keyword per line
