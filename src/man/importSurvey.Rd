\name{importSurvey}
\alias{importSurvey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import Survey Data from CSV/GPX Files
}
\description{
Imports lat/lon data from a CSV/GPX file of GPS track/waypoint data. The user
specifies the directory and file as well as being able to specify the type of
data (waypoint/track) in the CSV/GPX file. If you are unsure about your data,
Garmin Basecamp/Google Earth Pro are free to use mapping tools which can be
used to better understand your data.
}
\usage{
importSurvey(filepath, filename, filetype = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
    Specifies the path the the directory in which the CSV/GPX file is located.
  }
  \item{filename}{
    Specifies the name of the CSV/GPX file in filepath.
  }
  \item{filetype}{
    Specifies the type of data to import (waypoint or track).
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame object consisting of lat/lon data.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicholas Salonen
}
\note{
The default for filetype is NULL, however, if the data is imported from a GPX file and the filetype is not specified, filetype is set to 'track'. This could incorrectly import waypoint data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

surveyWpts <- importSurvey('~/DataDir', 'Invasive_Acacia.gpx', filetype = 'waypoint')
surveyTrack <- importSurvey('~/DataDir', 'Invasive_Acacia.gpx', filetype = 'track')
surveyData <- importSurvey('~/DataDir', 'Invasive_Acacia.csv', filetype = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ import }% use one of  RShowDoc("KEYWORDS")
\keyword{ CSV }% __ONLY ONE__ keyword per line
\keyword{ GPX }
