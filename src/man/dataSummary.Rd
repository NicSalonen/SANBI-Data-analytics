\name{dataSummary}
\alias{dataSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize All Historic Survey Data from CSV File
%%  ~~function to do ... ~~
}
\description{
The function reads a CSV file of historic acacia data and generates summary plots. It also computes a reproductive summary of invasive acacia species per year based on the presence of reproductive structures.
}
\usage{
dataSummary(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A filepath which includes the name of the CSV file.
%%     ~~Describe \code{x} here~~
}
}
\details{
The date format must be DD/MM/YYYY. If you get the following error
" Error in as.Date.default(un_date_count$dDate) :
do not know how to convert 'un_date_count$dDate' to class “Date”", please check the date format in the data sheet.
}
\value{
Produces informative plots based on the historic data and returns a summary table.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicholas Salonen
}
\note{
Data can be obtained from ....
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
summary <- dataSummary('~/All_Acacia_Data.csv')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
