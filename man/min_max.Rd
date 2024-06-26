% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_max.R
\name{min_max}
\alias{min_max}
\title{Min-max normalization}
\usage{
min_max(data)
}
\arguments{
\item{data}{dataframe with rows = observations and columns = quantiative
variables}
}
\value{
It returns a datafame of normalized data
}
\description{
Min-max normalization transforms each value by subtracting its minimum and
dividing by its range (maximum-minimum). The result is a new variable with a
minimum of zero and a maximum of one
}
\details{
Change the value of variable to negative if it has negative polarity
}
\examples{

data("Education")
Normalization=min_max(Education)
print(Normalization)

}
\references{
OECD/European Union/EC-JRC (2008), Handbook on Constructing
Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
<https://doi.org/10.1787/9789264043466-en>
}
\keyword{min-max}
\keyword{normalization}
