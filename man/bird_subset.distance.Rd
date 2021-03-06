% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bird_subset.distance.r
\name{bird_subset.distance}
\alias{bird_subset.distance}
\title{Subsets point count distance to specified value}
\usage{
bird_subset.distance(df, distance)
}
\arguments{
\item{df}{A data frame object}

\item{distance}{The distance that limit your data. 300 is max 1 is minimum.}
}
\value{
A data frame subsetted by x distance
}
\description{
Takes bird data that has been prepared and subsets the dataframe based on distance.
}
\examples{
data = bird_subset.distance(df)

}
