% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bird_species.all.r
\name{bird_species.all}
\alias{bird_species.all}
\title{Creates Species dataframe}
\usage{
bird_species.all(df)
}
\arguments{
\item{df}{A data frame object}
}
\value{
A data frame that should be saved as species
}
\description{
Takes bird data that has been prepared by bird_prepare function and creates a dataframe summerising all observations of species by year.
}
\examples{
species = bird_species.all(df)

}
