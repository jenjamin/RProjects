\name{GB_PosOneDimensionalPlot}
\alias{GB_PosOneDimensionalPlot}
\title{GB_PosOneDimensionalPlot}
\usage{
GB_PosOneDimensionalPlot(PosFile, NumberOfBins, Direction)
}
\description{
This function plots 1D concentration profile from a "ranged" atom probe data file.

PosFile is a dataframe that contains information from a .pos or .APT file that has an additional column with the identity of the ion associated with the mass.  Can be generated using PosFileRanger() function in RAPTools.

NumberOfBins is the number of bins that the user would like to to have along the analysis direction.

Direction is the direction along which the 1D concentration profile should be taken.  "x", "y", "z" are the only valid values.  It should be chosen such that Direction is perpendicular to the interface of interest.

}
\examples{

}
