\name{DetermineGBExtents}
\alias{DetermineGBExtents}
\title{DetermineGBExtents}
\usage{
DetermineGBExtents(PosFile, NumberOfBins, Direction)
}
\description{
This function is used to determine the extents of the grain boundary/interface in the atom probe data file.  It does this by calling other functions, including:

SplitComplexIons(), DeterminingElementForThresholdCalculations(), ThresholdCheckFunction(), GibbsExcessCalculator(), GrainBoundarySumm()

PosFile is a dataframe that contains information from a .pos or .APT file that has an additional column with the identity of the ion associated with the mass.  Can be generated using PosFileRanger() function in RAPTools.

NumberOfBins is the number of bins that the user would like to to have along the analysis direction.

Direction is the direction along which the 1D concentration profile should be taken.  "x", "y", "z" are the only valid values.  It should be chosen such that Direction is perpendicular to the interface of interest.

N.B. The area is calculated assuming that the ROI has a square or rectangular cross-section.

}
\examples{

}
