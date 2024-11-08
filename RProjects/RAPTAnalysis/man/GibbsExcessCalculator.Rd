\name{GibbsExcessCalculator}
\alias{GibbsExcessCalculator}
\title{GibbsExcessCalculator}
\usage{
GibbsExcessCalculator(Area, DetectionEfficiency, GrainBoundary, Grain1, Grain2)
}
\description{
This function calculates the Gibbs interfacial excess of each element present in the atom probe data file.

Area is the cross-sectional area of the interface.  It is important that the interface is perpendicular to the analysis direction

DetectionEfficiency is the detection efficiency of the LEAP instrument used.  Must be between 0 and 1.

GrainBoundary, Grain1, and Grain2 are dataframes containing just the ions that meet the distance criteria calculated during ThresholdCheckFunction()

}
\examples{

}
