\name{GrainBoundarySumm}
\alias{GrainBoundarySumm}
\title{GrainBoundarySumm}
\usage{
GrainBoundarySumm(Area, Grain1, Grain2, GB)
}
\description{
This function calculates various parameters of interest in the analysis of grain boundaries/interfaces in APT data.  It will output these to the console and save a variable (GBSummary) to the GlobalEnv.

Area is the cross-sectional area of the interface.  It is important that the interface is perpendicular to the analysis direction

DetectionEfficiency is the detection efficiency of the LEAP instrument used.  Must be between 0 and 1.

GrainBoundary, Grain1, and Grain2 are dataframes containing just the ions that meet the distance criteria calculated during ThresholdCheckFunction()

}
\examples{

}
