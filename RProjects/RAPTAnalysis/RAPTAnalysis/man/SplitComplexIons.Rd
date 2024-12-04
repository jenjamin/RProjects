\name{SplitComplexIons}
\alias{SplitComplexIons}
\title{SplitComplexIons}
\usage{
SplitComplexIons(DataframeOfCounts)
}
\description{
This function breaks complex ions into their consituent parts and returns a dataframe of Distance and counts for each elemennt.  For example, MoC into Mo and C or H2O into 2H and O.  This is useful when trying to plot changes in concentration of a certain element with distance.

DataframeOfCounts is a dataframe of counts for each ion with distance.  Generated from GB_PosOneDimensionalPlot() and returned to global environment.

}
\examples{

}
