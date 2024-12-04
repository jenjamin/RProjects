\name{HIsotope_DepthCalc}
\alias{HIsotope_DepthCalc}
\title{HIsotope_DepthCalc}
\usage{
HIsotope_DepthCalc(RangeFilePath, DataFilePath, BinSize)
}
\description{
This function reads an atom probe datafile and range file.  It creates a dataframe that contains information on how the counts of ions with a mass-to-charge-state ratio of less than 5 Da varies as a function of z depth.  This dataframe can be used to plot how the hydrogen isotope concentrations vary with distance.

RangeFilePath is the filepath to the .rrng file that you would like to read.

DataFilePath is the filepath to the atom probe data file that you would like to read.

BinSize is the size (in nm) of the bins you would like to use.

}
\examples{

}
