\name{CSR_DepthCalc}
\alias{CSR_DepthCalc}
\title{CSR_DepthCalc}
\usage{
CSR_DepthCalc(RangeFilePath, DataFilePath, RunNumber, StableIsotopeMass, BinSize, ChargeStates)
}
\description{
This function reads an atom probe datafile and range file.  By choosing the mass of a stable isotope and charge states, a dataframe will be generated that contains information has to how the counts of each ion charge state change with z distance in the data.  This dataframe can be used to plot how the charge states present varies with distance.

RangeFilePath is the filepath to the .rrng file that you would like to read.

DataFilePath is the filepath to the atom probe data file that you would like to read.

RunNumber is the experiment number/ID.  This is used to label plots.

StableIsotopeMass is the mass of a .  It is a good idea to use the most abundant isotope for a specific element (e.g. 56 for Fe), but to also consider potential peak overlaps with other ions.

BinSize is the size (in nm) of the bins you would like to use.

ChargeStates is a vector of the charge states that you would like to use. e.g. c(1,2,3) or seq(1,3,1)

}
\examples{

}
