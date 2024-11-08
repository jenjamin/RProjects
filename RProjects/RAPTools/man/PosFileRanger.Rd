\name{PosFileRanger}
\alias{PosFileRanger}
\title{PosFileRanger}
\usage{
PosFileRanger(PosFile, RangeFilePath)
}
\description{
PosFile is the filepath to the .pos or .APT file that you would like to read.

RangeFilePath is the filepath to the .rrng file that you would like to read.

This function takes an atom probe datafile and a .rrng file and returns an dataframe with all of the original data, plus an additional column with the name of the ion that corresponds to that range in the .rrng file.

Any masses that fall outside of a defined mass range will be assinged as "Noise".
}
\examples{

}
