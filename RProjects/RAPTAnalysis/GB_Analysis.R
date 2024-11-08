# Script for analysing a grain boundary containing pos file
# Will determine start and end of interface, GB composition and width
# Will return a .pos file of just the GB region
require(tools)
require(tidyverse)
require(RAPTools)
require(RAPTAnalysis)
require(InterpretMSSpectrum)
require(PeriodicTable)
if("BiocManager" %in% rownames(installed.packages()) == FALSE){
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install("Rdisop")
}

#### User defined  variables - change these for each analysis ####
# Path to .pos file
PosLocation <- c("")
# Path to .rrng file
RangeFileLocation <- c("")
# Detection Efficiency of instrument
DetectionEfficiency = 0.37
# Orientation is the "x", "y", or "z" direction that is perpendicular to the interface
Orientation = "z"
# Number of bins you would like the dataset to be divided into
Bins = 40

##### Combine .pos file with .rrng file to get chemical information ####
RangedPos <- PosFileRanger(
  PosLocation,
  RangeFileLocation)

#### Create datafraem for range file ####
RangeInfo <- read_rrng(RangeFileLocation)

#### Generate 1D concentration profile ####
OneDCountFunc(PosFile = RangedPos,
              NumberOfBins = Bins,
              Direction = Orientation)

# Calculate extent of GB region and perform Gibbs excess calculations
DetermineGBExtents(CrossSectionAreaType = "ellipse")

# Need to save .csv of GB calcs
write.csv(GBSummary,
          paste0(strsplit((PosLocation),"[.]")[[1]][1],"_GBSummary.csv"),
          row.names = F)

# Export .pos of GB region
# Uncomment if you want to save .pos of just GB region
# writeposR(readAPTData(PosLocation) %>%
#             filter(!!as.symbol(Orientation) > InterfaceStart & !!as.symbol(Orientation) < InterfaceEnd),
#           paste0(strsplit((PosLocation),"[.]")[[1]][1],"_GBRegion.pos"))

