# Function to combine .pos file and .rrng file
# Useful if want to plot histograms of certain ions etc.

# NEED TO INCLUDE
# CHECK IF FED FILE PATH OR ALREADY READ FILES
PosFileRanger <- function(PosFile, RangeFilePath){
  require(tidyverse)

  # Read .rrng and APDataFile
  RangeInfo <- read_rrng(RangeFilePath)
  PosRanged <- readAPTData((PosFile))
  # Check colnames and relabel m to Mass if necessary
  if(!any(c("m","Mass") %in% colnames(PosRanged))){
    stop("PosFileRanger Error - no 'mass' or 'm' column in data file.\n")
  }
  if("m" %in% colnames(PosRanged)){
    PosRanged <- PosRanged %>% rename(Mass= m)
  }
  #### Generate cut breaks and labels for ranges ####
  CutBreaks <- c(-0.10)
  for (Rows in seq(nrow(RangeInfo))) {
    CutBreaks <- c(
      CutBreaks,
      (RangeInfo %>%arrange(Start))$Start[Rows]-0.00001,
      (RangeInfo %>%arrange(Start))$End[Rows]+0.00001)
  }
  CutBreaks <- c(CutBreaks, Inf)

  CutList <- c("Noise")
  for (Rows in seq(nrow(RangeInfo))) {
    CutList <- c(
      CutList,
      (RangeInfo %>%arrange(Start))$Ion[Rows],
      "Noise")
  }
  #### Range pos ####
  PosRanged$Ion <- cut(PosRanged$Mass,
                           breaks = CutBreaks,
                           labels = CutList,
                           right = F)
  return(PosRanged)
  rm(i)
}
