# Functions for determing GB extents
require(tidyverse)
require(PeriodicTable)
require(InterpretMSSpectrum)

DetermineGBExtents <- function(PosFile = RangedPos,
                               NumberOfBins = Bins,
                               Direction = Orientation,
                               CrossSectionAreaType){

  # Check that valid NumberOfBins and Direction selected
  if(NumberOfBins < 0 | !NumberOfBins%%1==0){
    stop("Invalid NumberOfBins selected.  Must be integer and greater than 0.  Please reinitiate.\n")
  }
  if(!Direction %in% c("x","y","z")){
    stop("Invalid Direction inputted.  Must be 'x', 'y', 'z'.  Please reinitiate.\n")
  }
  # Check CrossSectionAreaType is valid
  if(!any(c("quadrilateral","ellipse") %in% CrossSectionAreaType)){
    stop("Invalid CrossSectionAreaType inputted.  Must be 'quadrilateral', 'ellipse'.  Please reinitiate.\n")
  }

  ElementCountDF <<- SplitComplexIons()

  DeterminingElementForThresholdCalculations()

  ThresholdCheckFunction()

  GrainA <- ElementCountDF %>% filter(Distance < min((ElementCountDF %>%
                                                        filter(UQ(as.symbol(ElementName)) >= CutOffAValue))$Distance))
  GrainB <- ElementCountDF %>% filter(Distance > max((ElementCountDF %>%
                                                        filter(UQ(as.symbol(ElementName)) >= CutOffBValue))$Distance))
  GB <- ElementCountDF %>%
    filter(Distance < min(GrainB$Distance)) %>%
    filter(Distance > max(GrainA$Distance))

  # Calculate cross-sectional area
  # Get max and min extents of axes perpendicular to Direction
  AxesOfInterest <- c("x","y","z")[!grepl(paste0(Direction, collapse = "|"), c("x","y","z"))]
  Axis1Extent = max(RangedPos[AxesOfInterest[1]])-min(RangedPos[AxesOfInterest[1]])
  Axis2Extent = max(RangedPos[AxesOfInterest[2]])-min(RangedPos[AxesOfInterest[2]])
  if(CrossSectionAreaType == "quadrilateral"){
    Area <- Axis1Extent * Axis2Extent
  }
  if(CrossSectionAreaType == "ellipse"){
    Area <- pi * (Axis1Extent/2) * (Axis2Extent/2)
  }

  GibbsExcessCalculator(Area = Area,
                        DetectionEfficiency,
                        GrainBoundary = GB,
                        Grain1 = GrainA,
                        Grain2 = GrainB) #ROI Area and machine detection efficiency

  GrainBoundarySumm(Area, GrainA, GrainB, GB)

  InterfaceStart <<- min((ElementCountDF %>%
                           filter(UQ(as.symbol(ElementName)) >= CutOffAValue))$Distance)

  InterfaceEnd <<- max((ElementCountDF %>%
                         filter(UQ(as.symbol(ElementName)) >= CutOffBValue))$Distance)
  # rm(ElementCountDF, periodicTable, CutOffAValue, CutOffBValue,GrainADistance, GrainBDistance,myColors,ThresholdApprove)
}
