#Calculates Gibbs Excess of Interface

GibbsExcessCalculator <- function(Area, DetectionEfficiency, GrainBoundary, Grain1, Grain2, DataframeOfDeconvolutedCounts) {
  # Check detection efficiency valid
  if(DetectionEfficiency < 0 | DetectionEfficiency > 1){
    stop("Invalid DetectionEfficiency selected.  Must be between 0 and 1.  Please reinitiate.\n")
  }

  DividingSurfacePosition <- (mean(GrainBoundary$Distance)-min(Grain1$Distance)) / (max(Grain2$Distance)-min(Grain1$Distance)) # remember to normalise for length of ROI
  LowerSurfacePosition <- (min(GrainBoundary$Distance)-min(Grain1$Distance)) / (max(Grain2$Distance)-min(Grain1$Distance))
  UpperSurfacePosition <- (max(GrainBoundary$Distance)-min(Grain1$Distance)) / (max(Grain2$Distance)-min(Grain1$Distance))

  TotalVolumeConc <- as.data.frame(colSums(
    DataframeOfDeconvolutedCounts %>%
      select(-Distance, -Atom_Count)
    )) %>%
    rownames_to_column("Element") %>%
    rename(NumberCounts = 2)

  TotalNIons <- sum(TotalVolumeConc$NumberCounts)

  TotalVolumeConc <- TotalVolumeConc %>%
    mutate(GlobalFraction = NumberCounts / TotalNIons)

  Grain1Conc <- as.data.frame(colSums(Grain1 %>%
                                        select(-Distance, -Atom_Count))) %>%
    rownames_to_column("Element") %>%
    rename(NumberCounts = 2)

  Grain1Ions <- sum(Grain1Conc$NumberCounts)

  Grain1Conc <- Grain1Conc %>%
    transmute(Grain1Fraction = NumberCounts / Grain1Ions)

  Grain2Conc <- as.data.frame(colSums(Grain2 %>%
                                        select(-Distance, -Atom_Count))) %>%
    rownames_to_column("Element") %>%
    rename(NumberCounts = 2)

  Grain2Ions <- sum(Grain2Conc$NumberCounts)

  Grain2Conc <- Grain2Conc %>%
    transmute(Grain2Fraction = NumberCounts / Grain2Ions)


  GibbsExcess <<- (cbind(TotalVolumeConc,
                         Grain1Conc,
                         Grain2Conc)) %>%
    mutate(
      Grain1GE = (1 / (Area * DetectionEfficiency)) * TotalNIons * (GlobalFraction - Grain1Fraction),
      Grain2GE = (1 / (Area * DetectionEfficiency)) * TotalNIons * (GlobalFraction - Grain2Fraction),
      GibbsExcess = (1 / (Area * DetectionEfficiency)) * TotalNIons * (
        GlobalFraction -
          Grain1Fraction * (DividingSurfacePosition) -
          Grain2Fraction * (1 - DividingSurfacePosition)
      ),
      GibbsExcessLower = (1 / (Area * DetectionEfficiency)) * TotalNIons * (
        GlobalFraction -
          Grain1Fraction * (LowerSurfacePosition) -
          Grain2Fraction * (1 - LowerSurfacePosition)
      ),
      GibbsExcessUpper = (1 / (Area * DetectionEfficiency)) * TotalNIons * (
        GlobalFraction -
          Grain1Fraction * (UpperSurfacePosition) -
          Grain2Fraction * (1 - UpperSurfacePosition)
      )

    )

}
