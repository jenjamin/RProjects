# Function to return summary of GB
GrainBoundarySumm <- function(Area, Grain1, Grain2, GB, CrossSectionType) {
  GBWidth <- max(GB$Distance) - min(GB$Distance)

  GBComp <- as.data.frame(colSums(GB %>%
                                    select(-Distance,-Atom_Count)))

  Total = sum(GBComp)

  GBComp <- GBComp %>%
    rownames_to_column("Element") %>%
    mutate(Composition = 100 * `colSums(GB %>% select(-Distance, -Atom_Count))` /
             Total)

  GBSummary <<- cbind(GBComp,
                      GibbsExcess %>%
                        select(-Element)) %>%
    select(Element,
           Composition,
           GibbsExcess,
           GibbsExcessLower,
           GibbsExcessUpper,
           Grain1Fraction,
           Grain2Fraction) %>%
    mutate(
      LowerErr = abs(GibbsExcess - GibbsExcessLower),
      UpperErr = abs(GibbsExcess - GibbsExcessUpper)
    ) %>%
    transmute(
      Element = Element,
      `Interface Composition (at.%)` = round(Composition,4),
      `Gibbs Excess` = GibbsExcess,
      `Gibbs Error` = pmax(LowerErr, UpperErr),
      `Phase A Composition (at.%)` = round(100 * Grain1Fraction,4),
      `Phase B Composition (at.%)` = round(100 * Grain2Fraction,4)
    )

  print(GBSummary)
  # setwd(NewDirectory)
  # write.csv(GBSummary,"Grain Boundary Calculations.csv",
  #           row.names = FALSE)
  # setwd('..')
  print(paste0("Width of interface is: ",round(GBWidth,2)," nm (2dp)"))
  print(paste0("Area used to calculated interface values was : ",round(Area,2)," nm^2 (2dp). *Area calculated by assuming ", CrossSectionType, " cross section*"))
}
