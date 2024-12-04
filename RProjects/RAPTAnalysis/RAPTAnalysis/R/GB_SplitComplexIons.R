# Function for separating complex ions into their consituent elements
require(PeriodicTable)
require(InterpretMSSpectrum)
SplitComplexIons <- function(DataframeOfCounts){

  IonList <- colnames(DataframeOfCounts %>%
                        select(-Distance))
  CorrectDF <- data.frame()
  for(Ions in IonList){
    SplitIon <- CountChemicalElements(Ions)

    # Check that ions are made up of real elements (i.e. not custom names)
    data(periodicTable)
    if(!all(names(SplitIon) %in% periodicTable$symb)){
      warning(paste0("Error - ", Ions," contains at least one part that is not an element present in the periodic table \n"))
    }
    # Create dataframe with corrected count
    IonCount <- DataframeOfCounts %>%
      select(Distance, Ions)
    CorrectElementCountInIon = data.frame()
    for (Element in 1:length(SplitIon)) {
      CorrectElementCount <- cbind(IonCount %>% select(Distance),
                                   ((IonCount %>%
                                       select(-Distance))* SplitIon[Element]))
      colnames(CorrectElementCount) <- c("Distance", names(SplitIon[Element]))
      CorrectElementCount <- CorrectElementCount %>%
        gather(Element, Count, -Distance)
      CorrectElementCountInIon <- rbind(CorrectElementCountInIon,CorrectElementCount)
    }
    CorrectDF <- rbind(CorrectDF,
                       CorrectElementCountInIon)
  }

  ElementCountDF <- CorrectDF %>%
    group_by(Element, Distance) %>%
    summarise(Count = sum(Count)) %>%
    spread(Element, Count) %>%
    ungroup()

  ElementCountDF <- ElementCountDF %>%
    mutate(Atom_Count = rowSums(.[2:ncol(ElementCountDF)]))

  print(ggplot(ElementCountDF %>%
                 gather(Element, value, -Distance,-Atom_Count) %>%
                 filter(Element != "O") %>%
                 mutate(`Concentration (at.%)` = 100 * (value/Atom_Count)),
               aes(Distance, `Concentration (at.%)`)) +
          myTheme() +
          geom_point(aes(colour = Element),
                     shape = 4,
                     stroke = 2) +
          ylim(0,100) +
          myTheme() +
          theme(aspect.ratio = 1) +
          geom_line(aes(colour = Element)) +
          labs(x = "Distance (nm)") +
          scale_color_manual(values = myColors)
  )

  return(ElementCountDF)
}
