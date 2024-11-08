# Script for plotting individual elements so user can determine which to use for thresholds
DeterminingElementForThresholdCalculations <- function(){
  IndividualPlot<-readline(prompt="Would you like to plot an individual element (Y/N)? " )
  IndividualPlot<-as.character(IndividualPlot)
  if(!any(c("Y","N") %in% IndividualPlot)){
    stop("Invalid input. You must select 'Y' or 'N'.  Program will terminate.\n")
  }
  while(IndividualPlot == c("Y")){
    ElementName<-readline(prompt="Enter element name you would like to plot: " )
    ElementName<<-as.character(ElementName)

    if(ElementName %in% colnames(ElementCountDF) == TRUE){
      print(paste0("Plotting Graph for ",ElementName))
    }else{
      print(paste0(ElementName, " is not present in your data. Please select an element that is from the list: ",
                   paste(as.character(colnames(ElementCountDF %>% select(-"Distance", -"Atom_Count"))),collapse= ", ")))
    }

    print(ggplot(ElementCountDF %>%
                   gather(Element, value, -Distance,-Atom_Count) %>%
                   filter(Element == ElementName),
                 aes(Distance, value)) +
            geom_line(aes(colour = Element)) +
            geom_point(aes(colour = Element),
                       shape = 4,
                       stroke = 2)+
            labs(x = "Distance (nm)",
                 y = "Count") +
            ggtitle(paste0(ElementName, " vs Distance")) +
            myTheme() +
            scale_color_manual(values = myColors))
    IndividualPlot<-readline(prompt="Would you like to plot an individual element (Y/N)? " )
    IndividualPlot<<-as.character(IndividualPlot)
  }

  ElementName<-readline(prompt="Enter element you would like to use to calculate thresholds: " )
  ElementName<<-as.character(ElementName)
  PValue<<-as.numeric(readline(prompt="Enter p-value you would like to use to calculate thresholds (typically 0.05 or 0.01): " ))

  while(PValue <= 0 | PValue > 1){
    print(paste0("This is an invalid p-value.  Must be between 0 and 1"))
    PValue<<-as.numeric(readline(prompt="Enter p-value you would like to use to calculate thresholds (typically 0.05 or 0.01): " ))
  }

  if(ElementName %in% colnames(ElementCountDF) == TRUE){
    print(paste0(ElementName, " will be used to calculate thresholds with a p-value of ",PValue))
  }else{
    print(paste0(ElementName, " is not present in your data"))
    stop()
  }
}
