# Functions for checking threshold that user is selecting
ThresholdSuitableCheck <-function(){
  ThresholdApprove<-readline(prompt="Does this element give sensible threshold? (Y/N): " )
  ThresholdApprove<<-as.character(ThresholdApprove)
  if(!any(c("Y","N") %in% ThresholdApprove)){
    stop("Invalid input. You must select 'Y' or 'N'.  Program will terminate.\n")
  }

  if(ThresholdApprove == c("Y")){
    cat(paste0("You have selected ", ElementName," with a p-value of ", PValue," for your calculations."))
  }else{
    DeterminingElementForThresholdCalculations()
    ThresholdCheckFunction()
  }
}

ThresholdCheckFunction <- function(){
  GrainADistance<<-as.numeric(readline(prompt="Distance up to which the Grain/Phase A is definitely present: " ))
  if(GrainADistance <= min(RangedPos[[Orientation]])){
    stop("This is an invalid value - it is below the minimum value in this orientation axis.\n")
  }
  if(GrainADistance > max(RangedPos[[Orientation]])){
    stop("This is an invalid value - it is below the maximum value in this orientation axis.\n")
  }

  GrainBDistance<<-as.numeric(readline(prompt="Distance after which the Grain/Phase B is definitely present: " ))
  if(GrainBDistance <= min(RangedPos[[Orientation]])){
    stop("This is an invalid value - it is below the minimum value in this orientation axis.\n")
  }
  if(GrainBDistance > max(RangedPos[[Orientation]])){
    stop("This is an invalid value - it is below the maximum value in this orientation axis.\n")
  }
  if(GrainBDistance <= GrainADistance){
    stop("This is an invalid value - it is less than the value selected for GrainADistance.\n")
  }

  AdjustedPValue <- 1 - (1 - PValue)^(1/nrow(ElementCountDF))
  x <- 1
  Prob <- 1
  while(Prob >= AdjustedPValue){
    Prob <- ppois(x, lambda = mean((ElementCountDF %>% filter(Distance <= GrainADistance))[[ElementName]]), lower=FALSE)
    x <- x + 1
  }
  CutOffAValue <<- x

  y <- 1
  Prob <- 1
  while(Prob >= AdjustedPValue){
    Prob <- ppois(y, lambda = mean((ElementCountDF %>% filter(Distance >= GrainBDistance))[[ElementName]]), lower=FALSE)
    y <- y + 1
  }
  CutOffBValue <<- y

  print(ggplot(ElementCountDF %>%
                 gather(Element, `Counts in Bin`, -Distance,-Atom_Count) %>%
                 filter(Element == ElementName),
               aes(Distance, `Counts in Bin`)) +
          geom_point(shape = 4,
                     stroke = 2) +
          geom_line(colour = "black", size = 2, alpha = 0.3) +
          geom_hline(yintercept = CutOffAValue,
                     colour="blue",
                     linetype="dashed") +
          geom_hline(yintercept = CutOffBValue,
                     colour="red",
                     linetype="dashed") +
          geom_vline(xintercept = min((ElementCountDF %>%
                                         filter(UQ(as.symbol(ElementName)) >= CutOffAValue))$Distance),
                     colour="blue") +
          geom_vline(xintercept = max((ElementCountDF %>%
                                         filter(UQ(as.symbol(ElementName)) >= CutOffBValue))$Distance),
                     colour="red") +
          annotate("text",
                   x =  min(ElementCountDF$Distance),
                   y = CutOffAValue, label = "Phase A cutoff value",
                   vjust = -1, hjust = -0.5,
                   colour="blue")  +
          annotate("text",
                   x =  max(ElementCountDF$Distance),
                   y = CutOffBValue, label = "Phase B cutoff value",
                   vjust = 1, hjust = 1,
                   colour="red") +
          annotate("text",
                   x =  min((ElementCountDF %>%
                               filter(UQ(as.symbol(ElementName)) >= CutOffAValue))$Distance),
                   y = 0, label = "Interface Start",
                   vjust = 1, hjust = 1.1,
                   colour="blue")  +
          annotate("text",
                   x = max((ElementCountDF %>%
                              filter(UQ(as.symbol(ElementName)) >= CutOffAValue))$Distance),
                   y = 0, label = "Interface End",
                   vjust = 1, hjust = -0.5,
                   colour="red") +
          myTheme() +
          ggtitle(paste0(ElementName," Counts vs Distance")) +
          theme(plot.title = element_text(hjust = 0.5))
  )

  ThresholdSuitableCheck()

}
