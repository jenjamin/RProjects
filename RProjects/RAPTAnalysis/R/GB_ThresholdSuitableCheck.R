# Functions for checking threshold that user is selecting
ThresholdSuitableCheck <-function(){
  ThresholdApprove<-readline(prompt="Does this element give sensible threshold? (Y/N): " )
  ThresholdApprove<-as.character(ThresholdApprove)
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
