#Function for reading .apt file to dataframe
readaptR <- function(posFileName) {
  # Save information into list called apt
  apt <- PARAPROBE_Transcoder2(posFileName)

  #### Get List of variables ####
  VariablesPresent <- c()
  for (SectionNumber in seq(length(apt$idtfyd_sections))) {
    if(apt$idtfyd_sections[[SectionNumber]]$healthy==T){
      VariablesPresent[SectionNumber] <- apt$idtfyd_sections[[SectionNumber]]$wcSectionType
    }
  }

  # Filter variables present to see which variables need to be added
  VariablesPresent <- VariablesPresent[!VariablesPresent %in% c("Position")]

  #### Create posfile ####
  posFile <- data.frame(
    x = c(apt$Position[1,]),
    y = c(apt$Position[2,]),
    z = c(apt$Position[3,])
  )

  for(Var in seq(VariablesPresent)){
    VarName <- VariablesPresent[Var]
    posFile[[VarName]] = c(apt[[VarName]][1,])
  }

  return(posFile)
}
