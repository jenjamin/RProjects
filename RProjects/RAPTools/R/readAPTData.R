# Function for reading .pos and .apt files and returning as dataframe
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

readAPTData <- function(DataFilePath){
  # Check if file is .apt or .pos (or invalid)
  library(tools) #needed to get filename without extension
  if(file_ext(DataFilePath) !="pos" & file_ext(DataFilePath) !="apt"){
    cat("Error!! APT Data File is neither .pos or .apt \n")
    break()
  }

  if(file_ext(DataFilePath) =="pos"){
    cat("File type is",file_ext(DataFilePath)," \n")
    # readposR(DataFilePath)
    return(readposR(DataFilePath))
    cat("File correctly read and assigned to dataframe")
  }

  if(file_ext(DataFilePath) =="apt"){
    cat("File type is",file_ext(DataFilePath)," \n")
    # readaptR(DataFilePath)
    return(readaptR(DataFilePath))
    rm(APTFileHeader2,APTBranchesDict,APTSectionHeaderAuto,PARAPROBE_Transcoder2, envir = .GlobalEnv )
    cat("File correctly read and assigned to dataframe")
  }
}
