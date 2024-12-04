# Function for generating dataframe required to plot time between evaporatin events vs depth plot from atom probe data and range file
# Returns dataframe of ion ionic % and depth in nm

TimeBetweenEvap_DepthCalc <- function(
    RangeFilePath,
    DataFilePath,
    BinSize,
    PulseFreq,
    DetectionEfficiency)
{

  # Check bin size is valid
  if(BinSize <= 0){
    stop("Invalid BinSize selected.  Must be greater than 0.  Please reinitiate.\n")
  }

  # Check PulseFreq is valid
  if(PulseFreq <= 0){
    stop("Invalid PulseFreq selected.  Must be greater than 0.  Please reinitiate.\n")
  }

  # Check DetectionEfficiency is valid
  if(DetectionEfficiency <= 0 | DetectionEfficiency > 1){
    stop("Invalid PulseFreq selected.  Must be greater than 0 and less than or equal to 1.  Please reinitiate.\n")
  }

  # Read .rrng file
  Ranges <- read_rrng(RangeFilePath)
  # Read .apt/.pos file
  Data <- readAPTData(DataFilePath)

  if(file_ext(DataFilePath) == "apt"){
    Data <- Data %>% mutate(m = Mass)
  }

  EvapTimeData <- Data %>%
    select(z, m, erate) %>%
    mutate(Depth = cut(z, seq(floor(min(Data$z)) - BinSize, ceiling(max(Data$z)) + BinSize, BinSize)),
           t_between_events = 1E6/(erate*PulseFreq*100*DetectionEfficiency)) %>%
    group_by(Depth) %>%
    summarise(Count = n(),
              t_between_events = mean(t_between_events)) %>%
    ungroup() %>%
    mutate(Range = as.character(Depth),
           a = str_sub(Range, 2, -2)) %>%
    separate(a, c("min", "max"), sep = ",") %>%
    mutate(min = as.numeric(min),
           max = as.numeric(max),
           AveDepth = (min + max)/2) %>%
    select(contains("+"), AveDepth, t_between_events)

  return(EvapTimeData)
}
