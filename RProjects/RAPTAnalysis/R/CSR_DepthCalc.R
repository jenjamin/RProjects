# Function for generating dataframe required to make CSR plot from atom probe data and range file

CSR_DepthCalc <- function(
    RangeFilePath,
    DataFilePath,
    RunNumber,
    StableIsotopeMass,
    BinSize,
    ChargeStates
){
  # Check charge states valid
  if(!all(ChargeStates == floor(ChargeStates))){
    stop('Problem with inputted charge states. Are they all integers > 0?.\n')
  }
  if(length(ChargeStates) > 5){
    stop('More than 5 charge states selected. This is likely not physical.\n')
  }
  # Check bin size is valid
  if(BinSize <= 0){
    stop("Invalid BinSize selected.  Must be greater than 0.  Please reinitiate.\n")
  }

  # Read .rrng file
  Ranges <- read_rrng(RangeFilePath)

  ####  Selecting just ranges of interest from DataFile ####
  MassesOfInterest <- data.frame(ChargeState = ChargeStates) %>%
    mutate(Mass = StableIsotopeMass / ChargeState)

  IsotopeRanges <- data.frame()
  for (i in seq(nrow(MassesOfInterest))) {
    IsotopeRanges <- rbind(
      IsotopeRanges,
      Ranges %>%
        filter(MassesOfInterest$Mass[i] > Start &
                 MassesOfInterest$Mass[i] < End) %>%
        mutate(ChargeState = MassesOfInterest$ChargeState[i])
    )
  }

  IsotopeRanges <- IsotopeRanges %>%
    select(Start, End, Ion, ChargeState) %>%
    arrange(-Start)
  # Check all chosen charge states have valid range associated with them
  if(nrow(IsotopeRanges) != length(ChargeStates)){
    stop('Problem with inputted charge states. Not all of the charge states have a range associated with them.  Correct .rrng file or change the number of charge states specified.\n')
  }
  if(length( unique(IsotopeRanges$Ion)) != 1){
    warning('Some of the charge states are not of the same ion!  This may be due to peak overlaps but this should be checked. \n')
  }
  # Read .apt/.pos file
  Data <- readAPTData(DataFilePath)

  if(file_ext(DataFilePath) == "apt"){
      Data <- Data %>% mutate(m = Mass)
    }
  #### Getting data for charge state plots ####
  # Filter to only select mass ranges of charge states and label with ion
  CSRVarWithDepth <- data.frame()
  for (i in seq(nrow(MassesOfInterest))) {
    CSRVarWithDepth <- rbind(CSRVarWithDepth, Data %>%
                        filter(between(
                          m, IsotopeRanges$Start[i], IsotopeRanges$End[i]
                        )) %>%
                        mutate(ChargeState = IsotopeRanges$ChargeState[i]))
  }

  CSRVarWithDepth <- CSRVarWithDepth %>%
    select(z, m, ChargeState) %>%
    mutate(Depth = cut(z, seq(floor(min(Data$z)) - BinSize, ceiling(max(Data$z)) + BinSize, BinSize))) %>%
    group_by(Depth, ChargeState) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = ChargeState,
                names_glue = "{ChargeState}+",
                values_from = Count,
                values_fill = 0) %>%
    mutate(Range = as.character(Depth),
           a = str_sub(Range, 2, -2)) %>%
    separate(a, c("min", "max"), sep = ",") %>%
    mutate(min = as.numeric(min),
           max = as.numeric(max),
           AveDepth = (min + max)/2) %>%
    select(contains("+"), AveDepth)

  return(CSRVarWithDepth)
}
