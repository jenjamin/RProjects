# Function for generating dataframe required to make hydrogen isotope depth plot from atom probe data and range file
# Returns dataframe of ion ionic % and depth in nm

HIsotope_DepthCalc <- function(
    RangeFilePath,
    DataFilePath,
    BinSize)
{

  # Check bin size is valid
  if(BinSize <= 0){
    stop("Invalid BinSize selected.  Must be greater than 0.  Please reinitiate.\n")
  }
  # Read .rrng file
  Ranges <- read_rrng(RangeFilePath)
  # Read .apt/.pos file
  Data <- readAPTData(DataFilePath)

  if(file_ext(DataFilePath) == "apt"){
    Data <- Data %>% mutate(m = Mass)
  }

  #### Extracting relevant data for hydrogen plots####
  HydrogenRanges <- # assume hydrogen isotopes only present below 5 Da
    cbind(Ranges %>%
            filter(Start < 5) %>%
            select(Start,End,Ion)
    )

  HydrogenIsotopesDepth <- data.frame()
  for (i in seq(nrow(HydrogenRanges))) {
    HydrogenIsotopesDepth <- rbind(HydrogenIsotopesDepth,
                                   Data %>%
                                     filter(between(m, HydrogenRanges$Start[i], HydrogenRanges$End[i])) %>%
                                     mutate(Ion = HydrogenRanges$Ion[i]))
  }

  HydrogenIsotopesDepth <-   merge(
    Data %>% # need to merge to get total ion count from all ions and then just counts of H isotopes
      mutate(Depth = cut(
        z, seq(floor(min(Data$z)) - BinSize, ceiling(max(Data$z)) + BinSize, BinSize)
      )) %>%
      group_by(Depth) %>%
      summarise(TotalNumberIons = n()),

    HydrogenIsotopesDepth %>%
      mutate(Depth = cut(
        z, seq(floor(min(Data$z)) - BinSize, ceiling(max(Data$z)) + BinSize, BinSize)
      )) %>%
        group_by(Depth, Ion) %>%
        summarise(TotalNumberIons = n()) %>%
        ungroup() %>%
        pivot_wider(
          names_from = Ion,
          values_from = TotalNumberIons,
          values_fill = 0
        )) %>%
      mutate(Range = as.character(Depth),
           a = str_sub(Range, 2, -2)) %>%
    separate(a, c("min", "max"), sep = ",") %>%
    mutate(min = as.numeric(min),
           max = as.numeric(max),
           AveDepth = (min + max)/2) %>%
    select(-Depth, -Range, -min, -max) %>%
    mutate(across(-c(TotalNumberIons,AveDepth), ~.x*100/TotalNumberIons)) # need to get concs normalised to original TotalNumber

  return(HydrogenIsotopesDepth)
}
