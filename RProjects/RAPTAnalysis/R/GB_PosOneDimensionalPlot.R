# Script for plotting variation of concentration or count with distance in .pos file
myTheme <- function(relSize = 16) {
  return(theme_classic(relSize))
}

OneDCountFunc <- function(PosFile, NumberOfBins, Direction){
  if(NumberOfBins < 0 | !NumberOfBins%%1==0){
    stop("Invalid NumberOfBins selected.  Must be integer and greater than 0.  Please reinitiate.\n")
  }
  if(!Direction %in% c("x","y","z")){
    stop("Invalid Direction inputted.  Must be 'x', 'y', 'z'.  Please reinitiate.\n")
  }

  NoiselessPos <- PosFile %>%
    filter(Ion != "Noise")

  QuantiledDF <- NoiselessPos %>%
    mutate(
      Quant =  ntile(NoiselessPos[,Direction], NumberOfBins),
      DirectionDistance = NoiselessPos[,Direction]
      )

  OneDCount <<- merge(
    QuantiledDF %>%
      group_by(Quant) %>%
      summarise(Distance = mean(DirectionDistance)) %>%
      ungroup(),
    QuantiledDF %>%
      group_by(Quant, Ion) %>%
      summarise(Ioncount = n()) %>%
      ungroup() %>%
      spread(Ion, Ioncount)) %>%
    select(-Quant)

  OneDCount[is.na(OneDCount)] <<- 0

  OneDConc <<- OneDCount %>%
    gather(Ion, Count, -Distance) %>%
    group_by(Distance) %>%
    mutate(Total = sum(Count),
           Concentration = 100 * Count/Total) %>%
    select(-Count, -Total) %>%
    spread(Ion, Concentration)

  # Create colour list from range file
  myColors <- c(paste0("#",
                       (RangeInfo %>%
                          select(Color, Ion) %>%
                          distinct())$Color),
                "#000000")
  names(myColors) <- c((RangeInfo %>%
                          select(Color, Ion) %>%
                          distinct())$Ion,
                       "Unranged")
  myColors <<- myColors # makes custom colours available for other functions

  print(ggplot(OneDCount %>%
           gather(Ion, Count, - Distance),
           aes(Distance, Count, color = Ion)) +
          geom_point(shape = 4,
                     stroke = 2) +
          geom_line(size = 2, alpha = 0.3) +
          scale_color_manual(values = myColors) +
          myTheme() +
          theme(aspect.ratio = 1) +
          labs(x = "Distance (nm)",
               y = "Count") +
          theme(legend.position = "bottom"))

  print(ggplot(OneDConc %>%
                 gather(Ion, Concentration, - Distance),
               aes(Distance, Concentration, color = Ion)) +
          geom_point(shape = 4,
                     stroke = 2) +
          geom_line(size = 2, alpha = 0.3) +
          ylim(0,100) +
          scale_color_manual(values = myColors) +
          myTheme() +
          theme(aspect.ratio = 1) +
          labs(x = "Distance (nm)",
               y = "Concentration (Ionic %)") +
          theme(legend.position = "bottom"))
  }

