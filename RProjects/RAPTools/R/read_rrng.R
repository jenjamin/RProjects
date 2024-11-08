# Function for reading .rrng files and creating dataframe of values
require(tidyverse)
read_rrng <- function(RangeFilePath){

  #### Read rrng file and create R friendly version ####
  RangeFile <- read.delim(RangeFilePath, colClasses = "character", header = FALSE)
  RowsToSkip <- as.numeric(gsub("Number=", "", RangeFile[2,])) + 5
  Ranges <- RangeFile %>% slice(RowsToSkip:n())
  rm(RowsToSkip, RangeFile)

  RangesDF <- data.frame()

  i = 0
  for(i in unique(str_count(Ranges$V1, ":") - 2)){

    Elements <- c()
    for(j in seq(1,i,1)){
      Elements <- append(Elements,paste("Element",j))
    }

    ColumnNames <- c("Start", "End", "Volume",
                     Elements, "Color")

    Ranges %>%
      mutate(NumberIons = str_count(V1, ":") - 2) %>%
      filter(NumberIons == i) %>%
      separate(V1,
               ColumnNames,
               sep = " ")

    RangesDF <- bind_rows(RangesDF,
                          Ranges %>%
                            mutate(NumberIons = str_count(V1, ":") - 2) %>%
                            filter(NumberIons == i) %>%
                            separate(V1,
                                     ColumnNames,
                                     sep = " ")
    )

  }

  rm(ColumnNames, Elements,i, j)

  # Creating R-friendly range file #

  RangeInfo <- cbind(
    RangesDF %>%
      mutate(Start = as.numeric(str_extract(Start,"[^=]+$")),
             End = as.numeric(str_extract(End, "[^=]+$")),
             Volume = gsub("Vol:", "", Volume),
             Color = gsub("Color:", "", Color)) %>%
      select(Start, End, Volume, Color),
    RangesDF %>%
      select(contains("Element")) %>%
      unite("Ion") %>%
      mutate(Ion = paste(gsub("1|Name|:|NA|_| ","",Ion)))
  )
  RangeInfo$Ion <- gsub(",","", RangeInfo$Ion)

  rm(Ranges, RangesDF)
  return(RangeInfo)

}
