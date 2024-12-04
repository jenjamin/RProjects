# Script to identifying edge clusters in APT datasets
# Find clusters on the edge of an alpha hull - fits cluster shape using convex hull
# posFileName = filepath to .pos or .apt file of whole dataset
# clusterStatsFile = filepath to .csv of cluster search output from IVAS/APSuite
# clusterIndexPosFile = filepath to .pos or .apt file of indexed solute clusters
# SamplingFraction  = How much one would like to sample the POS file by
# NNDMultiplier = What NND multiplier should be used to calculate alpha
# Returns a list of which cluster IDs are deemed to be on the edge of the dataile, AlphaHullShape so can be plotted for visualisation
# Saves Cluster Analysis CSV in same location as original file but with *only* NON-EDGE clusters
# Written by Ben Jenkins and Andy London

#### Function for identifying edge clusters ####
findEdgeClustersConvex <- function (posFileName,
                                    clusterStatsFile,
                                    clusterIndexPosFile,
                                    SamplingFraction,
                                    NNDMultiplier) {
  # Check valid values selected for  SamplingFraction & NNDMultiplier
  if(NNDMultiplier < 0){
    stop("Invalid value selected for NNDMultiplier.  Please reinitiate.\n")
  }
  if(NNDMultiplier > 0 & NNDMultiplier > 0){
    warning("You have selected a NNDMultiplier value between 0 and 1.\nPlease consider whether this is likely to lead to more than 1 volume being created by ashape3d\n")
  }
  if(SamplingFraction < 0 | SamplingFraction > 1){
    stop("Invalid value selected for SamplingFraction.  Please reinitiate.\n")
  }
  # get time for timing execution
  StartTime <- Sys.time()

  #### Read the cluster statistics file ####
  #cluster file path
  ClusterImport <- read.csv(clusterStatsFile, skip = 10)

  #### Cluster Import ####
  ClusterImport <-
    ClusterImport %>% select(
      id=X,
      Solute.Ions,
      Ranged.Ions,
      Total.Ions,
      Center_x..nm..Ranged,
      Center_y..nm..Ranged,
      Center_z..nm..Ranged
    ) %>%
    mutate(
      xpos = Center_x..nm..Ranged,
      ypos = Center_y..nm..Ranged,
      zpos = Center_z..nm..Ranged
    ) %>%
    filter(grepl("Cluster", id))

  ClusterImportedData <- ClusterImport

  # load index cluster file to define cluster centres and boundary by convex hull
  cat("Reading indexed cluster data file \n")
  clrIndxData <- readAPTData(clusterIndexPosFile)
  nclustersDet <- max(clrIndxData[,4])
  clrHull_list <- vector("list", nclustersDet)
  for (i in 1:nclustersDet) {
    thisClr <- clrIndxData[clrIndxData[,4]==i,1:3]
    COM <- summarise_all(thisClr,"mean")
    tempList<-convhulln(thisClr)
    clrHull <- thisClr[unique(array(tempList)),1:3]
    clrHull$id <- i
    clrHull_list[[i]] <- clrHull
  }
  clrHull_all <- bind_rows(clrHull_list)


  #### Load filtered pos file to improve speed ####
  cat("Reading whole dataset data file \n")
  posFile <- readAPTData(posFileName)
  PosFraction <- floor(nrow(posFile) * SamplingFraction)
  FilterPosFile <- posFile %>% sample_n(PosFraction)
  #### Parameters For Calculating Alpha Value####
  AlphaValue <- NNDMultiplier*round(ceiling(100*(max(nndist(FilterPosFile %>% select(x,y,z), k=1)))),2)/100

  print(paste0(
    "Alpha Value: ",
    AlphaValue,
    " Sampling fraction: ",
    SamplingFraction
  ))

  #### Using alpha-shape 3d ####
  M <- as.matrix(FilterPosFile[, 1:3])

  # free some memory
  rm(FilterPosFile)
  gc()

  AlphaHullShape <- ashape3d(M, alpha =  c(AlphaValue, 10))

  #### Ensuring there is only one connected volume ####
  comp <- components_ashape3d(AlphaHullShape, indexAlpha = "all")
  NumberVolumes <- as.data.frame(table(comp[]))

  #### Determing which clusters are edge clusters and returning values ####
  if (nrow(NumberVolumes) != 1) {
    stop("Error. Multiple volumes created.\n")
  } else{
    print(paste0("Alpha value has created one volume"))

    # determining which clusters are edge clusters
    ClustersInHull <-
      inashape3d(AlphaHullShape, indexAlpha = 1, as.matrix(clrHull_all[,1:3]))

    # getting names of clusters that are edge
    ClusterLocation2 <- cbind(clrHull_all, ClustersInHull) %>%
      dplyr::filter(ClustersInHull==FALSE) %>%
      group_by(id) %>%
      summarise(ID=first(id))

    TotalEdgeClusters <- sort(as.numeric(as.matrix(ClusterLocation2[,1])))
  }


  print(paste0(
    "Total number of clusters detected: ",
    length(TotalEdgeClusters)
  ))
  NumberOfAssignedEdgeClusters <- length(TotalEdgeClusters)
  # Print total exe time
  EndTime <- Sys.time()
  TotalTime <- EndTime - StartTime
  print(TotalTime)

  EdgeClusters <- TotalEdgeClusters
  NumberClusters <- max(as.numeric(gsub("Cluster ","",ClusterImportedData$id)))
  #return(list(TotalEdgeClusters=TotalEdgeClusters, ClusterImportedData=ClusterImportedData))

  #### Section to save just non-edge clusters from csv ####
  write.csv(read.csv(clusterStatsFile, skip = 10) %>%
              filter(X != "Matrix") %>% # Remove matrix as not a cluster
              mutate(ClusterID = parse_number(X)) %>% # Create column with cluster number
              filter(!ClusterID %in% as.vector(EdgeClusters)), # Select only clusters that are *not* edge
            file = paste0(dirname(clusterStatsFile),"\\Non-Edge_ClusterResults",basename(posFileName),".csv"),
            row.names = F)

  # ListToReturn <- list(
  #   "EdgeClusterList" = EdgeClusters,
  #   "AlphaHullShape" = AlphaHullShape
  # )

  return(list(
    "EdgeClusterList" = EdgeClusters,
    "AlphaHullShape" = AlphaHullShape
  ))
}

