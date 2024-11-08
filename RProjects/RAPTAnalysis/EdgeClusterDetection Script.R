# Script to identifying edge clusters in APT datasets
# Find clusters on the edge of an alpha hull - fits cluster shape using convex hull
# posFileName = filepath to .pos or .apt file of whole dataset
# clusterStatsFile = filepath to .csv of cluster search output from IVAS/APSuite
# clusterIndexPosFile = filepath to .pos or .apt file of indexed solute clusters
# SamplingFraction  = How much one would like to sample the POS file by
# NNDMultiplier = What NND multiplier should be used to calculate alpha
# Returns a list of which cluster IDs are deemed to be on the edge of the dataile, AlphaHullShape so can be plotted for visualisation
# Saves Cluster Analysis CSV in same location as original file but with *only* NON-EDGE clusters
# Written by Ben Jenkins - Oct 2024
require(RAPTools)
require(RAPTAnalysis)
require(tidyverse)
require(geometry)
require(spatstat)
require(alphashape3d)

EdgeClusterOutput <- findEdgeClustersConvex(posFileName = "C:/Users/gc9307/OneDrive - UK Atomic Energy Authority/Documents/Code/Edge-Cluster-Detection-master/Test Files/006_full.pos",
                       clusterIndexPosFile = "C:/Users/gc9307/OneDrive - UK Atomic Energy Authority/Documents/Code/Edge-Cluster-Detection-master/Test Files/006_full.cluster.indexed.pos",
                       clusterStatsFile = "C:/Users/gc9307/OneDrive - UK Atomic Energy Authority/Documents/Code/Edge-Cluster-Detection-master/Test Files/006_full - Top-Level ROI - Cluster Analysis (Ni, Cu).csv",
                       SamplingFraction = 0.005, # How much one would like to sample the POS file by
                       NNDMultiplier = 4 # What NND multiplier should be used to calculate alpha
)

# View Alpha shape that is generated
plot(
  EdgeClusterOutput$AlphaHullShape,
  indexAlpha = 1,
  walpha = F,
  transparency = 1,
  lit = T,
  col = c("#0014CE", "#FFBA00", "white")
) +
  bg3d(color="GREY") +
  light3d(theta=0, phi=15,
          x=NULL, y = NULL, z = NULL,
          viewpoint.rel = TRUE,
          ambient = "#FFFFFF",
          diffuse = "#FFFFFF",
          specular = "#FFFFFF")

