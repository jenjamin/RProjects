\name{findEdgeClustersConvex}
\alias{findEdgeClustersConvex}
\title{findEdgeClustersConvex}
\usage{
findEdgeClustersConvex(posFileName, clusterStatsFile, clusterIndexPosFile, SamplingFraction, NNDMultiplier)()
}
\description{
This function automatically detects which clusters intersect the edge of an atom probe analysis volume.

posFileName is the filepath to the .pos or .APT file that you want to apply the analysis to.

clusterStatsFile is the filepath to the Cluster Analysis .csv file that can be exported from APSuite/IVAS.

clusterIndexPosFile is the filepath to the .pos or .APT file *containing the positions of indexed cluster ions* that you want to apply the analysis to.

SamplingFraction is the fraction by which you would like to sample the dataset.  This can vary between 0 and 1.  Please note that, for low values, the generated alpha shape may not accurately reflect the shape of the actual dataset; this will lead to erroneous results.  If SamplingFraction is too high, the time to perform the algorithm may be prohibitively large.

NNDMultiplier is what NND multiplier should be used to calculate alpha

For more details on method used, please see https://doi.org/10.1016/j.matchar.2019.110078  - please note that, in the cited paper, the clusters are assumed to be cuboidal in shape.  In this function, a convex hull is fitted to each cluster.
}
\examples{

}
