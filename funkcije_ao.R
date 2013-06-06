# Functions made for Network analysis class project.
# 
# Roman Luštrik, 5.6.2013

#' Draws a subset of a data.frame as a network graph.
#' @param x data.frame. Has at least three columns, from, to and relation.
#' @param relation Character vector of length one. Rows with what relation from column relation (see x) should be used?
#' @param pkg Character. Package name to be subsetted.
#' @param ... Arguments passed to plot function that plots igraph object.
#' @return \code{igraph} object with the possible side effect of plotting the object.
#' @author Roman Luštrik

plotGraph <- function(x, relation = NULL, pkg.to = NULL, 
   pkg.from = NULL, plot = TRUE, ...) {
   
   require(RColorBrewer)
   # prepare the data to be plotted
   if (!is.null(relation)) {
      gd <- droplevels(x[x$relation %in% relation, ])
   } else {
      gd <- x
   }
   if (!is.null(pkg.to)) gd <- gd[gd$to %in% pkg.to, ]
   if (!is.null(pkg.from)) gd <- gd[gd$from %in% pkg.from, ]
   
   # make the data an igraph object and plot it (if specified)
   g <- graph.data.frame(gd, directed = TRUE)
   
   cc <- E(g)$relation
   if (any(unique(cc) == "Imports")) cc[cc == "Imports"] <- "#E41A1C"
   if (any(unique(cc) == "Suggests")) cc[which(cc == "Suggests")] <- "#984EA3"
   if (any(unique(cc) == "Enhances")) cc[which(cc == "Enhances")] <- "#4DAF4A"
   if (any(unique(cc) == "Depends")) cc[which(cc == "Depends")] <- "#377EB8"
   E(g)$color <- cc
   
   if (plot) plot(g, ...)
   # see ?igraph:::layout for possible layout options
   g
}

# high performance packages
hpp <- c("batch", "BatchExperiments", "BatchJobs", "bayesm", "bcp", 
   "biglars", "biglm", "bigmemory", "bigrf", "biopara", "bnlearn", 
   "caret", "cudaBayesreg", "data.table", "dclone", "doMC", "doMPI", 
   "doRedis", "doRNG", "doSNOW", "ff", "foreach", "fork", "GAMBoost", 
   "gcbd", "Geneland", "gputools", "GridR", "HadoopStreaming", "harvestr", 
   "HiPLARM", "inline", "latentnet", "lga", "magma", "mapReduce", 
   "Matching", "mchof", "multicore", "nws", "OpenCL", "orloca", 
   "partDSA", "pbdBASE", "pbdDEMO", "pbdDMAT", "pbdMPI", "pbdNCDF4", 
   "pbdSLAP", "peperr", "permGPU", "pmclust", "profr", "proftools", 
   "pvclust", "Rcpp", "Rdsm", "rgenoud", "RInside", "rJava", "rlecuyer", 
   "Rmpi", "rpud", "rredis", "rsprng", "scaleboot", "snow", "snowfall", 
   "snowFT", "speedglm", "sprint", "sqldf", "STAR", "tm", "varSelRF", 
   "WideLM", "xgrid")

# spatial packages
spp <- c("ade4", "adehabitat", "adehabitatHR", "adehabitatHS", "adehabitatLT", 
   "adehabitatMA", "ads", "akima", "ash", "aspace", "automap", "classInt", 
   "CompRandFld", "constrainedKriging", "cshapes", "DCluster", "deldir", 
   "DSpat", "ecespa", "fields", "FieldSim", "gdistance", "Geneland", 
   "GEOmap", "geomapdata", "geonames", "geoR", "geoRglm", "geosphere", 
   "geospt", "geospt", "GeoXp", "ggmap", "glmmBUGS", "gmt", "GriegSmith", 
   "gstat", "Guerry", "hdeco", "intamap", "landsat", "mapdata", 
   "mapproj", "maps", "maptools", "MarkedPointProcess", "MBA", "Metadata", 
   "micromap", "ModelMap", "ncdf", "ncf", "nlme", "OpenStreetMap", 
   "osmar", "pastecs", "PBSmapping", "PBSmodelling", "plotKML", 
   "psgp", "ramps", "RandomFields", "rangeMapper", "RArcInfo", "raster", 
   "rasterVis", "RColorBrewer", "regress", "rgdal", "rgeos", "RgoogleMaps", 
   "RPyGeo", "RSAGA", "RSurvey", "rworldmap", "sgeostat", "shapefiles", 
   "sp", "spacetime", "sparr", "spatcounts", "spatgraphs", "spatial", 
   "spatialCovariance", "SpatialExtremes", "spatialkernel", "spatialprobit", 
   "spatialsegregation", "spatstat", "spBayes", "spcosa", "spdep", 
   "spgrass6", "spgwr", "sphet", "splancs", "spsurvey", "Stem", 
   "tgp", "trip", "tripack", "tripEstimation", "UScensus2000blkgrp", 
   "UScensus2000cdp", "UScensus2000tract", "vardiag", "vec2dtransf", 
   "vegan")