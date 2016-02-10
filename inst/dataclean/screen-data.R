##
##
library(unpakathon)
phenos <- unique(phenolong$variable)
outliertbl <- do.call(rbind,lapply(phenos,function(x)
                                   {
                                       findOutlier(phenolong,x,0.01)
                                   })
                      )
