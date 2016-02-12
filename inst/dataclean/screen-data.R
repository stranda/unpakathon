##
##
library(unpakathon)
phenos <- unique(phenolong$variable)
#phenos <- phenos[!(phenos%in%"germinants.41d")]
outlierlst <- lapply(phenos,function(x)
                     {
                         print(x)
                         findOutlier(phenolong,x,0.01)
                     })
              
sapply(outlierlst,dim)
