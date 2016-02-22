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
              
#sapply(outlierlst,dim)

tmp <- lapply(outlierlst,function(x){
    if (dim(x)[1]>0)
    {
        write.csv(file=paste0(x$variable[1],"-outliers.csv"),row.names=F,x)
    }
})
