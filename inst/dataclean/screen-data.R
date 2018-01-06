##
##
library(unpakathon)
cut.prop <- 0.01  #this is the proportion of observations to clip from
                  #both the top and bottom of the distribution

phenos <- unique(phenolong$variable)
#phenos <- phenos[!(phenos%in%"germinants.41d")]
outlierlst <- lapply(phenos,function(x)
                     {
                         print(x)
                         df <- findOutlier(phenolong,x,cut.prop)
                         print(dim(df))
                         df
                     })
              
#sapply(outlierlst,dim)

tmp <- lapply(outlierlst,function(x){
    if (dim(x)[1]>0)
    {
        write.csv(file=paste0(x$variable[1],"-outliers.csv"),row.names=F,x)
    }
})
