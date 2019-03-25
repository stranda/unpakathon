###
### use the existing dataframes allfeat and methData to create a new dataframe allmeth
### only run if ../../data/all_arab_features.rda  or ../../data/methylation-all-chrom.rda change
### takes forever (like all day) to run on a typical computer
### no doubt there are much faster algorithms, but we indend to run once in a blue moon

load("../../data/all_arab_features.rda")
load("../../data/methylation-all-chrom.rda")

library(parallel)



lst <- mclapply(as.character(unique(methData$chrom)),mc.cores=5,function(chrm)
{
    
    allmeth <- allfeat
    allmeth$chrom <- paste0("Chr",allmeth$chrom)
    allmeth <- allmeth[allmeth$chrom==chrm,]
    allmeth$methsites <- 0
    
    searchmeth <- t(apply(allmeth[,1:2],1,sort))
    
    print("finished sorting")
    
    md <- methData[methData$chrom==chrm,]
    
    for (i in 1:dim(md)[1])
    {
        p1 <- (searchmeth[,1]<=md$pos[i])
        p2 <-  (searchmeth[,2]>=md$pos[i])
        p3 <- (allmeth$chrom==md$chrom[i])
        feats <- which((p1*p2*p3)>0)
        if (length(feats)>0)
        {
            allmeth$methsites[feats] <- allmeth$methsites[feats] + 1
        }
        if (!(i%%1000)) print (paste(round(100*i/dim(md)[1],2),"% done for",chrm))
    }
    allmeth
})


allmeth <- do.call(rbind,lst)
save(file="../../data/methylation_by_feature.rda",allmeth)
