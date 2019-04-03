###
### use the existing dataframes allfeat and methData to create a new dataframe allmeth
### only run if ../../data/all_arab_features.rda  or ../../data/methylation-all-chrom.rda change
### takes forever (like all day) to run on a typical computer
### no doubt there are much faster algorithms, but we indend to run once in a blue moon

load("../../data/all_arab_features.rda")
load("../../data/salk_positions.rda")

noloc <- which(substr(SalkPos$locus,1,2)!="AT")
if (length(noloc)>0)
{
    locs <- sapply(noloc,function(l)
    {
        as.character(allfeat[(allfeat$chrom==gsub("Chr","",as.character(SalkPos$chrom[l]))) &
                             (allfeat$begin <= SalkPos$pos[l]) &
                             (allfeat$end >= SalkPos$pos[l]) &
                             (allfeat$feature=="gene"),"locus"][1])
    })
    noloc.found = noloc[!is.na(locs)]
    if (length(noloc.found)>0)
        {
            SalkPos$locus <- as.character(SalkPos$locus)
            SalkPos$locus[noloc.found] <- locs[!is.na(locs)]
            save(file="../../data/salk_positions.rda",SalkPos)
        }
}

library(parallel)

SalkPos$chrom <- gsub("Chr","",SalkPos$chrom)
SalkInsert <- allfeat
searchsalk <- t(apply(SalkInsert[,1:2],1,sort))
print("finished sorting")
SalkInsert$SALK_Line <- NA
SalkInsert$pos <- NA


newlines <- NULL

for (i in 1:dim(SalkPos)[1])
{
    p1 <- (searchsalk[,1]<=SalkPos$pos[i])
    p2 <-  (searchsalk[,2]>=SalkPos$pos[i])
    p3 <- (SalkInsert$chrom==SalkPos$chrom[i])
    feats <- which((p1*p2*p3)>0)
    if (length(feats)>0)
    {
        for (j in 1:length(feats))
            if (!is.na(SalkInsert$SALK_Line[feats[j]]))
            {
                tmp <- SalkInsert[feats[j],]
                tmp$SALK_Line <- SalkPos$SALK[i]
                tmp$pos <- SalkPos$pos[i]
                newlines <- rbind(newlines,tmp)
            } else
            {
                SalkInsert$SALK_Line[feats[j]] <- SalkPos$SALK[i]
                SalkInsert$pos[feats[j]] <- SalkPos$pos[i]
            }
    }
    if (!(i%%1000)) print (paste(round(100*i/dim(SalkPos)[1],2),"% done"))
}

SalkInsert <- rbind(SalkInsert,newlines)
SalkInsert <- SalkInsert[order(SalkInsert$chrom,SalkInsert$locus,SalkInsert$feature,SalkInsert$transcript,SalkInsert$order),]
rownames(SalkInsert) <- 1:dim(SalkInsert)[1]

save(file="../../data/SalkInsert.rda",SalkInsert)



