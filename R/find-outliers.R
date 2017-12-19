#' @name findOutlier
#' @title Identify Outliers
#' @param pl phenotype file (long)
#' @param pheno is a phenotype
#' @param q the quantile
#' @param z.score if true converts the phenotypes to z-scores scaled by the sd in their own growth-chamber
#' @import dplyr
#' 
#' @export
findOutlier <- function(pl,pheno,q=0.01,z.score=F)
{
    tmpdf <- pl%>%filter(variable%in%pheno)
    if (z.score) {
        tmpdf <- scalePhenos(classifier=c("treatment","facility","experiment"),dat=tmpdf,lineid="accession")
        tmpdf$value[!is.finite(tmpdf$value)] <- NA
        tmpdf$value <- tmpdf$value-mean(tmpdf$value,na.rm=T)
    }
 
    tmpdf$direction <- NA
    lbound <- quantile(tmpdf$value,q,na.rm=T)
    hbound <- quantile(tmpdf$value,1-q,na.rm=T)
    small <- tmpdf%>%filter(value<lbound)
    if (dim(small)[1]>0) small$direction <- "low"
    large <- tmpdf%>%filter(value>hbound)
    if (dim(large)[1]>0) large$direction <- "high"
    rbind(small,large)
}
