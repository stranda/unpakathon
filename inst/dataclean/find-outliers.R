#' @name findOutlier
#' @title Identify Outliers
#' @param pl phenotype file (long)
#' @param pheno is a phenotype
#' @param q the quantile
#' @import dplyr
#' 
#' @export
findOutlier <- function(pl,pheno,q=0.01)
{
    tmpdf <- pl%>%filter(variable%in%pheno)
    lbound <- quantile(pl$value,q)
    hbound <- quantile(pl$value,1-q)
    small <- tmpdf%>%filter(value<lbound)
    if (dim(small)[1]>0) small$direction <- "low"
    large <- tmpdf%>%filter(value>hbound)
    if (dim(small)[1]>0) large$direction <- "high"
    rbind(small,large)
}
