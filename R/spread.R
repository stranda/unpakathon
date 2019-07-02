#' @name spread
#' @title takes a long phenotype object (with plantIDs) and spreads it into wide format
#' @param dat the phenolong frame or a subset of phenolong with all the same columns and headers
#'  
#' @export
spread <- function(dat)
{
    cast(dat,fun.aggregate=mean)
}
