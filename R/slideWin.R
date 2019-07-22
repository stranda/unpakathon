#' make a sliding window of gene density
#' @param chr chromosome number as a numeric value
#' @param sz size of sliding window
#' @export

slideWinGenes <- function(chr=1,sz=100000)
{
  vec <- c(allmeth %>% filter(as.numeric(gsub("Chr","",chrom))==chr,feature=="gene") %>% mutate(pos=(begin+end)/2) %>% select(pos) %>% arrange(pos))[[1]]

  strt=min(vec,na.rm=T)
  stp=max(vec,na.rm=T)
  wins = 1 + (stp - strt) %/% sz
  as.data.frame(do.call(rbind,lapply(0:(wins-1), function(w)
    {
      left = strt+(w*sz)
      right=left+sz
      ng <- length(vec[(vec>=left)&(vec<=right)])
      if (!(ng>0)) ng=0
      c(cent=(left+right)/2,num=ng)
  })))

  }

#' make a sliding window of methylation density
#' @param chr chromosome number as a numeric value
#' @param sz size of sliding window
#' @export

slideWinMeth <- function(chr=1,sz=100000)
{
  mp <- allmeth %>% filter(as.numeric(gsub("Chr","",chrom))==chr,feature=="gene") %>%
    mutate(pos=(begin+end)/2) %>% select(pos,methsites) %>% arrange(pos)
  vec <- unlist(mp$pos)
  strt=min(vec,na.rm=T)
  stp=max(vec,na.rm=T)
  wins = 1 + (stp - strt) %/% sz
  as.data.frame(do.call(rbind,lapply(0:(wins-1), function(w)
  {
    left = strt+(w*sz)
    right=left+sz
    tdf <- mp[(mp$pos>=left)&(mp$pos<=right),]
    c(cent=(left+right)/2,num=sum(tdf$methsites,na.rm=T))
  })))

}

