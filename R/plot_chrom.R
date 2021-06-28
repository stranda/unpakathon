#' @name plotChrom
#' @title plot a phenotype along a chromosome
#' @param pheno is a phenotype
#' @param adjust is an integer that describes how phenotypes are adjusted. 0=raw, 1=Columbia mean, 2=Phytometer mean, 3=All plant mean
#' @param chromo integer 1-5 corresponding to Arabidopsis chromosome number
#' @param meta the meta experiment default  experiment 3.  Can be a vector
#' @param span amount of line smoothing.  smaller is wigglyier
#' @import dplyr
#' @import ggplot2
#'  
#' @export
plotChrom <- function(pheno,
                      adjust=c(0,1,2,3)[2],
                      chromo = 1,
                      meta=c("3"),
                      span=1,
                      lines=NULL
                      )
{
  if (is.null(lines)) lines <- unique(phenolong$accession) 
  pl <- filter(phenolong,meta.experiment %in% meta) %>% filter(variable == pheno[1]) %>% filter(accession %in% lines)
  if (adjust==0) adj = pl else
    if (adjust ==1) adj  = pl %>% adjustPhenotypes::colcorrect(classifier=c("experiment","facility"),pheno) %>% 
                                     adjustPhenotypes::scalePhenos(pheno=pheno,classifier=c("experiment","facility")) else
      if (adjust==2) adj  = pl %>% adjustPhenotypes::phytcorrect(classifier=c("experiment","facility"),pheno) %>% 
          adjustPhenotypes::scalePhenos(pheno=pheno,classifier=c("experiment","facility")) else
            adj  = pl %>% adjustPhenotypes::allcorrect(classifier=c("experiment","facility"),pheno) %>% 
              adjustPhenotypes::scalePhenos(pheno=pheno,classifier=c("experiment","facility"))
  
  linemeans <- group_by(adj,accession,variable) %>% summarize(mn=mean(value)) %>% 
    left_join(geneaccession) %>% mutate(SALK=accession)
 
  linemeans <- linemeans %>% left_join(SalkPos %>% mutate(chrom=as.numeric(gsub("Chr","",as.character(chrom))))) %>% filter(chrom==chromo)
 
  ggplot(linemeans,aes(x=pos,y=mn))+geom_point()+geom_smooth(span=span)+ylab(pheno)+xlab(paste("Chromosome",chromo,"position"))

 }
