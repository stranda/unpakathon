#
# functions to take a phenotype along with classifying information to create an adjusted phenotype
#  either by the phytometers or by the means in each growth chamber/greenhouse of all plants
#
## the "dat" dataframes each function takes are in long, or melted format.

#require(dplyr)

#' @name phytcorrect
#' @title Correct phenotypes by the mean of the phytometers in each growth chamber in each Experiment
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @export
phytcorrect <- function(dat, pheno, classifier, lineid="line") {

        dat <- dat[dat$variable %in% pheno,]
        dat <- dat[!is.na(dat$value),] #don't mess with NAs
        
        filter.cond <- paste0("grepl('CS',",lineid,")")
        select.cond <- paste0(c(classifier,"variable","value"))
        group.cond <-  paste0(c(classifier,"variable"))
 ### mean all phyts by classifiers
        phytmn <- filter_(dat,filter.cond)%>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="value"] <- "mean"
        if ("plantID" %in% names(phytmn)) {phytmn <- phytmn[,-grep("plantID",names(phytmn))]}

### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"variable","value"))
        adjdat <- left_join(dat,phytmn)
        adjdat$value <- adjdat$value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
    }

#this one adjusts the phenotype by the means of all plants in each
#growth chamber
#
#' @name allcorrect
#' @title description Correct phenotypes by the mean of all plants in each growth chamber in each Experiment
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @export
allcorrect <- function(dat, pheno, classifier, lineid) {

        dat <- dat[dat$variable %in% pheno,]
        dat <- dat[!is.na(dat$value),] #don't mess with NAs
        
        select.cond <- paste0(c(classifier,"variable","value"))
        group.cond <-  paste0(c(classifier,"variable"))
 ### mean all phyts by classifiers
        phytmn <- dat %>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="value"] <- "mean"  
    if ("plantID" %in% names(phytmn)) {phytmn <- phytmn[,-grep("plantID",names(phytmn))]}

    
### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"variable","value"))
        adjdat <- left_join(dat,phytmn)
        adjdat$value <- adjdat$value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
        
    }
