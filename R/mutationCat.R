#' @name mutationCat
#' @title takes a vector of accession numbers and returns a vector of mut. categories
#' @param accession vector of accession numbers
#' @param catnames vector of length 3 for the names of control, mutant, and phytometer, respectively
#' @export
mutationCat <- function(accession,catnames=c("Columbia","TDNA-mutant","Phytometer"))
{
    col <- grepl("70000|60000",accession)
    salk <- grepl("SALK",accession)
    salk <- salk & !col
    phyt <- grepl("CS",accession)
    catn <- ifelse(col,catnames[1],ifelse(salk,catnames[2],catnames[3]))
    data.frame(mutcat=catn,phyt,salk,col)
}
