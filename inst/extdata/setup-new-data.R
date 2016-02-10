#
# run to create files in the 'data' subdir
#
# should result in
# a file called 'phenowide'
# a file called 'phenolong'
#
library(reshape)
library(dplyr)
library(unpakR)

dbInfo = read.table('../../../dbInfo.txt',as.is=T)

con <- dbConnect(MySQL(),dbname=dbInfo[,1],user=dbInfo[,2],password=dbInfo[,3])
query <- paste("SELECT Pl.idIndividualPlant, Pl.Accession_idAccession, T.name, E.name, E.meta, F.Name,",
               " Ph.name, O.value",
               " FROM Observation O",
               " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
               " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
               " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
               " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
               " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility;",
               sep="")
phenolong <- dbGetQuery(con,query)
#head(phenolong)
dbDisconnect(con)
phenolong <-
    phenolong[,!names(phenolong)%in%c("Treatment","Facility","Experiment")]
names(phenolong) <- c("plantID","accession","treatment","experiment","meta.experiment","facility","variable","value")
phenowide <- cast(phenolong,fun.aggregate=mean)
#phenolong$phenotype <- phenolong$variable
#phenolong <- phenolong[,!names(phenolong)%in%"variable"]

unpak_db <- src_mysql(dbname="unpak",host = "localhost", user = dbInfo[,2], password = dbInfo[,3])  #this needs improvement for security

independent <- collect(tbl(unpak_db,"IndependentDataCache"))
independent$accession <- independent$Accession_idAccession
tdna <- collect(tbl(unpak_db,"TDNARatio"))
tdna$accession <- tdna$Accession_idAccession
geneont <- collect(tbl(unpak_db,"GeneOntology"))
ga <- collect(tbl(unpak_db,"GeneAccession"))

phenolong <- left_join(phenolong,ga[,!names(ga)%in%c("idGeneAccession")],by=c("accession"="Accession_idAccession"))
phenowide <- left_join(phenowide,ga[,!names(ga)%in%c("idGeneAccession")],by=c("accession"="Accession_idAccession"))

save(file="../../data/independent.rda",independent)
save(file="../../data/tdna.rda",tdna)
save(file="../../data/phenowide.rda",phenowide)
save(file="../../data/phenolong.rda",phenolong)
save(file="../../data/geneont.rda",geneont)

