#
# run to create a table of parent ids from experiments past
#
# should result in
# a file called 'parent-table.csv'
#
library(dplyr)
library(unpakR)

dbInfo = read.table('../../../dbInfo.txt',as.is=T)

con <- dbConnect(MySQL(),dbname=dbInfo[,1],user=dbInfo[,2],password=dbInfo[,3])
query <- paste("SELECT Pl.idIndividualPlant, Pl.Accession_idAccession, Pl.plantnum_experiment, Pl.parent_experiment, Pl.Parent1, Pl.Parent2, T.name, E.name, E.meta, F.Name,",
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
names(phenolong) <- c("plantID","accession","plantnum_experiment","parent_experiment","Parent1","Parent2","treatment","experiment","meta.experiment","facility","variable","value")

parentids <- unique(phenolong[,c("plantID","accession","plantnum_experiment","parent_experiment","Parent1","Parent2","treatment","experiment","meta.experiment","facility")])

write.csv(file="parent-table.csv",row.names=F,parentids)

