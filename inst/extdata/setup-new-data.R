                                        #
                                        # run to create files in the 'data' subdir. run when db changes
                                        #
                                        # should result in
                                        # a file called 'phenowide'
                                        # a file called 'phenolong'
                                        # and several others

library(reshape)
library(dplyr)
library(unpakR)

dbInfo = read.table('../../../dbInfo.txt',as.is=T)

con <- dbConnect(MySQL(),dbname=dbInfo[,1],user=dbInfo[,2],password=dbInfo[,3])
query <- paste("SELECT Pl.idIndividualPlant, Pl.Accession_idAccession, Pl.plantnum_experiment,T.name, E.name, E.meta, F.Name,",
               " Ph.name, O.value, O.block",
               " FROM Observation O",
               " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
               " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
               " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
               " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
               " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility;",
               sep="")
phenolong <- dbGetQuery(con,query)

frc <- sapply(strsplit(phenolong$plantnum_experiment,"-"),function(x){x})

frc <- as.data.frame(t(sapply(strsplit(phenolong$plantnum_experiment,"-"),function(x){if (length(x)>0) c(x[1],x[2],x[3]) else NULL})))
names(frc) <- c("flat","row","column")
frc$flat <- as.numeric(frc$flat)
frc$column <- as.numeric(frc$column)

phenolong  <- cbind(phenolong,frc)
expts <- dbGetQuery(con,"SELECT * FROM Experiment;")

#head(phenolong)
dbDisconnect(con)


phenolong <-
    phenolong[,!names(phenolong)%in%c("Treatment","Facility","Experiment","plantnum_experiment")]
names(phenolong) <- c("plantID","accession","treatment","experiment","meta.experiment","facility","variable","value","block","flat","row","col")

phenowide <- cast(phenolong,fun.aggregate=mean)
#phenolong$phenotype <- phenolong$variable
#phenolong <- phenolong[,!names(phenolong)%in%"variable"]

unpak_db <- src_mysql(dbname="unpak",host = "localhost", user = dbInfo[,2], password = dbInfo[,3])  #this needs improvement for security

independent <- collect(tbl(unpak_db,"IndependentDataCache"))
independent$accession <- independent$Accession_idAccession
independent$locus <- independent$Gene_idGene
tdna <- collect(tbl(unpak_db,"TDNARatio"))
tdna$accession <- tdna$Accession_idAccession
geneont <- collect(tbl(unpak_db,"GeneOntology"),n=Inf)
geneont$locus <- geneont$Gene_idGene
ga <- collect(tbl(unpak_db,"GeneAccession"))

phenolong <- left_join(phenolong,ga[,!names(ga)%in%c("idGeneAccession")],by=c("accession"="Accession_idAccession"))
phenowide <- left_join(phenowide,ga[,!names(ga)%in%c("idGeneAccession")],by=c("accession"="Accession_idAccession"))

geneaccession=ga
print(dim(geneaccession))


datestamp <- data.frame(snapshot.date=date())
save(file="../../data/independent.rda",independent)
save(file="../../data/tdna.rda",tdna)
save(file="../../data/phenowide.rda",phenowide)
save(file="../../data/phenolong.rda",phenolong)
save(file="../../data/geneont.rda",geneont)
save(file="../../data/datestamp.rda",datestamp)
print("saving geneaccession")
save(file="../../data/geneaccession.rda",geneaccession)
