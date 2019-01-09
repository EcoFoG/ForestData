setwd("C:/Scripts R Guyafor/MesureCorr")

##Connexion a la base de donnees
library("RODBC")
library(data.table)
Connex=odbcConnect(dsn="Guyafor")

##Selection des donnees (toutes les mesures de toutes les placettes ayant au moins 3 campagnes)
req<-"SELECT dbo.TtGuyaforShiny.i_arbre AS idtree, dbo.taMesure.Circonference / PI() AS dbh, dbo.TtGuyaforShiny.code_vivant, dbo.TtGuyaforShiny.campagne,
dbo.taMesure.idMesure FROM dbo.TtGuyaforShiny INNER JOIN dbo.taMesure ON dbo.TtGuyaforShiny.i_arbre = dbo.taMesure.idArbre INNER JOIN dbo.taCampagne 
ON dbo.taMesure.idCampagne = dbo.taCampagne.idCampagne AND dbo.TtGuyaforShiny.campagne = dbo.taCampagne.Annee INNER JOIN dbo.ReSelecForetNbCampagnes 
ON dbo.TtGuyaforShiny.NomForet = dbo.ReSelecForetNbCampagnes.NomForet AND dbo.TtGuyaforShiny.n_parcelle = dbo.ReSelecForetNbCampagnes.n_parcelle
ORDER BY idtree, dbo.TtGuyaforShiny.campagne"

sqlQuery(Connex,req)->MesuresGuyafor

##Preparation des donnees pour l'analyse
source("correctionsV4.0.R")
data = data.table(MesuresGuyafor) ## open data as data.table (= data frame for fast calculation on large data)

## Correction des DBH sur les mesures d'arbres vivants
names(data)<-c("idtree","dbh","status","campagne","idMesure")
data <- data[order(campagne)]
dbh_corrige <- subset(data, status==1)[,.(campagne, idMesure, mega_correction(dbh, campagne, status)), by=.(idtree)]

##Mise en forme des donnees (mega_correction sort un vecteur avec, pour chaque ligne d'entree, deux lignes de sortie : circ_corr et code_corr. 
##Il faut donc trier selon 2 critères puis séparer dbh_corrige en deux selon les lignes paires et impaires et reassembler les deux tableaux 
##en collant les deux colonnes pour avoir circ_corr dans l'une et code_corr dans l'autre. X sert a reintroduire les mesures d'arbres morts 
##dans le tableau final). 
dbh_corrige<-dbh_corrige[order(dbh_corrige[,4],decreasing=F)]
dbh_corrige<-dbh_corrige[order(dbh_corrige[,3],decreasing=F)]
dbh_corrige1 = dbh_corrige[seq(0, nrow(dbh_corrige), 2),]
dbh_corrige2 = dbh_corrige[seq(1, nrow(dbh_corrige), 2),]
dbh_corrige = merge(dbh_corrige1, dbh_corrige2, by=c("idtree","campagne","idMesure"))
X<-cbind.data.frame(subset(data, status==0)[,c("idtree","campagne","idMesure","dbh")], rep(0,dim(subset(data, status==0))[1]))
names(X)<-c("idtree","campagne","idMesure","V3.x","V3.y")
dbh_corrige<-rbind(dbh_corrige,X)
dbh_corrige$V3.x<-dbh_corrige$V3.x*pi
dbh_corrige<-dbh_corrige[,c(3:5)]
colnames(dbh_corrige) = c("idMesure","circ_corr","code_corr")

##Corrections complementaires
######Corrections des Bugu Bugu, bois cathedrale (les corrections se font sur les Circonf_corr)
Req2<-"SELECT idMesure FROM dbo.TtGuyaforShiny WHERE (n_essence = 631)"
sqlQuery(Connex,Req2)->TousBuguBugu
dbh_corrige$circ_corr[dbh_corrige$idMesure %in% TousBuguBugu$idMesure]<-0.53*dbh_corrige$circ_corr[dbh_corrige$idMesure %in% TousBuguBugu$idMesure]
paste(dbh_corrige$code_corr[dbh_corrige$idMesure %in% TousBuguBugu$idMesure],"+5")->dbh_corrige$code_corr[dbh_corrige$idMesure %in% TousBuguBugu$idMesure]

######Corrections des gros arbres mal conformes de la P16 de Paracou (Application d'un Dmax empirique : 80dbh pour les kimboto,
#maho cigare, diagidia, buguni, yaya marecage, supun udu; 50dbh pour les weti udu, lebi koko, yaya montagne, wapa riviere,
#weti koko ; 55Circonf pour les patawa)

############Lot 1
Req3<-"SELECT idMesure FROM dbo.TtGuyaforShiny
       WHERE (n_essence = 402) AND (circonf = 888) OR (n_essence = 308) AND (circonf = 888) OR (n_essence = 210) AND (circonf = 888) OR (n_essence = 620) 
       AND (circonf = 888) OR (n_essence = 316) AND (circonf = 888) OR (n_essence = 669) AND (circonf = 888)"
sqlQuery(Connex,Req3)->Arbres888Lot1
80*pi->dbh_corrige$circ_corr[dbh_corrige$idMesure %in% Arbres888Lot1$idMesure]
paste(dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888Lot1$idMesure],"+6")->dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888Lot1$idMesure]

############Lot 2
Req4<-"SELECT idMesure FROM dbo.TtGuyaforShiny WHERE (n_essence = 651) AND (circonf = 888)
OR (n_essence = 705) AND (circonf = 888) OR (n_essence = 658) AND (circonf = 888)
OR (n_essence = 225) AND (circonf = 888) OR (n_essence = 707) AND (circonf = 888)"
sqlQuery(Connex,Req4)->Arbres888Lot2
50*pi->dbh_corrige$circ_corr[dbh_corrige$idMesure %in% Arbres888Lot2$idMesure]
paste(dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888Lot2$idMesure],"+6")->dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888Lot2$idMesure]

######Corrections du Bugu Bugu circonf=888
Req5<-"SELECT idMesure, nomPilote FROM dbo.TtGuyaforShiny WHERE (circonf = 888) AND (n_essence = 631)"
sqlQuery(Connex,Req5)->Arbres888BuguBugu
80*pi*0.53->dbh_corrige$circ_corr[dbh_corrige$idMesure %in% Arbres888BuguBugu$idMesure]
paste(dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888BuguBugu$idMesure],"+6")->dbh_corrige$code_corr[dbh_corrige$idMesure %in% Arbres888BuguBugu$idMesure]

##Expedition des donnees vers Guyafor sur Tovomita, SQLServer
sqlClear(channel=Connex, "dbo.taMesure_Corr", errors = TRUE)
sqlSave(channel= Connex, dat= dbh_corrige, tablename = "dbo.taMesure_Corr", append = TRUE, rownames = FALSE, colnames = FALSE, verbose = FALSE, safer = FALSE, addPK = FALSE, fast = TRUE, test = FALSE, nastring = NULL)####

##Cloture de la connection odbc
odbcClose(Connex)
