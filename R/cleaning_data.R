setwd("...")

library(data.table)
source("R/corrections.R")

data = as.data.table(DataGuyafor) ## open data as data.table (= data frame for fast calculation on large data)
data <- (data[NomForet == "Paracou" & n_parcelle == "1"])
# data <- data[1:100,]

# column names : t = time (year), idtree = ID of individual tree, dbh = DBH
# idtree <- unlist(data[, idTree, with=FALSE])
idtree <- unlist(data[, i_arbre, with=T])
# dbh <- unlist(data[, Measure, with=FALSE])
dbh <- unlist(data[, circonf, with=T])
# year <- format(data[, MeasureDate, with=FALSE],"%Y")
year <- format(data[, DateMesure, with=T],"%Y")
# statut <- data[, Status, with=FALSE]
statut <- data[, code_vivant, with=T]

# data[order(data$i_arbre,data$campagne),]

# Debug #
stupid <- function(x) return(x)

data[, .(stupid(dbh)), by=.(as.factor(idtree))]

data <- data %>%
  group_by(as.factor(i_arbre)) %>%
  mutate(status = tree.status(circonf))

# Debug #


# data$status <- tree.status(dbh)
data$status = data[,.(tree.status(dbh)), by=(as.factor(idtree))]$V1

## corrected DBH: dbh_c
data$dbh_c = data[,.(mega_correction(dbh, t, status)), by=.(as.factor(idtree))]$V1
threshold = 10 ## min DBH = 10 cm
data$dbh_c[data$dbh_c<threshold] = NA
data = subset(data, !is.na(dbh_c))

## checking changes
mod_id <- unique(data$idtree[data$dbh!=data$dbh_c & !is.na(data$dbh)])
length(mod_id) # nb of changed trees
par(mfrow=c(2,3))
# first 48 changed trees: blue = before correction, red = after correction
sapply(mod_id[!is.na(mod_id)][1:48], plot.corr)
