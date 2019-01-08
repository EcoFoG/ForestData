setwd("C:/Users/camille.piponiot/Google Drive/Data TmFO cleaned")

library(data.table)
library(stringdist)
library(tidyr)
library(BIOMASS)


##################################################
########     (1) OPEN AND FORMAT DATA     ########
##################################################

#### (a) itacoatiara ####
# Bloco B
itab <- read.csv2("data/Itacoatiara_UPAB.csv")
itab[, (grep("VOL|DAP", names(itab)))] <-
  as.numeric(apply(itab[, (grep("VOL|DAP", names(itab)))], 2, function(X) {
    gsub(("\\-|[[:alpha:]]|[[:space:]]|[[:blank:]]"),
         NA,
         as.numeric(X))
  }))
itab$idtree = paste("ita",
                    paste("B", itab$PARCELA, sep = ""),
                    itab$SUBPARC,
                    sep = "_",
                    rownames(itab))

tree_data = data.table(
  site = "ita",
  plot = paste("B", itab$PARCELA, sep = ""),
  subplot = itab$SUBPARC,
  idtree = itab$idtree,
  name =  itab$N..CIENT.,
  logged = (itab$CIF.2 == 8 & !is.na(itab$CIF.2)), 
  x = NA, y = NA
)

data = data.table(
  idtree = rep(itab$idtree, 4),
  dbh = as.numeric(as.character(
    c(itab$DAP.1, itab$DAP.2, itab$DAP.3, itab$DAP.4)
  )),
  year = rep(c(1996, 1998, 2001, 2014), each = nrow(itab))
)


# Bloco C
itac <- read.csv2("data/Itacoatiara_UPAC.csv")
itac[, (grep("VOL|DAP", names(itac)))] <-
  as.numeric(apply(itac[, (grep("VOL|DAP", names(itac)))], 2, function(X) {
    gsub(("\\-|[[:alpha:]]|[[:space:]]|[[:blank:]]"),
         NA,
         as.numeric(X))
  }))
itac$idtree <-
  paste("ita",
        paste("C", itac$PARCELAAV, sep = ""),
        itac$SUBPARCAV,
        rownames(itac),
        sep = "_")

tree_data = rbind(
  tree_data,
  data.table(
    site = "ita",
    plot = paste("C", itac$PARCELAAV, sep = ""),
    subplot = itac$SUBPARCAV,
    idtree = itac$idtree,
    name = itac$N..CIENT,
    logged = (itac$CIF.2 == 8 & !is.na(itac$CIF.2)), 
    x = itac$X, y = itac$Y
  )
)

data = rbind(data, data.table(
  idtree = rep(itac$idtree, 3),
  dbh = as.numeric(as.character(c(
    itac$DAP.1, itac$DAP.2, itac$DAP.3
  ))),
  year = rep(c(1997, 2001, 2014), each = nrow(itac))
))

# Bloco D
itad <- read.csv2("data/Itacoatiara_UPAD.csv")
itad[, (grep("VOL|DAP", names(itad)))] <-
  as.numeric(apply(itad[, (grep("VOL|DAP", names(itad)))], 2, function(X) {
    gsub(("\\-|[[:alpha:]]|[[:space:]]|[[:blank:]]"),
         NA,
         as.numeric(X))
  }))
itad$idtree <-
  paste("ita", paste("D", itad$PARCELAAV, sep = ""), rownames(itad), sep =
          "_")

tree_data = rbind(
  tree_data,
  data.table(
    site = "ita",
    plot = paste("D", itad$PARCELAAV, sep = ""),
    subplot = itad$SUBPARCAV,
    idtree = itad$idtree,
    name = itad$N..CIENT.,
    logged = (itad$CIF.2 == 8 & !is.na(itad$CIF.2)), 
    x = itad$X, y = itad$Y
  )
)

data = rbind(data, data.table(
  idtree = rep(itad$idtree, 3),
  dbh = as.numeric(as.character(c(
    itad$DAP.1, itad$DAP.2, itad$DAP.3
  ))),
  year = rep(c(1998, 2001, 2014), each = nrow(itad))
))
data$minDBH = 15

plot_data = data.table(unique(tree_data[, c("site", "plot","subplot"), with = FALSE]), treat =
                         "RIL")


## get vernacular names
itab_camila = read.csv("data/UPA_B_MIL_2014_camille.csv")
vernDT = unique(data.table(name = itab_camila$N..CIENT., vern = itab_camila$N..VULGAR))
itac_camila = read.csv("data/UPA_C_MIL_2014.csv")
vernDT = rbind(vernDT, unique(
  data.table(name = itac_camila$N..CIENT., vern = itac_camila$N..VULGAR)
))
itad_camila = read.csv("data/UPA_D_MIL_2014_camille.csv")
vernDT = unique(rbind(
  vernDT,
  data.table(name = itad_camila$N..CIENT., vern = itad_camila$N..VULGAR)
))
# clean species names
trim <- function (x)
  gsub("^\\s+|\\s+$", "", x)
simpl <-
  function(x) {
    y = unlist(strsplit(x, split = "\\s+|[[:punct:]]"))
    paste(y[1], y[2])
  }
tree_data$name = as.character(trim(as.character(tree_data$name)))
tree_data$name = sapply(tree_data$name, simpl)
vernDT$name = as.character(vernDT$name)
vernDT$name = sapply(trim(vernDT$name), simpl)
# typos in vern names -> more than 2 vernacular names for 1 species (but actually it's the same name) : keep only 1 of them
vernDT = vernDT[, .(last(vern)), .(name)]
colnames(vernDT)[2] = "vern"
tree_data = merge(tree_data, vernDT, by = "name", all.x = TRUE)



#### (b) Tabocal ####
tabocal <- read.csv2("data/tabocal.csv")
tabocal$idtree <-
  paste(
    "tbc",
    tabocal$Parcela,
    tabocal$Subparcela,
    paste(tabocal$N, tabocal$Cod_spp, sep = ""),
    sep = "_"
  )
LOGGED <- tabocal$idtree[tabocal$CIF == 6]
tabocal <- subset(tabocal, CIF < 5)
## remove duplicated measures
tabocal = tabocal[!duplicated(cbind(tabocal$idtree, tabocal$Ano)),]

## double names: trust last one
tab_tree = data.table(
  plot = tabocal$Parcela,
  subplot = tabocal$Subparcela,
  idtree = tabocal$idtree,
  name = paste(tabocal$Genero, tabocal$Especie),
  vern = tabocal$Nome.comum, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x , y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "tbc"

tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x","y"), with =
                                        FALSE])

data = rbind(data,
             data.table(
               idtree = tabocal$idtree,
               dbh = tabocal$DAP / 10,
               year = tabocal$Ano,
               minDBH=10
             ))

plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "CL"))

#### (c)  Cumaru ####
cumaru <- read.csv2("data/cumaru.csv")
cumaru$idtree <-
  paste("cum", cumaru$PP, cumaru$Sub_PP, cumaru$Num_Arv, sep = "_")
LOGGED <- cumaru$idtree[cumaru$CIF == 6]
cumaru <- subset(cumaru, CIF < 5)

tab_tree = data.table(
  plot = cumaru$PP,
  subplot = cumaru$Sub_PP,
  idtree = cumaru$idtree,
  name = cumaru$Nome.Cientifico,
  vern = cumaru$Nome.Comum, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "cum"

tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])

data = rbind(data,
             data.table(
               idtree = cumaru$idtree,
               dbh = cumaru$DAP..mm. / 10,
               year = cumaru$Ano,
               minDBH=10
             ))

plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "CL"))

#### (d) Ecosilva ####
## (no vern)
ecosilva <- read.csv2("data/data_CIKEL.csv")
ecosilva$idtree <-
  paste("eco", ecosilva$n_parcelle, ecosilva$i_arbre, sep = "_")
LOGGED <- ecosilva$idtree[ecosilva$code_mesure == 5]
ecosilva = subset(ecosilva, code_vivant == 1)

tab_tree = data.table(
  plot = ecosilva$n_parcelle,
  subplot = ecosilva$i_arbre,
  idtree = ecosilva$idtree,
  name = ecosilva$n_essence,
  vern = NA, 
  x = ecosilva$x, y = ecosilva$y
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "eco"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])

data = rbind(
  data,
  data.table(
    idtree = ecosilva$idtree,
    dbh = ecosilva$circonf / pi,
    year = ecosilva$campagne,
    minDBH = 20
  )
)
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "RIL"))

#### (e) Chico Bocao ####
chico <- read.csv2("data/chico_bocao.csv")
chico$idtree <-
  paste("chb", chico$Parc, chico$SubP, chico$N, sep = "_")
LOGGED <- chico$idtree[chico$CIF == 6]
chico <- subset(chico, CIF < 5)
## double names: trust last one
tab_tree = data.table(
  plot = chico$Parc,
  subplot = chico$SubP,
  idtree = chico$idtree,
  name = chico$Nome.Cientifico,
  vern = chico$Nome.comum, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "chb"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])

data = rbind(data,
             data.table(
               idtree = chico$idtree,
               dbh = chico$DAP / 10,
               year = chico$Ano,
               minDBH=10
             ))
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "CL"))


#### (f) Iracema ####
iracema <- data.table(read.csv2("data/Iracema_TMFO_20cm.csv"))
iracema$idtree <-
  paste(
    "ira",
    iracema$Parc,
    iracema$SubParc,
    paste(iracema$Ni, iracema$Cod_Sp, sep = ""),
    sep = "_"
  )
## remove duplicated idtrees/year (5 measures)
iracema = iracema[!duplicated(cbind(iracema$idtree, iracema$Ano))]
LOGGED <- iracema$idtree[iracema$CIF == 8]
iracema <- subset(iracema, CIF < 3)
iracema$DAP.cm.[iracema$idtree %in% LOGGED &
                  iracema$Ano > 2004] = NA
## double names: trust last one
tab_tree = data.table(
  plot = iracema$Parc,
  subplot = iracema$SubParc,
  idtree = iracema$idtree,
  name = iracema$Nome_Vulgar,
  vern = iracema$Nome_Vulgar, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "ira"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])

data = rbind(data,
             data.table(
               idtree = iracema$idtree,
               dbh = iracema$DAP.cm.,
               year = iracema$Ano,
               minDBH = 10
             ))
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "CL"))


#### (g) Peteco #### 
## (no vern)
peteco <- read.csv2("data/Peteco-Paragominas.csv")
peteco$idtree <-
  paste(
    "pet",
    peteco$p23_cdparcela,
    peteco$p23_cdsubparcela,
    peteco$p23_nrindividuo,
    peteco$p23_nrfuste,
    sep = "_"
  )
LOGGED <- peteco$idtree[peteco$p23_cdcif == 8]
peteco <- subset(peteco, p23_cdcif < 5)
peteco$species <-
  sapply(peteco$Especie, function(x)
    substr(x, regexpr("\\[", x)[1] + 1, regexpr("\\]", x)[1] - 1), simplify =
      T)
## double names: trust last one
tab_tree = data.table(
  plot = peteco$p23_cdparcela,
  subplot = peteco$p23_cdsubparcela,
  idtree = peteco$idtree,
  name = peteco$species,
  vern = NA, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "pet"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])
data = rbind(
  data,
  data.table(
    idtree = peteco$idtree,
    dbh = peteco$DAP.cm.,
    year = peteco$p23_cdmedicao,
    minDBH = 10
  )
)
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot","subplot"), with =
                                                          FALSE]), treat = "CL"))

#### (h) Tapajos ####
tapajos <-
  read.csv2("data/Dados_km114_Angela de Avila_Atualizado_Fev_2015.csv")
tapajos$idtree <-
  paste("tpj",
        tapajos$Plot,
        tapajos$Subplot,
        tapajos$Nind,
        tapajos$Nbole,
        sep = "_")
LOGGED <- tapajos$idtree[tapajos$Cif == 8 & tapajos$Year == 1983]
tapajos <- subset(tapajos, Cif %in% c(1:4, 11:16))
# remove duplicated measure
tapajos = tapajos[!duplicated(cbind(tapajos$Year, tapajos$idtree)),]
## double names: trust last one
tab_tree = data.table(
  plot = tapajos$Plot,
  subplot = tapajos$Subplot,
  idtree = tapajos$idtree,
  name = tapajos$Species1,
  vern = tapajos$VernName,
  treat = tapajos$Treat, 
  x = NA, y = NA
)
for(i in 0:4){tab_tree$treat = gsub(tab_tree$treat, pattern = i, replacement = c("ctrl","CL","silv","silv","silv+")[i+1])}
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y,treat)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "tpj"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])
data = rbind(data,
             data.table(
               idtree = tapajos$idtree,
               dbh = tapajos$dbhcm,
               year = tapajos$Year,
               minDBH = 5 
             ))
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot", "subplot","treat"), with =
                                                          FALSE])))


#### (i) Jari ####
jari <- read.csv2("data/Jari_Fernanda_MFT_31032014_2148.csv")
jari$idtree <-
  paste("jar", jari$plot, jari$sub, jari$nr_ind, jari$fuste, sep = "_")
LOGGED <- jari$idtree[jari$cif == 8]
jari <- subset(jari, cif %in% c(1:4, 11:16))
jari$treat = "ctrl"
jari$treat[jari$trat == "expl" & jari$trat_silv == 1] = "silv"
jari$treat[jari$trat == "expl" & jari$trat_silv == 0] = "CL"
## double names: trust last one
tab_tree = data.table(
  plot = jari$plot,
  subplot = jari$sub,
  idtree = jari$idtree,
  name = jari$name,
  vern = jari$nome.vernacular,
  treat = jari$treat, 
  x = NA, y = NA
)
tab_tree = tab_tree[, .(name = last(name), vern = last(vern)), .(plot, subplot, idtree, x, y, treat)]
tab_tree$logged = tab_tree$idtree %in% LOGGED
tab_tree$site = "jar"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x", "y"), with =
                                        FALSE])
data = rbind(data,
             data.table(
               idtree = jari$idtree,
               dbh = jari$DBH..cm.,
               year = jari$census,
               minDBH = 10
             ))
plot_data = rbind(plot_data, data.table(unique(tab_tree[, c("site", "plot", "subplot","treat"), with =
                                                          FALSE])))


### tree was measured with a ladder / POM was raised?
# for previous sites, we do not have this information
data$ladder = NA

#### (j) Braga Supay y Lobillo ####
lobillo = read.csv2("data/Base_Datos_Parcelas_1h_GINA.csv")
lobillo$idtree = paste("bsl", lobillo$Plot, lobillo$Sub, lobillo$ID, sep =
                         "_")
lobillo$D.2001 = as.numeric(gsub(pattern = ",", replacement = ".", lobillo$D.2001))
## remove 1998: almost no measurement
lobillo$D.1998 = NULL
# error
lobillo$D.1999[lobillo$idtree == "bsl_5_9_3540"] = lobillo$D.1999[lobillo$idtree ==
                                                                    "bsl_5_9_3540"] / 10
tree_data = rbind(
  tree_data,
  data.table(
    site = "bsl",
    plot = lobillo$Plot,
    subplot = lobillo$Sub,
    idtree = lobillo$idtree,
    name = lobillo$Especie,
    logged = NA,
    vern = NA, 
    x = lobillo$X, y = lobillo$Y
  )
)
data_lob = gather(lobillo,
                  grep("D.1|D.2", colnames(lobillo)),
                  key = "year",
                  value = "dbh")
data_lob$year = as.numeric(gsub(
  x = data_lob$year,
  pattern = "D.",
  replacement = ""
))
data_lob$minDBH=10
data_lob$ladder = NA
data_lob$ladder[data_lob$year %in% c(2001, 2008)] = 0
data_lob$ladder[(data_lob$year == 2001 &
                   !is.na(data_lob$POM.2001) &
                   data_lob$POM.2001 > 1300) |
                  (data_lob$year == 2008 &
                     !is.na(data_lob$POM.2008) &
                     data_lob$POM.2008 > 1300)] = 1
data = rbind(data, data.table(data_lob[, c("idtree", "dbh", "year", "ladder","minDBH")]))
plot_data_bsl = data.table(site = "bsl", unique(data_lob[,c("Plot","Sub")]), treat="CL")
colnames(plot_data_bsl) = c("site","plot","subplot","treat")
plot_data = rbind(plot_data, plot_data_bsl)


### (k) Paragominas ####
parago = read.csv2("data/paragominas_2014_copia_camila.csv", stringsAsFactors = FALSE)
parago$minDBH = 10
parago$minDBH[is.na(parago$sub)] = 25
parago$name = sapply(parago$NOMECIENTIFICO, function(x) {
  ul = unlist(strsplit(x, split = "\\s+"))[1:2]
  paste(ul, collapse = " ")
})
parago$idtree = paste("prg", parago$TRAT, parago$sub, parago$Thales, sep = "_")

# for now, let's consider that all commercial trees >50cmdbh that died during logging operations were harvested
parago$logged = !is.na(parago$COMERCIAL) &
  parago$COMERCIAL == 1 &
  !is.na(as.numeric(parago$DAP93)) &
  as.numeric(parago$DAP93) > 50 &
  parago$ANOMORTO == 1994 & parago$TRAT > 1

tree_data = rbind(
  tree_data,
  data.table(
    site = "prg",
    plot = parago$TRAT,
    subplot = parago$sub,
    idtree = parago$idtree,
    name = parago$name,
    logged = parago$logged,
    vern = parago$NOMEVULGAR, 
    x = NA,  y = NA #### a ameliorer
  )
)

data_prg = gather(parago,
                  key = year ,
                  value = dbh,
                  grep(x = colnames(parago), pattern = "DAP"))
data_prg$dbh = as.numeric(data_prg$dbh)
data_prg$year = as.numeric(gsub(data_prg$year, pattern = "DAP", replacement = ""))
data_prg$year[data_prg$year > 80 &
                data_prg$year < 100] = data_prg$year[data_prg$year > 80 &
                                                       data_prg$year < 100] + 1900
data_prg$year[data_prg$year < 80] = data_prg$year[data_prg$year < 80] + 2000
data_prg$ladder = NA
data_prg$dbh[data_prg$idtree=="prg_3_6_20414"&data_prg$year==2009] = 20.9
data_prg$sub[is.na(data_prg$sub)] = 0
plot_data_prg = data.frame(site="prg",unique(parago[,c("TRAT","sub")]))
data = rbind(data, data.table(data_prg[, c("idtree", "dbh", "year", "ladder","minDBH")]))

commercial_list = unique(parago$NOMECIENTIFICO[parago$COMERCIAL == 1])

plot_data = rbind(plot_data, data.table(
  site = "prg",
  plot = 1:3,
  subplot=1,
  treat = c("ctrl", "RIL", "CL")
))

#### (n) INPA ####
inpa <- read.csv2("data/BASE DATOS ACTUALIZADO 2014 INPA B2 B1-CAMILE.csv", stringsAsFactors = FALSE, encoding="latin1")
inpa$plot <- paste(inpa$Bloque,inpa$Trat, sep="")

# find subplot of each tree
inpa$subplot = inpa$Nº.PPM ## 1 ha subplots
inpa$subplot[is.na(inpa$subplot) & inpa$Subparcela%in% c("categoria \"B\"","categoria \"D\"")] = "B"
inpa$subplot[is.na(inpa$subplot) & inpa$Subparcela=="categoria \"A\""] = "A"
inpa$minDBH = 10; inpa$minDBH[inpa$subplot=="B"] = 20; inpa$minDBH[inpa$subplot=="A"] = 40; 
coords = inpa[,c("plot","subplot","X5","Y5")]
coords = data.table(subset(coords, !is.na(subplot)&!is.na(X5)))
# par(mfrow=c(2,2))
# sapply(unique(inpa$plot), function(p){
#   sub=subset(inpa, plot==p)
#   plot(sub$X5,sub$Y5, col=as.numeric(as.factor(sub$minDBH)), main=p)
# })
# define coords by plot
coords_subp = coords[,.(xmin=quantile(X5,c(0.005)),
                        xmax=quantile(X5,c(0.995)),
                        ymin=quantile(Y5,c(0.005)),
                        ymax=quantile(Y5,c(0.995))),.(plot,subplot)]
inpa$subplot[is.na(inpa$subplot)] <- sapply(which(is.na(inpa$subplot)), function(i){
  coord = c(inpa$X5[i], inpa$Y5[i]); if (is.na(coord[1])) coord = c(inpa$X6[i], inpa$Y6[i])
  subp = subset(coords_subp, plot==inpa$plot[i] & xmin<coord[1] & xmax>coord[1] & ymin<coord[2] & ymax>coord[2])
  sub = subp$subplot[!(subp$subplot %in% c("A","B"))]
  if (length(sub)==0)  {sub = subp$subplot[subp$subplot!="A"]}
  if (length(sub)==0) sub = "A"
  return(sub)
})
inpa$minDBH[inpa$subplot=="A"] = 40; inpa$minDBH[inpa$subplot=="B"] = 20; 
inpa$idtree <-
  paste("inp",
        inpa$plot,
        inpa$subplot,
        1:nrow(inpa),
        sep = "_")

# measurement year per bloc
years = data.frame(Bloque = inpa$Bloque, 
                   year = apply(inpa[,grep("Fecha", colnames(inpa))],2, function(X) as.numeric(tstrsplit(X," ")[[5]]) ))
years = gather(years, 2:7, key="n_measure", value = "year")
years = data.table(subset(years, !is.na(year)))
years$n_measure = as.numeric(as.factor(years$n_measure))
years = years[,.(year=min(year)),.(Bloque,n_measure)]
inpa = gather(inpa, key="n_measure",value="dbh", grep("Dap",colnames(inpa)) )
inpa$dbh = as.numeric(gsub(inpa$dbh,pattern=",",replacement="."))
inpa$n_measure = as.numeric(as.factor(inpa$n_measure))
# fill NAs in  years of measurement
inpa$year = NULL
inpa = merge(inpa, years, by=c("Bloque","n_measure"))

tab_tree = data.table(
  plot = inpa$plot,
  subplot = inpa$subplot,
  idtree = inpa$idtree,
  name = inpa$Nombre.cientifico,
  logged = (inpa$Categ1==4 & !is.na(inpa$Categ1) & inpa$dbh>35), ## see inpa$Categ -> categ = 4
  vern = inpa$Nombre.comun.5,
  t = as.numeric(inpa$year), 
  x = inpa$X5, y = inpa$Y5
)
tab_tree = tab_tree[, .(name = last(name), logged = first(logged), vern = last(vern), subplot = unique(subplot)), .(plot, idtree, x, y)]
tab_tree$site = "inp"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "x","y"), with =FALSE])

data = rbind(
  data,
  data.table(
    idtree = inpa$idtree,
    dbh = inpa$dbh,
    year = inpa$year,
    ladder = as.numeric(inpa$hmed>1.5), 
    minDBH = inpa$minDBH
  )
)
data = subset(data, !is.na(dbh))

plot_data_inp = unique(data.frame(site = "inp",
                                  plot=inpa$plot,
                                  plot.size=inpa$Superficie.Trat., 
                                  subplot = inpa$subplot))
plot_data_inp = subset(plot_data_inp, (plot.size==4&!(subplot%in%c("A","B")))|
                         (plot.size>5 & plot.size<16 & subplot=="B") |
                         (plot.size>16 & subplot=="A"))
plot_data_inp$treat = substr(plot_data_inp$plot,2,2)
for(i in 1:4){plot_data_inp$treat = gsub(plot_data_inp$treat, pattern = c("N","I","T","M")[i], replacement = c("CL","silv+","ctrl","silv")[i])}
plot_data_inp = subset(plot_data_inp, (plot.size==4&!subplot%in%c("A","B"))|(plot.size>4 & subplot %in% c("A","B")))

plot_data$plot.size = 1
plot_data$plot.size[plot_data$site=="prg"] = 24.5
plot_data$plot.size[plot_data$site%in%c("pet","tpj")] = 0.25
plot_data = rbind(plot_data, plot_data_inp[colnames(plot_data)])


#### (m) La Chonta ####
lachonta <- read.csv2("data/BASE DE DATOS ACTUALIZADO 2014 LA CHONTA B1 B2 B3 - CAMILE.csv",stringsAsFactors = FALSE,encoding = "latin1")
lachonta$plot <- paste(lachonta$Bloque,lachonta$Trat, sep="")

# find subplot of each tree
lachonta$subplot = lachonta$Nº.PPM ## 1 ha subplots
lachonta$subplot[is.na(lachonta$subplot) & lachonta$Subparcela%in% c("categoria \"B\"","categoria \"D\"")] = "B"
lachonta$subplot[is.na(lachonta$subplot) & lachonta$Subparcela=="categoria \"A\""] = "A"
lachonta$subplot[lachonta$subplot==49 & !is.na(lachonta$subplot)] <- NA
lachonta$minDBH = 10; lachonta$minDBH[lachonta$subplot=="B"] = 20; lachonta$minDBH[lachonta$subplot=="A"] = 40; 
nosp = subset(lachonta, is.na(subplot))
coords = lachonta[,c("plot","subplot","X5","Y5")]
coords[is.na(coords$X5),c("X5","Y5")] <- lachonta[is.na(coords$X5),c("X6","Y6")]
coords = data.table(subset(coords, !is.na(subplot)&!is.na(X5)))
# par(mfrow=c(2,2))
# sapply(unique(lachonta$plot), function(p){
#   sub=subset(lachonta, plot==p)
#   plot(sub$X5,sub$Y5, col=as.numeric(as.factor(sub$minDBH)), main=p)
# })
# define coords by plot
coords_subp = coords[,.(xmin=quantile(X5,c(0.005)),
                        xmax=quantile(X5,c(0.995)),
                        ymin=quantile(Y5,c(0.005)),
                        ymax=quantile(Y5,c(0.995))),.(plot,subplot)]
lachonta$subplot[is.na(lachonta$subplot)] <- sapply(which(is.na(lachonta$subplot)), function(i){
  coord = c(lachonta$X5[i], lachonta$Y5[i]); if (is.na(coord[1])) coord = c(lachonta$X6[i], lachonta$Y6[i])
  subp = subset(coords_subp, plot==lachonta$plot[i] & xmin<coord[1] & xmax>coord[1] & ymin<coord[2] & ymax>coord[2])
  sub = subp$subplot[!(subp$subplot %in% c("A","B"))]
  if (length(sub)==0)  {sub = subp$subplot[subp$subplot!="A"]}
  if (length(sub)==0) sub = "A"
  return(sub)
})
lachonta$minDBH[lachonta$subplot=="A"] = 40; lachonta$minDBH[lachonta$subplot=="B"] = 20; 
lachonta$idtree <-
  paste("lch",
        lachonta$plot,
        lachonta$subplot,
        1:nrow(lachonta),
        sep = "_")

# measurement year per bloc
years = data.frame(Bloque = lachonta$Bloque, 
                   year = apply(lachonta[,grep("Fecha", colnames(lachonta))],2, function(X) as.numeric(paste("20",tstrsplit(X,"-")[[3]], sep="")) ))
years = gather(years, 2:8, key="n_measure", value = "year")
years = data.table(subset(years, !is.na(year)))
years$n_measure = as.numeric(as.factor(years$n_measure))
years = years[,.(year=min(year)),.(Bloque,n_measure)]
lachonta = gather(lachonta, key="n_measure",value="dbh", grep("Dap",colnames(lachonta)) )
lachonta$dbh = as.numeric(gsub(lachonta$dbh,pattern=",",replacement="."))
lachonta$n_measure = as.numeric(as.factor(lachonta$n_measure))
# fill NAs in  years of measurement
lachonta$year = NULL
lachonta = merge(lachonta, years, by=c("Bloque","n_measure"))

tab_tree = data.table(
  plot = lachonta$plot,
  subplot = lachonta$subplot,
  idtree = lachonta$idtree,
  name = lachonta$Nombre.científico,
  logged = (lachonta$Categ2==4 & !is.na(lachonta$Categ1) & lachonta$dbh>35),
  vern = lachonta$Nombre.Comun.6,
  t = as.numeric(lachonta$year), 
  x = lachonta$X5, y = lachonta$Y5
)
tab_tree = tab_tree[, .(name=last(name), logged=first(logged), vern=last(vern), subplot = unique(subplot)), .(plot, idtree, x, y)]
tab_tree$site = "lch"
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern","x","y"), with =FALSE])

data = rbind(
  data,
  data.table(
    idtree = lachonta$idtree,
    dbh = lachonta$dbh,
    year = lachonta$year,
    ladder = as.numeric(lachonta$Hmed>1.5), 
    minDBH = lachonta$minDBH
  )
)
data = subset(data, !is.na(dbh))

plot_data_lch = unique(data.frame(site = "lch",
                                  plot=lachonta$plot,
                                  plot.size=lachonta$Sup...ha., 
                                  subplot = lachonta$subplot))
plot_data_lch = subset(plot_data_lch, (plot.size==4&!(subplot%in%c("A","B")))|
                         (plot.size>5 & plot.size<20 & subplot=="B") |
                         (plot.size>20 & subplot=="A"))
plot_data_lch$treat = substr(plot_data_lch$plot,2,2)
for(i in 1:4){plot_data_lch$treat = gsub(plot_data_lch$treat, pattern = c("N","I","T","M")[i], replacement = c("CL","silv+","ctrl","silv")[i])}

plot_data = rbind(plot_data, plot_data_lch[colnames(plot_data)])


#### (n) Paracou ####
paracou <- read.csv2("data/paracou.csv")
paracou$idtree <-
  paste("prc",
        paracou$n_parcelle,
        paracou$n_carre,
        paracou$i_arbre,
        sep = "_")
paracou$dbh <- paracou$circonf / pi
paracou <-
  subset(paracou,!campagne %in% c(1996, 1998, 2000, 2002, 2008, 2010, 2012))

tab_tree = data.table(
  plot = paracou$n_parcelle,
  subplot = paracou$n_carre,
  idtree = paracou$idtree,
  name = paste(paracou$Genre, paracou$espece),
  logged = (paracou$code_mesure == 4 &
              paracou$code_vivant == 0),
  vern = paracou$nomPilote,
  t = paracou$campagne, 
  xplot = paracou$x, yplot = paracou$y
)
tab_tree = tab_tree[, .(name=last(name), logged=logged[t == 1987], vern=last(vern)), .(plot, subplot, idtree, xplot, yplot)]
tab_tree$logged = tab_tree$logged & !is.na(tab_tree$logged)
tab_tree$site = "prc"

tree_data$xplot = tree_data$x; tree_data$x = NULL
tree_data$yplot = tree_data$y; tree_data$y = NULL

tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "xplot","yplot"), with = FALSE])
paracou = subset(paracou, code_vivant == 1)
data = rbind(
  data,
  data.table(
    idtree = paracou$idtree,
    dbh = paracou$dbh,
    year = paracou$campagne,
    ladder = paracou$code_vivant * (paracou$code_mesure %in% c(1:4, 10)), 
    minDBH = 10
  )
)

paracou$name = paste(paracou$Genre, paracou$espece)
commercial_list = unique(c(commercial_list, paracou$name[paracou$Commerciale ==
                                                           1]))
commercial_list_paracou = unique(paracou$name[paracou$Commerciale ==1]) 
save(commercial_list_paracou, file="C:/Users/camille.piponiot/Google Drive/volume/gfbi/data/list_commerciales.Rdata")

data = subset(data, !is.na(dbh))

plot_data$subplot = NA
plot_data_prc = data.table(
  site = "prc",
  plot = 1:18,
  plot.size = 6.25,
  treat = c(0:3, 2, 0, 1, 3, 1, 2, 0, 3, rep(0, 6))
)
subplot_prc = unique(data.frame(plot = paracou$n_parcelle, subplot = paracou$n_carre))
plot_data_prc = merge(plot_data_prc, subplot_prc, by="plot")
plot_data_prc$treat[plot_data_prc$treat == 0] = "ctrl"
plot_data_prc$treat[plot_data_prc$treat == 1] = "CL"
plot_data_prc$treat[plot_data_prc$treat ==2] = "silv"
plot_data_prc$treat[plot_data_prc$treat ==3] = "silv+"
plot_data_prc$plot.size = 6.25
plot_data = rbind(plot_data, plot_data_prc[,colnames(plot_data),with=FALSE])


#### (o) Tortue ####
tortue <- read.csv("data/data_Tortue_180228.csv", stringsAsFactors = FALSE)
tortue = subset(tortue, !(Plot %in% c("Eperon Barré","P17 complémentaire")))
tortue$plot <- as.numeric(as.factor(as.character((tortue$Plot))))
tortue$idtree <-
  paste("tor",
        tortue$plot,
        tortue$SubPlot,
        tortue$idTree,
        sep = "_")
tortue$dbh <- tortue$CircCorr / pi

tab_tree = data.table(
  plot = tortue$plot,
  subplot = tortue$SubPlot,
  idtree = tortue$idtree,
  name = paste(tortue$Genus, tortue$Species),
  logged = (tortue$CodeMeas == 4 &
              tortue$CodeAlive == 0),
  vern = tortue$VernName,
  t = tortue$CensusYear, 
  xplot = tortue$Xfield, yplot = tortue$Yfield, 
  xutm = tortue$Xutm, yutm = tortue$Yutm
)
# tortue$CodeAlive[tortue$CodeMeas==4] <- 1 ## logged trees were marked as dead the year before logging, so we put them as alive instead
tab_tree = tab_tree[, .(name=last(name), logged=logged[t == 2006], vern=last(vern)), .(plot, subplot, idtree, xplot, yplot, xutm, yutm)]
tab_tree$logged = tab_tree$logged & !is.na(tab_tree$logged)
tab_tree$site = "tor"

tree_data$xutm = NA; tree_data$yutm = NA
tree_data = rbind(tree_data, tab_tree[, c("site", "plot", "subplot", "idtree", "name", "logged", "vern", "xplot","yplot","xutm","yutm"), with = FALSE])
tortue = subset(tortue, CodeAlive == 1 )
data = rbind(
  data,
  data.table(
    idtree = tortue$idtree,
    dbh = tortue$dbh,
    year = tortue$CensusYear,
    ladder = tortue$CodeAlive * (tortue$CodeMeas %in% c(1:4, 10)), 
    minDBH = 10
  )
)

tortue$name = paste(tortue$Genus, tortue$Species)
tortue$treat <- "ctrl"
tortue$treat[grep("expl",tortue$Plot)] <- "RIL"
  
plot_data_tor = data.table(
  site = "tor",
  plot = tortue$plot,
  plot.size = tortue$PlotSurface,
  treat = tortue$treat
)
subplot_tor = unique(data.frame(plot = tortue$plot, subplot = tortue$SubPlot))
plot_data_tor = unique(merge(plot_data_tor, subplot_tor, by="plot"))
plot_data = rbind(plot_data, plot_data_tor[,colnames(plot_data),with=FALSE])

save(plot_data, file = "new_data/plot_data.Rdata")


data = subset(data, !is.na(dbh))

## keep only 'data' to free some space in the RAM ##
rm(list = setdiff(ls(), c(
  "data", "tree_data", "commercial_list", "plot_data"
)))

########################################################################################
#######################               CORRECT DATA               #######################
########################################################################################

############ (1) CORRECT SPECIES NAME AND GET WOOD DENSITY ############

### keep only genus/sp
trim <- function (x)
  gsub("^\\s+|\\s+$", "", x)
tree_data$name = trim(tree_data$name)
tree_data$genus <-
  tstrsplit(tree_data$name, split = "\\s+|[[:punct:]]")[[1]]
tree_data$species <-
  tstrsplit(tree_data$name, split = "\\s+|[[:punct:]]")[[2]]
tree_data$name_simpl <-
  paste(tree_data$Genus, tree_data$species, sep = " ")

########### (a) correct names ###########
names = data.frame(genus = tree_data$genus, species = tree_data$species)
names = unique(names)

# correctionTaxo = data.frame(cbind(names,correctTaxo(genus=names$genus, species=names$species)))
# save(correctionTaxo,file="correctTaxo/correctTaxo.Rdata")
load("correctTaxo/correctTaxo.Rdata")
# # # in case some new names are added: # # #
newnames = names[!(paste(names$genus, names$species) %in% paste(correctionTaxo$genus, correctionTaxo$species)),]
if (dim(newnames)[1]>0) {
  correctionTaxo2 = data.frame(cbind(newnames,correctTaxo(genus=newnames$genus, species=newnames$species)))
  correctionTaxo2$genusCorrected <- unlist(lapply(correctionTaxo2$genusCorrected, function(x) {if (length(x) == 0 ){return(NA)} else return(x)}))
  correctionTaxo = data.table(rbind(correctionTaxo,correctionTaxo2))
  save(correctionTaxo,file="correctTaxo/correctTaxo.Rdata")
}
correctionTaxo = data.table(correctionTaxo)

correctionTaxo$speciesCorrected[!is.na(correctionTaxo$speciesCorrected)&correctionTaxo$speciesCorrected=="niopiodes"] <- "niopoides"
correctionTaxo$nameModified[!is.na(correctionTaxo$speciesCorrected)&correctionTaxo$speciesCorrected=="niopiodes"] <- "TRUE"

correctionTaxo$speciesCorrected[!is.na(correctionTaxo$species)&correctionTaxo$nameModified=="SpNotFound"] <- NA
correctionTaxo$speciesCorrected[!is.na(correctionTaxo$species)&correctionTaxo$speciesCorrected=="sp"] <- NA

correctionTaxo$genusCorrected[!is.na(correctionTaxo$nameModified)&correctionTaxo$nameModified=="TaxaNotFound"] <- NA
correctionTaxo$speciesCorrected[!is.na(correctionTaxo$nameModified)&correctionTaxo$nameModified=="TaxaNotFound"] <- NA

correctionTaxo$genusCorrected[grepl("^[[:lower:]]", correctionTaxo$genus)] <- NA
correctionTaxo$speciesCorrected[grepl("^[[:lower:]]", correctionTaxo$genus)] <- NA

tree_data = merge(tree_data,
                  correctionTaxo,
                  by = c("genus", "species"),
                  all.x = TRUE)
save(tree_data, file = "new_data/tree_data.Rdata")

############ (b)  determine commercial species ############
## every logged species should be considered as commercial
commercial_list = unique(c(commercial_list, tree_data$name[tree_data$logged]))
commercial = data.table(name = trim(commercial_list))
commercial$genus = tstrsplit(commercial$name, split = "\\s+|[[:punct:]]")[[1]]
commercial$species = tstrsplit(commercial$name, split = "\\s+|[[:punct:]]")[[2]]
commercial$name = paste(commercial$genus, commercial$species)
commercial = merge(commercial,
                   correctionTaxo,
                   by = c("genus", "species"),
                   all.x = TRUE)
commercial = commercial[!is.na(commercial$speciesCorrected), c("genusCorrected", "speciesCorrected"), with =
                          FALSE]
colnames(commercial) = c("genus", "species")


# compilation des espèces exploitées dans toutes les parcelles + listes données par les PIs
list_commsp <- read.csv("data/List_comm.species_corrected.csv", stringsAsFactors = FALSE)
list_commsp = data.table(genus = list_commsp$genV, species = list_commsp$speV)
commercial = data.table(rbind(list_commsp, commercial))
commercial = unique(commercial)
save(commercial, file = "new_data/commercial_species_camila.Rdata")
# add ITTO list?


####################################################################################
#################          (2)   DBH CORRECTION                    #################
####################################################################################

#### (a) Limit to minDBH - 250 cm DBH ####
data$site = tstrsplit(data$idtree, split = "_")[[1]]
data$plot = tstrsplit(data$idtree, split = "_")[[2]]
# site_data = data.table(read.csv2("data/sites_clim_soil.csv", stringsAsFactors = FALSE))
# data = merge(data, site_data[, c("site", "minDBH"), with = FALSE], by =
#                "site")
data$minDBH = as.numeric(as.character(data$minDBH))
data = subset(data, dbh < 300 & dbh >= minDBH)
data$year = as.numeric(data$year)

#### (b) Check for missing data ####
source("functions/add_missing.R")
source("functions/tree_status.R")
missing = data[, .(add_missing(yr = year, id = idtree)), .(site, plot)]
missing$idtree = tstrsplit(missing$V1, split = "\\s+")[[1]]
missing$year = as.numeric(tstrsplit(missing$V1, split = "\\s+")[[2]])
missing$V1 = NULL
data = merge(data,
             missing,
             by = c("site", "plot", "idtree", "year"),
             all = TRUE)
status = data[, .(year, status=tree_status(dbh)), .(idtree)]
data = merge(data, status, by = c("idtree", "year"))
# remove status = 0 (dead trees) + not recruited yet// spot overgrown recruits??
data = subset(data, !is.na(status) & status == 1)
data = data[order(site, idtree, year)]


#### (c)  Correct dbh ####
source("functions/repl_missing.R")
source("functions/dbh_correction.R")
# first_census = data[,min(year),.(site,plot)]
# colnames(first_census)[3] = "first_census"
# data = merge(data, first_census,by=c("site","plot"))
## check for duplicates ##
data$key = paste(data$idtree, data$year)
setkey(data, key)
data = data[!duplicated(data)]
data$key = NULL
data = data[order(site, idtree, year)]
data_corr = data[, .(year = year,
                     dbh_c = dbh_correction(
                       X = dbh,
                       tm = year,
                       limit = minDBH,
                       ladder = ladder
                     )), by =
                   .(idtree)]
# colnames(data_corr) = c("idtree", "year", "dbh_c")
if (any(is.na(data_corr$dbh_c))) { 
  print(paste(sum(is.na(data_corr$dbh_c)), "corrected DBH have NA value."))
  data_corr = subset(data_corr,!is.na(data_corr$dbh_c))}
data = merge(data, data_corr, by = c("idtree", "year"))
data$dbh_c[data$dbh_c < data$minDBH & !is.na(data$dbh_c)] <- NA

## plot problematic trees and their correction
source("functions/plot_corr.R")
mod_id <-
  unique(subset(data, dbh != dbh_c &
                  !is.na(dbh_c) &
                  !is.na(dbh) & data$site == "tor")$idtree)
length(mod_id) # nb trees modified
par(mfrow = c(2,4), mar=c(1,1,3,1))
sapply(mod_id[!is.na(mod_id)], plot_corr)


#### (e) threshold values ####
## here : 50 cm (volume)
source("functions/threshold.R")
dt_thres = data[, .(year, threshold(X = dbh_c, tm = year, thres = 50)), by =
                  .(idtree)]
colnames(dt_thres)[3] = "dbh_c_50"
data = merge(data, dt_thres, by = c("idtree", "year"))


###### SAVE DATA #######
save(data, file = "new_data/dataDBH.Rdata")

