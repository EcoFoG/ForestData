rm(list = ls())
load("Paracou.RData")
library(tidyverse)
data = EcoFoG::Paracou2df()

corrected <- data %>%
  filter(CodeCorr != 0) %>%
  mutate(species = paste(Genus, Species, sep = "_")) %>%
  select(idTree,
         CensusYear,
         Circ,
         CircCorr,
         CodeCorr,
         species,
         Plot,
         CodeMeas,
         VernName) %>%
  reshape2::melt(id.vars = c("idTree","species","CodeCorr","CensusYear","CodeMeas","Plot","VernName"),
                 measure.vars = c("Circ","CircCorr"),
                 variable.name = "Ctype",
                 value.name = "C") %>%
  as.tibble

ids <- corrected %>% select(idTree) %>% unique %>% pull
for(i in ids){
  # cnt <- cnt+1
  # i <- outlyers_ids[cnt]
  # print(cnt)
  # i = ids[1]
  temp <- corrected %>%
    filter(idTree == i) %>%
    select(CensusYear,CodeCorr,species, idTree,C,Ctype,CodeMeas,Plot,VernName) %>%
    mutate(D = C/pi) %>% na.omit
  g1 <- temp %>%
    ggplot(aes(x =CensusYear, y =D, color = Ctype))+
    geom_point()+

    # geom_jitter()+
    ggtitle(label = paste0("Corrected and uncorrected diameter vs time for individual ",i),
            subtitle = paste0(paste(unique(c(temp$species,temp$VernName)), collapse = " / "),
                              ", correction codes: ",paste(unique(temp$CodeCorr),collapse = " / "),
                              ", measure codes: ",paste(unique(temp$CodeMeas),collapse = " / "),
                              ", Plot: ",paste(unique(temp$Plot),collapse = " / ")))+
    scale_x_continuous(breaks = 1984:2017)+
    xlab("Census year")+
    ylab("Diameter (cm)")+
    theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  Ctype),
                                                                         size = 3)
  g1
  ggsave(g1, filename = paste0("./corrected_size_trees/g_corrected_size_",i,".jpeg"),device = "jpeg")

}



library(data.table)

tab <- data %>%
  filter(CodeAlive == 1 &
           CodeCorr != 0) %>%
  mutate(CodeMeas = paste(idTree,CensusYear, sep = "_")) %>%
  select(idTree,Circ,CodeAlive,CensusYear,CodeMeas)
tab = data.table(tab) ## open data as data.table (= data frame for fast calculation on large data)

## Correction des DBH sur les mesures d'arbres vivants
# names(data)<-c("idtree","dbh","status","campagne","idMesure")
tab <- tab[order(CensusYear)]
tab$DBH <- tab$Circ/pi
dbh_corrige <- subset(tab, CodeAlive==1)[,.(CensusYear, CodeMeas, mega_correction(DBH, CensusYear, CodeAlive)), by=.(idTree)]
dbh_corrige<-dbh_corrige[order(dbh_corrige[,4],decreasing=F)]
dbh_corrige<-dbh_corrige[order(dbh_corrige[,3],decreasing=F)]
dbh_corrige1 = dbh_corrige[seq(0, nrow(dbh_corrige), 2),]
dbh_corrige2 = dbh_corrige[seq(1, nrow(dbh_corrige), 2),]
dbh_corrige = merge(dbh_corrige1, dbh_corrige2, by=c("idTree","CensusYear","CodeMeas"))
truc <- dbh_corrige %>%
  rename(MegaCorr = V3.x,
         CodeCorr2 = V3.y) %>%
  mutate(MegaCorr = MegaCorr*pi) %>%
  select(-CodeMeas) %>%
  as.tbl %>%
  right_join(
    data %>%
      filter(CodeCorr != 0 & CodeAlive == 1) %>%
      mutate(species = paste(Genus, Species, sep = "_")) %>%
      select(idTree,
             CensusYear,
             Circ,
             CircCorr,
             CodeCorr,
             species,
             Plot,
             CodeMeas,
             VernName)
  ) %>%
  reshape2::melt(id.vars = c("idTree","species","CodeCorr","CodeCorr2","CensusYear","CodeMeas","Plot","VernName"),
                 measure.vars = c("Circ","CircCorr","MegaCorr"),
                 variable.name = "Ctype",
                 value.name = "C") %>%
  as.tibble


nrow(truc)

codcor <- truc %>%
  select(CodeCorr)%>%
    rowwise %>%
  mutate(CodeCorr = strsplit(CodeCorr, split = " ")[[1]][1])
codcor %>% pull %>% table
truc$CodeCorr2 %>% table
(truc$CodeCorr2 == codcor) %>% sum()
truc
ids <- truc %>% select(idTree) %>% unique %>% pull
for(i in ids){
  # cnt <- cnt+1
  # i <- outlyers_ids[cnt]
  # print(cnt)
  # i = ids[1]
  temp <- truc %>%
    filter(idTree == i) %>%
    select(CensusYear,CodeCorr,species, idTree,C,Ctype,CodeMeas,Plot,VernName) %>%
    mutate(D = C/pi) %>% na.omit
  g1 <- temp %>%
    ggplot(aes(x =CensusYear, y =D, color = Ctype))+
    geom_point()+

    # geom_jitter()+
    ggtitle(label = paste0("Corrected and uncorrected diameter vs time for individual ",i),
            subtitle = paste0(paste(unique(c(temp$species,temp$VernName)), collapse = " / "),
                              ", correction codes: ",paste(unique(temp$CodeCorr),collapse = " / "),
                              ", measure codes: ",paste(unique(temp$CodeMeas),collapse = " / "),
                              ", Plot: ",paste(unique(temp$Plot),collapse = " / ")))+
    scale_x_continuous(breaks = 1984:2017)+
    xlab("Census year")+
    ylab("Diameter (cm)")+
    theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  Ctype),
                                                                         size = 3)
  # g1
  ggsave(g1, filename = paste0("./corrected_size_trees/compare3/g_corrected_size_",i,".jpeg"),device = "jpeg")

}
