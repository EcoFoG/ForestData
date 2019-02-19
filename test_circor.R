rm(list= ls())
library(tidyverse)
library(ForestData)
data <- EcoFoG::Paracou2df()
ids <- data %>% filter(CodeCorr != 0) %>% select(idTree) %>% unique %>% pull
test <- data %>%
  # filter(Plot == 6 & SubPlot == 1 & CensusYear < 2004) %>%
  filter(idTree %in% ids & Plot %in% 1:16) %>%
  select(idTree,
         Genus,
         Species,
         Plot,
         SubPlot,
         Circ,
         CodeAlive,
         CensusYear,
         CodeMeas)


# %>%
  # mutate(IDM = paste(CensusYear, idTree, sep = "_"))
# ?correct_alive
testcorr <- ForestData::correct_alive(test %>% mutate(Plotsub = paste(Plot, SubPlot, sep = "_")),
                                      id_col = "idTree",
                                      time_col = "CensusYear",
                                      status_col = "CodeAlive",
                                      plot_col = "Plotsub",
                                      byplot = TRUE,
                                      dead_confirmation_censuses = 2,
                                      use_size = FALSE) %>% reattribute_fixed_informations()


testcorr$CodeMeas <- ifelse(is.na(testcorr$CodeMeas),
                            0,
                            testcorr$CodeMeas)
testcorr <- testcorr %>%  reconstitute_pom_paracou()
test_circor <- correct_size(testcorr %>% filter(status_corr == 1))
idz <- test_circor %>% filter(code_corr != "0") %>% select(id) %>% unique %>% pull
test_circor %>% filter(id %in% idz) %>% View


# library(data.table)

# tab <- data %>%
  # filter(CodeAlive == 1 &
           # idTree %in% ids) %>%
  # mutate(CodeMeas = paste(idTree,CensusYear, sep = "_")) %>%
  # select(idTree,Circ,CodeAlive,CensusYear,CodeMeas)
# tab = data.table(tab) ## open data as data.table (= data frame for fast calculation on large data)

## Correction des DBH sur les mesures d'arbres vivants
# names(data)<-c("idtree","dbh","status","campagne","idMesure")
# tab <- tab[order(CensusYear)]
# tab$DBH <- tab$Circ/pi
# dbh_corrige <- subset(tab, CodeAlive==1)[,.(CensusYear, CodeMeas, mega_correction(DBH, CensusYear, CodeAlive)), by=.(idTree)]
# dbh_corrige<-dbh_corrige[order(dbh_corrige[,4],decreasing=F)]
# dbh_corrige<-dbh_corrige[order(dbh_corrige[,3],decreasing=F)]
# dbh_corrige1 = dbh_corrige[seq(0, nrow(dbh_corrige), 2),]
# dbh_corrige2 = dbh_corrige[seq(1, nrow(dbh_corrige), 2),]
# dbh_corrige = merge(dbh_corrige1, dbh_corrige2, by=c("idTree","CensusYear","CodeMeas"))
truc <- test_circor %>%
  rename(NEW = size_corr,
         CodeCorr2 = code_corr,
         idTree = id,
         CensusYear = time) %>%
  select(-CodeMeas, -CodeAlive) %>%
  arrange(idTree,CensusYear) %>%
  as.tbl %>%
  right_join(
    data %>%
      filter(CodeAlive == 1 & idTree %in% ids) %>%
      arrange(idTree,CensusYear) %>%
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
  )%>%
  reshape2::melt(id.vars = c("idTree","species","CodeCorr","CodeCorr2","CensusYear","CodeMeas","Plot","VernName"),
                 measure.vars = c("Circ","CircCorr","NEW"),
                 variable.name = "Ctype",
                 value.name = "C") %>%
  as.tibble

# truc <- dbh_corrige %>%
#   rename(MegaCorr = V3.x,
#          CodeCorr2 = V3.y) %>%
#   mutate(MegaCorr = MegaCorr*pi) %>%
#   select(-CodeMeas) %>%
#   as.tbl %>%

  # right_join(
  #   data %>%
  #     filter(CodeAlive == 1 & idTree %in% ids) %>%
  #     mutate(species = paste(Genus, Species, sep = "_")) %>%
  #     select(idTree,
  #            CensusYear,
  #            Circ,
  #            CircCorr,
  #            CodeCorr,
  #            species,
  #            Plot,
  #            CodeMeas,
  #            VernName)
  # ) %>%
  # reshape2::melt(id.vars = c("idTree","species","CodeCorr","CodeCorr2","CensusYear","CodeMeas","Plot","VernName"),
  #                measure.vars = c("Circ","CircCorr","MegaCorr"),
  #                variable.name = "Ctype",
  #                value.name = "C") %>%
  # as.tibble


# nrow(truc)

# codcor <- truc %>%
  # select(CodeCorr)%>%
  # rowwise %>%
  # mutate(CodeCorr = strsplit(CodeCorr, split = " ")[[1]][1])
# codcor %>% pull %>% table
# truc$CodeCorr2 %>% table
# # (truc$CodeCorr2 == codcor) %>% sum()
# truc
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
  ggsave(g1, filename = paste0("./test_circcorr/g_corrected_size_",i,".jpeg"),device = "jpeg")

}
getwd()

