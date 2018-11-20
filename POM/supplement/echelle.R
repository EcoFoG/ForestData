data_ladder <- read.csv("R_Gaelle_good.csv", sep = ";",dec=".",header=T,stringsAsFactors = F)

laddered_trees <- data_ladder %>%
  filter(Echelle == 1) %>%
  select(idTree) %>%
  unique %>%
  pull

laddered <- data_ladder %>%
  filter(idTree %in% laddered_trees) %>%
  rename(id = idTree) %>%
  mutate(ladder_tag = 0)

for(t in laddered_trees){
  tree <- laddered[which(id == t),]
  tree <- tree[order(CensusYear),]
  begin_ladder <- tree[which.max(tree$Echelle),"CensusYear"] == min(tree$CensusYear)
  no_pom_reported <- all(tree$CodeMeas == 0)
  #tag false negatives
  if(no_pom_reported){
    if(begin_ladder){
      laddered[which(id == t),"ladder_tag"] <- "initially_laddered"
    }
    else{
      laddered[which(id == t),"ladder_tag"] <- "not_reported"
    }
  }
}

trees_tocheckv2 <- trees_tocheck %>%
  left_join(laddered %>%
              filter(!ladder_tag == 0) %>%
              filter(CensusYear == 2017) %>%
              filter(CodeAlive == 1))
