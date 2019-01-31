library(tidyverse)
data_ladder <- read.table("NINO2.txt", sep = ";",header=T)

names(data_ladder) <- c("idForet","Plot","campagne","Echelle","idArbre")

data_ladder <- data_ladder %>%
  rename(idTree = idArbre) %>%
  # rowwise %>%
  mutate(Echelle = ifelse(Echelle == 1, T, ifelse(Echelle == 0,F,NA)))

laddered_trees <- data_ladder %>%
  filter(Echelle) %>%
  select(idTree) %>%
  unique %>%
  pull

uprisen_trees <- titi %>%
  filter(is_singlet) %>%
  select(id) %>%
  unique %>%
  pull

ids <- titi %>% select(id) %>% unique %>% pull
length(ids)

uprisen_trees %>% length
which(uprisen_trees %in% laddered_trees) %>% length
which(!uprisen_trees %in% laddered_trees) %>% length

laddered_trees %>% length
which(laddered_trees%in%uprisen_trees  ) %>% length
which(!laddered_trees%in%uprisen_trees  ) %>% length

which(laddered_trees%in%ids  ) %>% length
which(!laddered_trees%in%ids  ) %>% length

length(laddered_trees)
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
