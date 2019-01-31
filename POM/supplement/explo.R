data_ladder <- read.table("NINO2.txt", sep = ";",header=T)
names(data_ladder) <- c("idForet","Plot","campagne","Echelle","idArbre")

iddata_ladder <- data_ladder %>%
  select(idArbre) %>%
  unique %>%
  pull
idtest <- test %>%
  select(idTree) %>%
  unique %>%
  pull

test %>%
  filter(!idTree%in%iddata_ladder) %>% nrow

only_paracou <- test %>%
  filter(!idTree%in%iddata_ladder) %>% select(idTree,CensusYear,Plot)
only_paracou %>% select(CensusYear,Plot) %>% table


only_ladder <- data_ladder %>%
  filter(!idArbre%in%idtest) %>% select(idArbre,campagne,Plot)
only_ladder %>% select(campagne,Plot) %>% table

View(data_ladder)

save(only_ladder, file = "only_ladder.RData")
save(only_paracou, file = "only_paracou.RData")

only_ladder %>% select(idArbre) %>% unique %>% nrow
