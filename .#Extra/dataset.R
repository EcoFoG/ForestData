data <- correct_alive(data %>% mutate(Plotsub = paste(Plot, SubPlot, sep="_")))
test <- data %>%
  rename(plot = Plot, time= CensusYear, size=Circ, size=Circ, size_corr = CircCorr, id = idTree, code_corr = CodeCorr)
test <- test %>% mutate(species = paste(Genus,Species, sep = "_"))
library(ForestData)

display_corrected_trees(test,
  code_corr_col="code_corr",
  size_corr_col = "size_corr",
  size_col = "size",
  measure_type = "Circ",
  status_corr = "status_corr",
  id_col = "id",
  time_col = "time",
  info_cols = c("species", "plot"),
  path_save = "./graphs",
  name = "diameter_corrections_",
  tag_pom = FALSE,
  create_folder = TRUE,
  overwrite = TRUE,
  device = "jpeg")


SubPlot1 <- data %>% filter(CensusYear %in% 1984:2003 & SubPlot %in% c(1) & Plot == 6)
SubPlot3 <- data %>% filter(CensusYear %in% seq(1985,2003, by=2) & SubPlot %in% c(3) & Plot == 6)

example_census <- SubPlot1 %>%
  rbind(SubPlot3) %>%
  select(Forest,
         SubPlot,
         idTree,
         Family,
         Genus,
         Species,
         CensusYear,
         CodeAlive,
         CodeMeas,
         Circ) %>%
  rename(Plot = SubPlot) %>%
  mutate(binomial_name=  paste(Genus, Species, sep="_")) %>%
  mutate(Forest = paste0(Forest,":P6"))
save(example_census,file="./data/example_census.RData")

data("example_census")
prepare_forestdata(example_census,plot_col="Plot",id_col="idTree",time_col="CensusYear", status_col = "CodeAlive",size_col="Circ",measure_type = "C",POM_col = "POM")
example_status_corr <- correct_alive(example_census)
save(example_status_corr, file = "./data/example_status_corr.RData")
example_size_corr <- correct_size(example_status_corr)
save(example_size_corr, file = "./data/example_size_corr.RData")
example_recruits_corr <- correct_recruits(example_size_corr)
save(example_recruits_corr, file = "./data/example_recruits_corr.RData")


ba <- compute_ba(example_recruits_corr)
