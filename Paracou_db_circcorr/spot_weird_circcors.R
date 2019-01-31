print("Hello world")

# Clean the biniou and load libz ------------------------------------------
rm(list = ls())
invisible(gc())


library(gtable)

if("tidyverse" %in% installed.packages()){
  if(!("tidyverse"%in% loadedNamespaces()))
    library(tidyverse)
  
}else{
  install.packages("tidyverse") # In this case, sorry...
  library(tidyverse)
}

# load data ---------------------------------------------------------------
path <- "type ur path"

if("Paracou.RData" %in% list.files()){
  load("Paracou.RData")
}else if("Paracou.RData" %in% list.files(path)){
  load(file.path(path,"Paracou.RData"))
}else{
  data <- EcoFoG::Paracou2df()
}

data <- data[order(data$idTree,data$CensusYear),]
data %>% filter(CodeAlive == T) -> data

# Create columns ----------------------------------------------------------
data <- data %>% mutate(CircCorr_lag = lag(CircCorr),
                        Circ_lag = lag(Circ),
                        id_lag = lag(idTree),
                        time_lag = lag(CensusYear))

# Compute -----------------------------------------------------------------

data$species = paste(data$Genus,data$Species,sep="_")
data$growth_corr = (data$CircCorr-data$CircCorr_lag)/pi
data$growth = (data$Circ-data$Circ_lag)/pi
data$time = data$CensusYear - data$time_lag
data$growth_annual_corr = data$growth_corr/data$time
data$growth_annual = data$growth/data$time

## Suppress IDz mismatches (because we computed on whole database)

idmatch = data$idTree == data$id_lag
data$growth_annual_corr[which(!idmatch)] <- NA
data$growth_annual[which(!idmatch)] <- NA
data$growth_corr[which(!idmatch)] <- NA
data$growth[which(!idmatch)] <- NA

# Summarize and identify weirdoz ------------------------------------------
outlyers_ids <- data[which(data$growth_annual_corr > 5 | data$growth_annual_corr < -2),"idTree"] %>% unique
length(outlyers_ids)

# Isolate and search how come they be like that ---------------------------

## Quick check of idTree issue (the last thing we would suspect, but whatever, we just check)
## Everything alright on this side

# if(outlyers %>% filter(idTree == i) %>% select(species) %>% unique %>% pull %>% length > 1 |
#    outlyers %>% filter(idTree == i) %>% select(Plot) %>% unique %>% pull %>% length > 1 |
#    outlyers %>% filter(idTree == i) %>% select(SubPlot) %>% unique %>% pull %>% length > 1 ){
#   print("WTF")
# }
outlyers <- data[which(data$idTree %in%outlyers_ids),]
# table(outlyers %>% select(CodeCorr, Plot))



# Compare Circ and Circcorr in outlyers -----------------------------------

outlyers <- outlyers %>% 
  reshape2::melt(id.vars = c("idTree","species","CodeCorr","CensusYear"),
                 measure.vars = c("Circ","CircCorr"),
                 variable.name = "Ctype",
                 value.name = "C") %>%
  mutate(cor = grepl("corr",Ctype))%>% 
  left_join(outlyers %>%reshape2::melt(id.vars = c("idTree","species","CodeCorr","CensusYear"),
                                       measure.vars = c("growth","growth_corr"),
                                       variable.name = "Gtype",
                                       value.name = "G") %>% 
              mutate(cor = grepl("corr",Gtype))) 
# %>% head(40)
# i <- outlyers_ids[3]
for(i in outlyers_ids){
  # cnt <- cnt+1
  # i <- outlyers_ids[cnt]
  # print(cnt)

   temp <- outlyers %>%
     filter(idTree == i) %>%
     select(CensusYear,CodeCorr,species, idTree,C,G, Ctype, Gtype,cor) %>%
     mutate(D = C/pi,
            is_outlyer = as.factor(ifelse(G > 5,
                                          yes = "abnormal\ increase",
                                             no =  ifelse(G < -2,
                                                          yes = "abnormal\ decrease",
                                                          no = "NTD")))) %>% na.omit
   g1 <- temp %>%
     ggplot(aes(x =CensusYear, y =D, color = Ctype, shape = is_outlyer))+
     geom_point()+
     # geom_jitter()+
     ggtitle(label = paste0("Corrected diameter vs time for individual ",i),
             subtitle = paste0(paste(unique(temp$species), collapse = " ")," code correction: ",paste(unique(temp$CodeCorr),collapse = " ")))+
     scale_x_continuous(breaks = 1984:2017)+
     theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  Ctype),
                              size = 3)

   # g2 <- temp %>%
   #   ggplot(aes(x =CensusYear, y =growth_annual, color = is_outlyer_CircCorr))+
   #   geom_point()+
   #   ggtitle(label = paste0("Corrected diametr growth vs time for individual ",i),
   #           subtitle = paste0(paste(unique(temp$species), collapse = " "),", code correction: ",paste(unique(temp$CodeCorr),collapse = " ")))+
   #   scale_x_continuous(breaks = 1984:2017)+
   #   theme(axis.text.x = element_text(angle=45))
   # 
   # g1 <- ggplotGrob(g1)
   # g2 <- ggplotGrob(g2)
   # g <- rbind(g2, g1, size = "first")

   ggsave(g1, filename = paste0("./graphs/compare_circ_circcorr/g_circcorr_outlyer_",i,".jpeg"),device = "jpeg")
   dev.off()

}
data %>% filter(CodeCorr != 0) %>% pull %>% length / data %>% filter(CodeCorr == 0) %>% pull %>% length()
# 
# 
# g1 <- ggplotGrob(g1)
# g2 <- ggplotGrob(g2)
# g <- cbind(g2, g1, size = "first")
# plot(g)
# codes_outlyers <- unique(outlyers$CodeCorr)


# Do graphs and save them -------------------------------------------------




# cnt <- 0
# 
# dir.create("./graphs")
# 
# for(i in outlyers_ids){
#   cnt <- cnt+1
#   # i <- outlyers_ids[cnt]
#   print(cnt)
# 
#    temp <- outlyers %>%
#      filter(idTree == i) %>%
#      select(growth_annual, CensusYear,species, idTree,CircCorr,CodeCorr) %>%
#      mutate(diameter = CircCorr/pi,
#             is_outlyer_CircCorr = as.factor(ifelse(growth_annual > 5,
#                                                    yes = "abnormal\ increase",
#                                                    no =  ifelse(growth_annual < -2,
#                                                                 yes = "abnormal\ decrease",
#                                                                 no = "NTD"))))
#    g1 <- temp %>%
#      ggplot(aes(x =CensusYear, y =diameter, color = is_outlyer_CircCorr))+
#      geom_point()+
#      ggtitle(label = paste0("Corrected diameter vs time for individual ",i),
#              subtitle = paste0(paste(unique(temp$species), collapse = " ")," code correction: ",paste(unique(temp$CodeCorr),collapse = " ")))+
#      scale_x_continuous(breaks = 1984:2017)+
#      theme(axis.text.x = element_text(angle=45))
# 
#    g2 <- temp %>%
#      ggplot(aes(x =CensusYear, y =growth_annual, color = is_outlyer_CircCorr))+
#      geom_point()+
#      ggtitle(label = paste0("Corrected diametr growth vs time for individual ",i),
#              subtitle = paste0(paste(unique(temp$species), collapse = " "),", code correction: ",paste(unique(temp$CodeCorr),collapse = " ")))+
#      scale_x_continuous(breaks = 1984:2017)+
#      theme(axis.text.x = element_text(angle=45))
# 
#    g1 <- ggplotGrob(g1)
#    g2 <- ggplotGrob(g2)
#    g <- rbind(g2, g1, size = "first")
# 
#    ggsave(g, filename = paste0("./graphs/g_circcorr_outlyer_",i,".jpeg"),device = "jpeg")
#    dev.off()
# 
# }
# 
# 
# g1 <- ggplotGrob(g1)
# g2 <- ggplotGrob(g2)
# g <- cbind(g2, g1, size = "first")
# plot(g)
# codes_outlyers <- unique(outlyers$CodeCorr)



# Other graphs, quite useless

# for(code in codes_outlyers){
#   assign(paste0("code_",gsub("\ ","_",code),"_outlyers"),
#          outlyers[which(outlyers$CodeCorr == code),] %>%
#            mutate(is_outlyer_CircCorr = as.factor(ifelse(growth_annual > 5,
#                                            yes = 1,
#                                            no =  ifelse(growth_annual < -2,
#                                                   yes = -1,
#                                                   no = 0))
#                                            )
#                   )
#          )
# }
#

# for(code in codes_outlyers){
#   assign(paste0("g_code_",
#                 gsub("\ ",
#                      "_",
#                      code),
#                 "_outlyers"),
#          get(paste0("code_",
#                     gsub("\ ",
#                          "_",code)
#                     ,"_outlyers")
#              ) %>%
#     ggplot(aes(x=CensusYear,
#                y=growth_annual,
#                color = is_outlyer_CircCorr)
#            )+
#     geom_point()+
#       geom_hline(yintercept = 5, color = "red")+
#       scale_color_discrete()+
#       ggtitle(label = "Annual growths (CircCorr) as a function of Census Year",
#               subtitle= paste0("for individuals with code ",code," that show abnormal values"))
#   )
#   # ggsave(get(paste0("g_code_",
#   #                              gsub("\ ",
#   #                                   "_",
#   #                                   code),
#   #                              "_outlyers")
#   #                       ),
#   #        filename = file.path("./graphs",
#   #                             paste0("g_code_",
#   #                                    gsub("\ ",
#   #                                         "_",
#   #                                         code),
#   #                                    "_outlyers.pdf")),
#   #        device = "pdf"
#   #        )
#   # dev.off()
#
# }
