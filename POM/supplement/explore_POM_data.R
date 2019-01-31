rm(list = ls())

load("Paracou_basic_POM.RData")
library(tidyverse)
library(gtable)

# ids <- titi %>%
#   filter(is.na(POM)) %>%
#   select(id) %>%
#   unique %>%
#   pull
#
titi <- titi %>% filter(Plot %in% 1:16) %>%  arrange(id,time)
# ladders <- titi %>%
  # filter(id %in% ids)


# for(i in ids){
#   temp <- ladders[which(ladders$id == i),c("time","POM","measure","code")]
#
#   g1 <- temp %>%
#     ggplot(aes(x = time, y = POM))+
#     geom_point()+
#     ggtitle(label = paste0("Corrected POM vs time for individual ",i))+
#     theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  code),
#                                                                          size = 3)
#
#   g2 <- temp %>%
#     ggplot(aes(x = time, y = measure))+
#     geom_point()+
#     ggtitle(label = paste0("Raw Circumference vs time for individual ",i))+
#     theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  code),
#                                                                          size = 3)
#
#   g1 <- ggplotGrob(g1)
#   g2 <- ggplotGrob(g2)
#   g <- rbind(g2, g1, size = "first")
#
#   ggsave(g, filename = paste0("./POM/graphs/g_circcorr_outlyer_",i,".jpeg"),device = "jpeg")
#   dev.off()
# }


# Is CodeMeas reliable ? --------------------------------------------------
# lag(c(1,2,3,4))
# titi_copy <- titi
titi$code_copy <- titi$code
titi[which(!titi$code %in% c(1,2,3)),"code"] <- 0

titi$code_lag <- lag(titi$code)
titi$code_lag_two <- lag(titi$code_lag)
titi$code_lag_three <- lag(titi$code_lag_two)
titi$code_lag_four <- lag(titi$code_lag_three)
titi$code_lag_five <- lag(titi$code_lag_four)

titi$time_lag <- lag(titi$time)
titi$time_lag_two <- lag(titi$time_lag)
titi$time_lag_three <- lag(titi$time_lag_two)
titi$time_lag_four <- lag(titi$time_lag_three)
titi$time_lag_five <- lag(titi$time_lag_four)

titi$id_lag <- lag(titi$id)
titi$id_lag_two <- lag(titi$id_lag)
titi$id_lag_three <- lag(titi$id_lag_two)
titi$id_lag_four <- lag(titi$id_lag_three)
titi$id_lag_five <- lag(titi$id_lag_four)


# Unique POM shift
titi$is_singlet <- titi$code %in% 1:3
titi$time_singlet <- ifelse(titi$is_singlet,
                            titi$time,
                            NA)
# Repeated twice
titi$is_duet <- titi$code == titi$code_lag &
  titi$code %in% 1:3 &
  titi$id == titi$id_lag

titi$time_duet <- ifelse(titi$is_duet & titi$time != 1,
                            paste(titi$time_lag,titi$time,sep = "_"),
                            NA)

#Repeated three times
titi$is_triplet <- titi$code == titi$code_lag &
  titi$code == titi$code_lag_two &
  titi$code %in% 1:3 &
  titi$id == titi$id_lag &
  titi$id == titi$id_lag_two

titi$time_triplet <- ifelse(titi$is_triplet & !titi$time%in%c(1,2),
                            paste(titi$time_lag_two,titi$time,sep = "_"),
                            NA)

#Repeated four times
titi$is_quadruplet <- titi$code == titi$code_lag &
  titi$code == titi$code_lag_three &
  titi$code == titi$code_lag_two &
  titi$id == titi$id_lag &
  titi$id == titi$id_lag_two &
  titi$id == titi$id_lag_three &
  titi$code %in% 1:3

titi$time_quadruplet <- ifelse(titi$is_duet & !titi$time%in%c(1,2),
                            paste(titi$time_lag_three,titi$time,sep = "_"),
                            NA)

#Repeated five times
titi$is_quintuplet <- titi$code == titi$code_lag &
  titi$code == titi$code_lag_three &
  titi$code == titi$code_lag_four &
  titi$code == titi$code_lag_two &
  titi$id == titi$id_lag &
  titi$id == titi$id_lag_two &
  titi$id == titi$id_lag_three &
  titi$id == titi$id_lag_four&
  titi$code %in% 1:3

titi$time_quintuplet <- ifelse(titi$is_duet & !titi$time%in%c(1,2),
                               paste(titi$time_lag_four,titi$time,sep = "_"),
                               NA)


# Isolate true duets, i.e. which are not due to triplets. -----------------


titi$is_duet_lag <- lag(titi$is_duet)
titi$is_duet_lead <- lead(titi$is_duet)

titi$real_duets <- titi$is_duet
# titi$real_duets[-1] <- titi$is_duet[-1] & titi$is_duet[-1]!=titi$is_duet_lag[-1]
# any(titi$is_duet[-1]&titi$is_duet_lag[-1])


titi$real_duets[which(titi$is_duet&titi$is_duet_lag)] <- F
titi$real_duets[which(titi$is_duet&titi$is_duet_lead)] <- F
identical(titi$real_duets,titi$is_duet)

titi$time_real_duets = ifelse(titi$real_duets, titi$time_duet, NA)


# Real singlets -----------------------------------------------------------

titi$is_singlet_lag <- lag(titi$is_singlet)
titi$is_singlet_lead = lead(titi$is_singlet)

titi$real_singlets <- titi$is_singlet
titi$real_singlets[which(titi$is_singlet & titi$is_singlet_lag == titi$is_singlet)] <- FALSE
titi$real_singlets[which(titi$is_singlet & titi$is_singlet_lead == titi$is_singlet)] <- FALSE

titi$time_real_singlets = ifelse(titi$real_singlets, titi$time_singlet, NA)



ids <- titi %>% filter(real_singlets) %>% select(id) %>% unique %>% pull

titi %>% filter(code %in% c(1,2,3)) %>% select(id) %>% unique %>%  pull %>% length

titi %>% filter(id %in% ids ) %>% filter(time == 2017, CodeAlive == T) %>% select(id) %>% unique %>%  pull %>% length

id_singlets <- titi %>% filter(real_singlets) %>% select(id) %>% unique %>% pull
id_duets <- titi %>% filter(real_duets) %>% select(id) %>% unique %>% pull
id_triplets <- titi %>% filter(is_triplet) %>% select(id) %>% unique %>% pull

# singlets <- titi %>% filter(id%in%id_singlets & time==2017 & CodeAlive) %>% select(id, Plot, POM) %>% unique
singlets <- titi %>% filter(id%in%id_singlets & CodeAlive) %>% select(id, Plot,POM,time,CodeAlive) %>% arrange(id, time)
duets <- titi %>% filter(id%in%id_duets & CodeAlive) %>% select(id, Plot,POM,time,CodeAlive) %>% arrange(id, time)
triplets <- titi %>% filter(id%in%id_triplets & CodeAlive) %>% select(id, Plot,POM,time,CodeAlive) %>% arrange(id, time)

for(i in id_singlets){
  discard <- which(singlets$id == i & (singlets$time != ifelse(length(singlets$time[which(singlets$id == i)])>1,
                                                               max(singlets$time[which(singlets$id == i)]),
                                                               0)))
  singlets <- singlets[-discard,]
}
for(i in id_duets){
  discard <- which(duets$id == i & (duets$time != ifelse(length(duets$time[which(duets$id == i)])>1,
                                                         max(duets$time[which(duets$id == i)]),
                                                         0)
                                    )
                   )
  duets <- duets[-discard,]
}
for(i in id_triplets){
  discard <- which(triplets$id == i & (triplets$time != ifelse(length(triplets$time[which(triplets$id == i)])>1,
                                                               max(triplets$time[which(triplets$id == i)]),
                                                               0)))
  triplets <- triplets[-discard,]
}

discard <- NA
for(i in 1:nrow(singlets)){
  if(!( singlets[i,"time"] > 2016 | (singlets[i,"Plot"] == 16 & singlets[i,"time"] == 2015))){
    discard <- c(discard, i)
  }
}
if(length(discard > 1)){
  singlets <- singlets[-discard[-1],]
}

discard <- NA
for(i in 1:nrow(duets)){
  if(!( duets[i,"time"] >= 2016 | (duets[i,"Plot"] == 16 & duets[i,"time"] == 2015))){
    discard <- c(discard, i)
  }
}
if(length(discard > 1)){
  duets <- duets[-discard[-1],]
}

discard <- NA
for(i in 1:nrow(triplets)){
  if(!( triplets[i,"time"] >= 2016 | (triplets[i,"Plot"] == 16 & triplets[i,"time"] == 2015))){
    discard <- c(discard, i)
  }
}
if(length(discard > 1)){
  triplets <- triplets[-c(discard[-1]),]
}
#here

(max(singlets$time[which(singlets$id == i)])>2016) |
  (singlets$Plot[which(triplets$id == i)] == "16" &
     max(triplets$time[which(singlets$id == i)]) == 2015)

singlets %>% select(time, Plot) %>% table



singlets %>% group_by(Plot) %>% summarise(count = n())
duets %>% group_by(Plot) %>% summarise(count = n())

# triplets <- titi %>% filter(id%in%id_triplets & time==2017 & CodeAlive) %>% select(id, Plot,POM) %>% unique

triplets %>% group_by(Plot) %>% summarise(count_triplets = n()) %>%
  full_join(duets %>% group_by(Plot) %>% summarise(count_duets = n())) %>%
  full_join(singlets %>% group_by(Plot) %>% summarise(count_singlets = n()))
# Individuals with strange stuff happening and to look at individu --------

# ID = 103934 , 166791
tojoin <- titi %>%
  filter(CodeAlive) %>%
  select(id, Xutm,Yutm,Xfield,Yfield,Genus,Species,measure,time) %>%
  mutate(species = paste(Genus, Species, sep="_")) %>%
  select(-Genus,-Species) %>%
  arrange(id, desc(time))
# tojoin_id <- tojoin %>% select(id) %>% unique %>% pull
# for(i in tojoin_id){
#   tojoin <- tojoin[-(which(tojoin$id == i)[-1]),]
# }
trees_tocheck <- singlets %>%
  mutate(type = "singlet") %>%
  rbind(duets %>%
          mutate(type = "duet")) %>%
  rbind(triplets %>%
          mutate(type = "triplets")) %>%
  left_join(tojoin)

trees_tocheck %>% select(POM,type) %>% table


save(trees_tocheck, file = "trees_tocheck.RData")





titi %>%
  filter_("real_singlets") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_real_singlets", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_real_singlets,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))+
  ggtitle(label="Occurence of single POM shift codes in function of census time", subtitle = "for plots 1 to 15")
# save(titi, file = "test_measure_codes.RData")

titi %>%
  filter_("is_duet") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_duet", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_duet,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))

titi %>%
  filter_("real_duets") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_real_duets", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_real_duets,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))+
  ggtitle(label="Occurence of POM shift codes duets in function of census time", subtitle = "for plots 1 to 15")
titi$real_duets %>% sum
titi$is_duet %>% sum
titi$is_triplet %>% sum
titi %>%
  filter_("is_triplet") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_triplet", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_triplet,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))+
  ggtitle(label="Occurence of POM shift codes triplets in function of census time", subtitle = "for plots 1 to 15")


# Explore POM reliability -------------------------------------------------
titi[1391064:1391068,]

titi[1391066, "measure"] <- 53

titi$measure_lag <- lag(titi$measure)

titi$growth_abs <- titi$measure-titi$measure_lag
titi$growth_abs[which(!titi$id == titi$id_lag)] <- NA

data <- titi %>% select(id,time,measure,growth_abs,is_duet,real_duets,real_singlets,is_triplet,Genus,Species, code, code_copy)

# titi$CodeAlivesummary


statis <- data.frame("threshold"=rep(seq(10,-10, by=-0.5),4), "cases" = NA, "code"=NA,"growth" = NA) %>%
  arrange(desc(threshold)) %>%
  mutate(code = rep(c(0,1,2,3),41))

statis_singlets <- statis

statis_multiplets <- statis

# statis_duets <- statis
#
# statis_triplets <- statis

count_cases <- function(data, threshold, cod){
  return(data %>%
           filter(code== cod) %>%
           filter(growth_abs< threshold) %>% #more negative than thresh
           nrow)
}

count_cases_singlets <- function(data, threshold, cod){
  return(data[which(data$real_singlets),] %>%
           filter(code== cod) %>%
           filter(growth_abs< threshold) %>% #more negative than thresh
           nrow)
}

count_cases_multiplets <- function(data, threshold, cod){
  return(data[which(!data$real_singlets),] %>%
           filter(code== cod) %>%
           filter(growth_abs< threshold) %>% #more negative than thresh
           nrow)
}

# count_cases_duets <- function(data, threshold, cod){
#   return(data %>%
#            filter(real_duets == T) %>%
#          filter(code== cod) %>%
#            filter(growth_abs< threshold) %>% #more negative than thresh
#            nrow)
# }
#
# count_cases_triplets <- function(data, threshold, cod){
#   return(data %>%
#            filter(is_triplet == T) %>%
#            filter(code== cod) %>%
#            filter(growth_abs< threshold) %>% #more negative than thresh
#            nrow)
# }

for(co in c(0,1,2,3)){
  for(th in seq(10,-10, by=-0.5)){
    statis[which(statis$code == co & statis$threshold == th), "cases"] <- count_cases(data = data, threshold = th,cod = co)

    statis_singlets[which(statis$code == co & statis$threshold == th), "cases"] <- count_cases_singlets(data = data, threshold = th,cod = co)

    statis_multiplets[which(statis$code == co & statis$threshold == th), "cases"] <- count_cases_multiplets(data = data, threshold = th,cod = co)
    # statis[which(statis$code == co & statis$threshold == th), "growth"] <- data %>% filter(code == co, threshold == th)
  }
}

statis <- statis %>% mutate(code = as.factor(code))

statis_singlets <- statis_singlets %>% mutate(code = as.factor(code))

statis_multiplets <- statis_multiplets %>% mutate(code = as.factor(code))

statis %>% filter(code !=0) %>%  ggplot(aes(x = threshold, y = cases, fill = code))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=-1, y = 1800, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 1, y = 1800, label = "delta(Circ) < -2*pi"), angle = 90)+
  ggtitle(label="Number of supposed POM shifts for which delta(circ) < threshold", subtitle = "In function of measure code, singlets and multiplets pooled, for plots 1 to 15")

statis_singlets %>% filter(code !=0) %>%  ggplot(aes(x = threshold, y = cases, fill = code))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=-1, y = 1350, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 1, y = 1350, label = "delta(Circ) < -2*pi"), angle = 90)+
  ggtitle(label="Number of supposed POM shifts for which delta(circ) < threshold", subtitle = "In function of measure code, for singlets, for plots 1 to 15")

statis_multiplets %>% filter(code !=0) %>%  ggplot(aes(x = threshold, y = cases, fill = code))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=-1, y = 100, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 1, y = 100, label = "delta(Circ) < -2*pi"), angle = 90)+
  ggtitle(label="Number of supposed POM shifts for which delta(circ) < threshold", subtitle = "In function of measure code, for multiplets, for plots 1 to 15")

statos <- statis %>%
  group_by(code) %>%
  mutate(cases_lag = lag(cases)) %>%
  group_by(code,threshold) %>%
  summarise(delta = cases-cases_lag)

statos_singlets <- statis_singlets %>%
  group_by(code) %>%
  mutate(cases_lag = lag(cases)) %>%
  group_by(code,threshold) %>%
  summarise(delta = cases-cases_lag)

statos_multiplets <- statis_multiplets %>%
  group_by(code) %>%
  mutate(cases_lag = lag(cases)) %>%
  group_by(code,threshold) %>%
  summarise(delta = cases-cases_lag)

statos %>% filter(code != 0) %>%  ggplot(aes(x = threshold, y = -delta, fill = as.factor(code)))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))

statos_singlets %>% filter(code != 0) %>%  ggplot(aes(x = threshold, y = -delta, fill = as.factor(code)))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))

statos_multiplets %>% filter(code != 0) %>%  ggplot(aes(x = threshold, y = -delta, fill = as.factor(code)))+ geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))
# data[1391064:1391068,]

data %>% filter(!(growth_abs < -100 | growth_abs > 100)) %>% ggplot(aes(x = growth_abs, color =  code))+geom_histogram( bins =  200)

data %>% filter(code == 0) %>%
  filter(growth_abs < 50 & growth_abs > -50) %>%
  mutate(code = as.factor(code)) %>%
  ggplot(aes(x = growth_abs, fill = code,stat(density)))+
  geom_histogram()

?geom_histogram
+
  geom_histogram(data)

data %>%
  filter(growth_abs < 50 & growth_abs > -50) %>%
  mutate(code = as.factor(code)) %>%
  ggplot(aes(x = growth_abs, fill = code,color=code,stat(density)))+
  geom_histogram(bins = 50, position = "dodge", alpha = 0.6)+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=1,y = 0.25, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 1, y = 0.25, label = "delta(Circ) < -2*pi"), angle = 90)+
  ggtitle(label="Density hist of the absolute growth rate for each code", subtitle = "for plots 1 to 15")

data %>%
  filter(growth_abs < 50 & growth_abs > -50) %>%
  filter(code != 0) %>%
  mutate(code = as.factor(code)) %>%
  ggplot(aes(x = growth_abs, fill = code,color=code,stat(density)))+
  geom_histogram(bins = 50, position = "dodge", alpha = 0.6)+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=3,y = 0.15, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 4, y = 0.15, label = "delta(Circ) < -2*pi"), angle = 90)+
  facet_wrap(~real_singlets)+
  ggtitle(label="density histogram of absolute growth per measure code (1 to 3), for singlets (\"TRUE\") and multiplets (\"FALSE\")", subtitle = "for plots 1 to 15")

data %>%
  filter(growth_abs < 50 & growth_abs > -50) %>%
  filter(!real_singlets & code != 0) %>%
  mutate(code = as.factor(code)) %>%
  ggplot(aes(x = growth_abs, fill = code,color=code,stat(density)))+
  geom_histogram(bins = 50, position = "dodge", alpha = 0.6)

data %>%
  filter(growth_abs < 50 & growth_abs > -50) %>%
  filter(code != 0) %>%
  # filter(real_singlets) %>%
  mutate(code = as.factor(code)) %>%
  ggplot(aes(x = growth_abs, fill = real_singlets,color=real_singlets,stat(density)))+
  geom_histogram(bins = 50, position = "dodge", alpha = 0.4)+
  geom_vline(xintercept = 0, colour = "black", size = 1, linetype = 2)+
  geom_text(aes(x=3,y = 0.15, label = "delta(Circ) < 0"), angle = 90)+
  geom_vline(xintercept = -2*pi, colour = "black", size = 1, linetype = 3)+
  geom_text(aes(x=-2*pi - 4, y = 0.15, label = "delta(Circ) < -2*pi"), angle = 90)+
  facet_wrap(~code)+
  ggtitle(label="density histogram of absolute growth: singlets vs multiplets", subtitle = "for plots 1 to 15")

+
  geom_density(alpha = 0.3)

titi %>% select(Plot,time) %>% mutate(time= as.factor(time)) %>% table
summary(data$growth_abs)

data %>% filter(!(growth_abs < -80 | growth_abs > 80)) %>% select(growth_abs) %>% pull %>% hist(breaks = 300)
