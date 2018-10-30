load("Paracou_basic_POM.RData")
library(tidyverse)
library(gtable)

ids <- titi %>%
  filter(is.na(POM)) %>%
  select(id) %>%
  unique %>%
  pull
ladders <- titi %>%
  filter(id %in% ids)


for(i in ids){
  temp <- ladders[which(ladders$id == i),c("time","POM","measure","code")]

  g1 <- temp %>%
    ggplot(aes(x = time, y = POM))+
    geom_point()+
    ggtitle(label = paste0("Corrected POM vs time for individual ",i))+
    theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  code),
                                                                         size = 3)

  g2 <- temp %>%
    ggplot(aes(x = time, y = measure))+
    geom_point()+
    ggtitle(label = paste0("Raw Circumference vs time for individual ",i))+
    theme(axis.text.x = element_text(angle=45))+ggrepel::geom_text_repel(aes(label =  code),
                                                                         size = 3)

  g1 <- ggplotGrob(g1)
  g2 <- ggplotGrob(g2)
  g <- rbind(g2, g1, size = "first")

  ggsave(g, filename = paste0("./POM/graphs/g_circcorr_outlyer_",i,".jpeg"),device = "jpeg")
  dev.off()
}


# Is CodeMeas reliable ? --------------------------------------------------
# lag(c(1,2,3,4))
# titi_copy <- titi

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

titi$time_triplet <- ifelse(titi$is_duet & !titi$time%in%c(1,2),
                            paste(titi$time_lag_two,titi$time,sep = "_"),
                            NA)

#Repeated four times
titi$is_quadruplet <- titi$code == titi$code_lag &
  titi$code == titi$code_lag_three &
  titi$code == titi$code_lag_two &
  titi$code %in% 1:3

titi$time_quadruplet <- ifelse(titi$is_duet & !titi$time%in%c(1,2),
                            paste(titi$time_lag_three,titi$time,sep = "_"),
                            NA)

#Repeated five times
titi$is_quintuplet <- titi$code == titi$code_lag &
  titi$code == titi$code_lag_three &
  titi$code == titi$code_lag_four &
  titi$code == titi$code_lag_two &
  titi$code %in% 1:3

titi$time_quintuplet <- ifelse(titi$is_duet & !titi$time%in%c(1,2),
                               paste(titi$time_lag_four,titi$time,sep = "_"),
                               NA)

titi %>%
  filter_("is_duet") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_duet", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_duet,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))

titi %>%
  filter_("is_triplet") %>%
  mutate(code = as.factor(code)) %>%
  group_by_("time_triplet", "code") %>%
  summarise(N = n()) %>%
  ggplot(aes(x = time_triplet,y=N,fill = code))+ geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))

for(i in ids){
  temp <- titi[which(titi$id == i),c("time","POM","measure","code")]
  for(j in 1:length(temp)){

  }
}
