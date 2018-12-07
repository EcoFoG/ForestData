library(tidyverse)
trees_uncorrected = data.frame(id = c(rep(1,8),rep(2,8),rep(3,8)),
                               status = c(rep(1,8),
                                          rep(1,3),0,0,rep(1,2),0,
                                          rep(NA,4),rep(1,4)),
                               time = rep(seq(2000,2014,by=2),3))
trees_uncorrected <- trees_uncorrected[-c(2,5,23),]
trees_uncorrected$col1 <- 1
trees_uncorrected$col2 <- 2

trees_corrected = data.frame(id = c(rep(1,8),rep(2,8),rep(3,8)),
                             status = c(rep(1,8),
                                        rep(1,3),1,1,rep(1,2),0,
                                        rep(NA,4),rep(1,4)),
                             time = rep(seq(2000,2014,by=2),3))
trees_corrected <- trees_corrected %>% na.omit()
trees_corrected$col1 <- 1
trees_corrected$col2 <- 2

censuses <- sort(unique(trees_uncorrected$time),decreasing=F)
ids <- unique(trees_uncorrected$id)

for(i in ids){
  tree_temp <- trees_uncorrected[which(trees_uncorrected$id == i),]
  for(c in censuses){
    if(!c%in%tree_temp$time){
      line <- data.frame(id = i, status = NA, time = c)
      tree_temp <- rbind(tree_temp,line)
    }
  }
  tree_temp <- tree_temp[order(tree_temp$time),]
  first <- which(!is.na(tree_temp$status) & tree_temp$status)[1]
  last <- which(!is.na(tree_temp$status) & tree_temp$status)[length(which(!is.na(tree_temp$status) & tree_temp$status))]
  tree_temp$status[first:last] <- 1
  trees_uncorrected <- rbind(trees_uncorrected[which(trees_uncorrected$id != i),], tree_temp)

}


