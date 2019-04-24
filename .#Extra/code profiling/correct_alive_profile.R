library(tidyverse)
paracou <- EcoFoG::Paracou2df()

paracoutest <- paracou %>% filter(Plot %in% c(1) & SubPlot==1)

profvis::profvis({
  .correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size){
    if(use_size != FALSE){
      if(!use_size %in% names(data_plot)){
        stop("use_size defaults to FALSE, but to activate this option, it must contain the name of the column containing circumference or diameter measurements")
      }
      else{
        data <- .use_size_status(data_plot, use_size)
      }
    }

    censuses <- unique(data_plot$time)
    ids <- unique(data_plot$id)

    for(i in ids){
      data_plot <- rbind(data_plot[which(data_plot$id != i),],
                         .correct_alive_tree(data_plot[which(data_plot$id == i),],
                                             censuses,
                                             dead_confirmation_censuses,
                                             i))
    }
    return(data_plot)
  }

  .correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i){
    absents <- !censuses %in% tree_temp$time
    nabs <- sum(absents)
    if(nabs > 0){
      if("plot" %in% names(tree_temp)){
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = rep(NA, nabs),
                               status_corr = rep(NA, nabs),
                               plot = rep(tree_temp$plot[1],nabs))
      }
      else{
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = rep(NA, nabs),
                               status_corr = rep(NA, nabs))
      }
      new.rows[,names(tree_temp)[-which(names(new.rows)%in%names(tree_temp))]] <- NA
      tree_temp[(nrow(tree_temp)+1):(nrow(tree_temp)+nabs),names(new.rows)] <- new.rows
    }
    tree_temp <- tree_temp[order(tree_temp$time),]
    if(!all(is.na(tree_temp$status))){
      first_seen_alive <- which(tree_temp$status == 1)[1]
      last_seen_alive <- max(which(tree_temp$status == 1))
      tree_temp$status_corr[first_seen_alive:last_seen_alive] <- 1
      if(!last_seen_alive == nrow(tree_temp)){
        if(!is.na(tree_temp$status[last_seen_alive+1]) &  tree_temp$status[last_seen_alive+1] == 0){
          tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
        }
        else{
          if(last_seen_alive < nrow(tree_temp)-(dead_confirmation_censuses-1)){
            tree_temp$status_corr[last_seen_alive+1] <- 0
            tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
          }
          else{
            tree_temp[(last_seen_alive+1):nrow(tree_temp),"status_corr"] <- NA
          }
        }
      }
      if(first_seen_alive > 1){
        tree_temp <- tree_temp[-(1:(first_seen_alive-1)),]
      }
    }
    return(tree_temp)
  }
  data = paracoutest
id_col = "idTree"
time_col = "CensusYear"
alive_col = "CodeAlive"
plot_col = "Plot"
byplot = TRUE
dead_confirmation_censuses = 2
use_size = FALSE
  if(!is.data.frame(data)){
    stop("data must be a dataframe")
  }
  if(!time_col%in%names(data)){
    stop("The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it")
  }else names(data)[which(names(data)==time_col)] <- "time"
  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }else names(data)[which(names(data)==id_col)] <- "id"
  if(use_size != FALSE & !alive_col%in%names(data)){
    stop("The name you indicated (or let to default) for the tree vital status column is apparently not in the colnames of the dataset. You must specify it, or if it does not exist, use the argument use_size to create it from measurements under the hypothesis that only live trees were measured in your inventory")
  }else names(data)[which(names(data)==id_col)] <- "status"
  if(byplot & !plot_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }else if(byplot) names(data)[which(names(data)==plot_col)] <- "plot"
  if(!(is.numeric(dead_confirmation_censuses)&length(dead_confirmation_censuses)==1)) stop("argument dead_confirmation censuses must be an integer scalar")
  if(!is.logical(byplot)) stop("byplot must be a single logical value(TRUE or FALSE)")
  if(!"status" %in% names(data)) data$status <- NA
  data$status_corr <- data$status
  if(byplot){
    plots <- unique(data$plot)
    for(p in plots){
      data <- rbind(data[which(data$plot != p),], .correct_status_plotlevel(data[which(data$plot == p),],dead_confirmation_censuses,use_size))
    }
  }else
    .correct_status_plotlevel(data, dead_confirmation_censuses, use_size)

})

