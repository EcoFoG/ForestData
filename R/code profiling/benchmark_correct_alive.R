microbenchmark::microbenchmark({
  .correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size){
    if(use_size != FALSE){
      if(!use_size %in% names(data_plot)){
        stop("use_size defaults to FALSE, but to activate this option, it must contain the name of the column containing circumference or diameter measurements")
      }
      else{
        data_plot <- .use_size_status(data_plot, use_size)
      }
    }

    censuses <- unique(data_plot$time)
    ids <- unique(data_plot$id)
    data_plot <- data_plot[order(data_plot$id, data_plot$time),]
    data_plot <- do.call(rbind,lapply(ids,function(i) .correct_alive_tree(data_plot[which(data_plot$id == i),],
                                                                          censuses,
                                                                          dead_confirmation_censuses,
                                                                          i)))
    return(data_plot)
  }

  .correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i){
    vars <- names(tree_temp)
    first_record <- tree_temp$time[which(tree_temp$status == 1)[1]]
    if(is.na(first_record)){
      if(all(is.na(tree_temp$status))){
        message <- paste0("tree ",i," has only NA life status. If it is a isolated outlyer, please manually check it. If there is no life status column in your dataset, you can create it from size measurement (see the vignette)")
      }else if(all(isFALSE(tree_temp$status))){
        message <- paste0("tree ",i," has only been recorded dead. It might be that it has been recruited and died on the same between-censuses interval. Please verify it")
      }
      warning(message)
    }else{
      absents <- (censuses > first_record & !censuses %in% tree_temp$time)

      # last_seen_alive <- max(which(tree_temp$status == 1))

      # first_seen_alive <- which(tree_temp$status == 1)[1]
      # last_seen_alive <- max(which(tree_temp$status == 1))
      # tree_temp$status_corr[first_seen_alive:last_seen_alive] <- 1
      # if(!last_seen_alive == nrow(tree_temp)){
      #   if(!is.na(tree_temp$status[last_seen_alive+1]) &  tree_temp$status[last_seen_alive+1] == 0){
      #     tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
      #   }
      #   else{
      #     if(last_seen_alive < nrow(tree_temp)-(dead_confirmation_censuses-1)){
      #       tree_temp$status_corr[last_seen_alive+1] <- 0
      #       tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
      #     }
      #     else{
      #       tree_temp[(last_seen_alive+1):nrow(tree_temp),"status_corr"] <- NA
      #     }
      #   }
      # }
      # if(first_seen_alive > 1){
      #   tree_temp <- tree_temp[-(1:(first_seen_alive-1)),]
      # }


      # absents <- which(!censuses %in% tree_temp$time)
      # nabs <- length(absents)
      nabs <- sum(absents)
      # print(nabs)
      # print(absents)
      # print(censuses)
      # print(first_record)
      # print(censuses > first_record)
      # print(which(tree_temp$status == 1)[1])
      # print(tree_temp)
      if(nabs > 0){
        if("plot" %in% vars){
          new.rows <- data.frame(id = i,
                                 time = censuses[absents],
                                 status = NA,
                                 status_corr = NA,
                                 plot = tree_temp$plot[1])
          newnames <- c("id","time","status","status_corr","plot")
        }
        else{
          new.rows <- data.frame(id = i,
                                 time = censuses[absents],
                                 NA,
                                 NA)
          newnames <- c("id","time","status","status_corr")
        }

        # new.rows[,vars[-which(vars%in%newnames)]] <- NA
        # print(new.rows)
        tree_temp[(nrow(tree_temp)+1):(nrow(tree_temp)+nabs),newnames] <- new.rows
        # if(rnorm(1,0,1) > 0.7) print(tree_temp[,vars[-which(vars%in%newnames)]])
        tree_temp <- tree_temp[order(tree_temp$time),]
      }
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
  }else names(data)[which(names(data)==alive_col)] <- "status"
  if(byplot & !plot_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }else if(byplot) names(data)[which(names(data)==plot_col)] <- "plot"
  if(!(is.numeric(dead_confirmation_censuses)&length(dead_confirmation_censuses)==1)) stop("argument dead_confirmation censuses must be an integer scalar")
  if(!is.logical(byplot)) stop("byplot must be a single logical value(TRUE or FALSE)")
  if(is.na(alive_col) & !"status" %in% names(data)){
    data$status <- NA
  }
  # else if(!is.na(status_col)){
  # names(data)[which(names(data)==status_col)] <- "status"
  # }
  data$status_corr <- data$status
  if(byplot){
    plots <- unique(data$plot)
    # for(p in plots){
    #   data <- rbind(data[which(data$plot != p),], .correct_status_plotlevel(data[which(data$plot == p),],dead_confirmation_censuses,use_size))
    # }
    data <- do.call(rbind,lapply(plots,function(p) .correct_status_plotlevel(data[which(data$plot == p),],
                                                                             dead_confirmation_censuses,
                                                                             use_size)))
  }else
    .correct_status_plotlevel(data, dead_confirmation_censuses, use_size)
},
{
  .correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size){
    if(use_size != FALSE){
      if(!use_size %in% names(data_plot)){
        stop("use_size defaults to FALSE, but to activate this option, it must contain the name of the column containing circumference or diameter measurements")
      }
      else{
        data_plot <- .use_size_status(data_plot, use_size)
      }
    }

    censuses <- unique(data_plot$time)
    ids <- unique(data_plot$id)
    data_plot <- data_plot[order(data_plot$id, data_plot$time),]
    data_plot <- do.call(rbind,lapply(ids,function(i) .correct_alive_tree(data_plot[which(data_plot$id == i),],
                                                                          censuses,
                                                                          dead_confirmation_censuses,
                                                                          i)))
    return(data_plot)
  }

  .correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i){
    vars <- names(tree_temp)
    absents <- !censuses %in% tree_temp$time
    # absents <- which(!censuses %in% tree_temp$time)
    # nabs <- length(absents)
    nabs <- sum(absents)
    if(nabs > 0){
      if("plot" %in% vars){
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = rep(NA, nabs),
                               status_corr = rep(NA, nabs),
                               plot = rep(tree_temp$plot[1],nabs))
        newnames <- c("id","time","status","status_corr","plot")
      }
      else{
        new.rows = matrix()
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = rep(NA, nabs),
                               status_corr = rep(NA, nabs))
        newnames <- c("id","time","status","status_corr")
      }

      # new.rows[,vars[-which(vars%in%newnames)]] <- NA
      # print(new.rows)
      tree_temp[(nrow(tree_temp)+1):(nrow(tree_temp)+nabs),newnames] <- new.rows
      # if(rnorm(1,0,1) > 0.7) print(tree_temp[,vars[-which(vars%in%newnames)]])
      tree_temp <- tree_temp[order(tree_temp$time),]
    }
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
    # for(p in plots){
    #   data <- rbind(data[which(data$plot != p),], .correct_status_plotlevel(data[which(data$plot == p),],dead_confirmation_censuses,use_size))
    # }
    data <- do.call(rbind,lapply(plots,function(p) .correct_status_plotlevel(data[which(data$plot == p),],
                                                                             dead_confirmation_censuses,
                                                                             use_size)))
  }else
    .correct_status_plotlevel(data, dead_confirmation_censuses, use_size)
},
times = 5)


microbenchmark::microbenchmark({
  toto <- data.frame("a"=rnorm(100000,0,1),
                     "b"=rep(NA,100000),
                     "c"=rep(NA,100000),
                     "d"==rep(NA,100000))
},
{
  titi <- data.frame(a=rnorm(100000,0,1),
                     b=rep(NA,100000),
                     c=rep(NA,100000),
                     d=rep(NA,100000))
},
{
  a=rnorm(100000,0,1)
  b=rep(NA,100000)
  c=rep(NA,100000)
  d=rep(NA,100000)
  toto <- data.frame(a,b,c,d)
},
{
  a=rnorm(100000,0,1)
  b=NA
  c=NA
  d=NA
  toto <- data.frame(a,b,c,d)
},
{
  tutu <- data.frame(a=rnorm(100000,0,1),
               b=NA,
               c=NA,
               d=NA)
},
times = 1000)

