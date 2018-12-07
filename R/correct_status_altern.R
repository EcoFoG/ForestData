#' Title
#'
#' @param data Data.frame, no default. Forest inventory in the form of a long-format time series - one line is a measure for one individual during one census time.
#' @param id_col
#' @param time_col
#' @param alive_col
#' @param plot_col
#' @param byplot Logical. If there are several plots in your dataset, the correction is performed by plot, in case these would not be censuses the same years or with the same frequencies one another.
#' @param dead_confirmation_censuses Integer, defaults to 2. This is the number of censuses needed to state that a tree is considered dead, if unseen. In Paracou, we use the rule-of-thumb that if a tree is unseen twice, its probability to be actually dead is close to 1. The choice of this value involves that trees unseen during the X-1 last inventories can not be corrected for death, and thus mortality rates should not be calculated for these censuses.
#' @param use_size Character, but defaults to FALSE. Optional argument specifying that circumference or diameter must be used to create the vital status field. If your data already containes a field indicating whether the tree is dead -0 or FALSE- or alive -1 or TRUE-, let it to its default value. If you use this option, make sure beforehand that only live trees are measured in your dataset.
#'
#' @return
#' @export
#'
#' @examples
correct_alive <- function(data,
                          id_col = "idTree",
                          time_col = "CensusYear",
                          alive_col = "CodeAlive",
                          plot_col = "Plot",
                          byplot = TRUE,
                          dead_confirmation_censuses = 2,
                          use_size = FALSE){
# Checks and preparation --------------------------------------------------

  # Trivial check of data arg
  if(!is.data.frame(data)){
    stop("data must be a dataframe")
  }

  #Checks if columns are well specified and temporarily replace their names
  ## Census years
  if(!time_col%in%names(data)){
    stop("The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[,which(names(data)==time_col)]) <- "time"
  ## ids
  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[,which(names(data)==id_col)]) <- "id"
  ## alive code
  if(use_size != FALSE & !alive_col%in%names(data)){
    stop("The name you indicated (or let to default) for the tree vital status column is apparently not in the colnames of the dataset. You must specify it, or if it does not exist, use the argument use_size to create it from measurements under the hypothesis that only live trees were measured in your inventory")
  }
  else names(data[,which(names(data)==id_col)]) <- "status"
  ## Plots
  if(byplot & !plot_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else if(byplot) names(data[,which(names(data)==plot_col)]) <- "Plot"

  ##Other
  if(!(is.numeric(dead_confirmation_censuses)&length(dead_confirmation_censuses)==1)) stop("argument dead_confirmation censuses must be an integer scalar")

  if(!is.logical(byplot)) stop("byplot must be a single logical value(TRUE or FALSE)")

  if(!"status" %in% names(data)) data$status <- NA
  data$status_corr <- data$status

# Call internals by plot or not --------------------------------------------

  if(byplot){
    plots <- unique(data$Plot)
    for(p in 1:length(plots)){
      data <- rbind(data[which(data$plot != p),], .correct_status_plotlevel(data[which(data$plot == p),],dead_confirmation_censuses,use_size))
    }
    return(data)
  }
  else return(.correct_status_plotlevel(data, dead_confirmation_censuses, use_size))
}


# internals ---------------------------------------------------------------

.correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size){
  if(use_size != FALSE){
    if(!use_size %in% names(data_plot)){
      stop("use_size must contain the name of the column containing circumference or diameter measurements")
    }
    else{
      data <- .use_size_status(data_plot, use_size)
    }
  }

  #Dataset preparation
  # data_plot$status_corr <- data_plot$status
  censuses <- unique(data_plot$time)
  ids <- unique(data_plot$id)

  for(i in ids){
    tree_temp <- data_plot[which(data_plot$id == i),]

    data_plot <- rbind(data_plot[which(data_plot$id != i),], .correct_alive_tree(tree_temp, censuses, dead_confirmation_censuses,i))
  }
  return(data_plot)
}

.correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i){

  # Create lines for each census
  # Instead of re-looping, we can extract directly the censuses for which tree was not seen.
  absents <- which(!censuses %in% tree_temp$time)
  # Create new rows, but without knowing how many column are in data and what they contain !
  # print(absents)
  # print(length(absents))
  # print(absents)
  print(paste("tree temp before",i))
  print(tree_temp)
  if(length(absents) > 0){
    new.rows <- data.frame(id = i,
                           time = censuses[absents],
                           status = rep(NA, length(absents)),
                           status_corr = rep(NA, length(absents)))
    print(paste("new_rows",i))
    print(new.rows)
    tree_temp[(nrow(tree_temp)+1):(nrow(tree_temp)+nrow(new.rows)),names(new.rows)] <- new.rows
  }
  print(paste("tree temp after",i))
  print(tree_temp)
  #Make sure that it is ordered by increasing census time
  tree_temp <- tree_temp[order(tree_temp$time),]

  #Find the first and last time the tree was seen alive. He was obviously alive during all censuses between those
  if(!all(is.na(tree_temp$status))){
    #Isolate the first and last census when seen alive.
    first_seen_alive <- min(which(tree_temp$status == 1))
    last_seen_alive <- max(which(tree_temp$status == 1))

    #Correct tree status between both
    tree_temp$status_corr[first_seen_alive:last_seen_alive] <- 1

    # Then, determine what happened after the last time the tree was seen alive. Seen dead ? unseen ?
    # Here comes the argument dead_confirmation_censuses (that defaults to 2), which represents
    # How many consecutive censuses without seeing the tree are needed to state its death.

    #If the tree is declared dead just after having been seen alive for the last time, suppress the censuses after that
    if(!last_seen_alive == length(tree_temp$status)){
      if(tree_temp$status[last_seen_alive+1] == 0){
        tree_temp <- tree_temp[-(last_seen_alive+1:nrow(tree_temp)),]
      }
      else{ #If the tree was then unseen
        if(last_seen_alive < length(tree_temp$status_corr)-(dead_confirmation_censuses-1)){
          # if(all(is.na(tree_temp$status[last_seen_alive+1:last_seen_alive+dead_confirmation_censuses]))){
          tree_temp$status_corr[last_seen_alive+1] <- 0
          tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
          # }
        }
        ## Question here: do we let unseen reports if there are not enough to state that the tree is dead ?
      }
    }
    print(first_seen_alive)
    print(last_seen_alive)
    if(first_seen_alive > 1){
      # print(tree_temp$status_corr[1:(first_seen_alive-1)])
      if(all(is.na(tree_temp$status_corr[1:(first_seen_alive-1)]))){
        tree_temp <- tree_temp[-(1:(first_seen_alive-1)),]
      }
    }
  }
  return(tree_temp)
}

.use_size_status <- function(data_plot, use_size){
  data_plot$status <- NA
  data_plot$status[which(!is.na(data_plot[which(names(data_plot)== use_size)]))] <- 1
  return(data_plot)
}

# Tools and alternative codes ---------------------------------------------

## Loop on censuses to create new lines: deprecated ?
# for(c in censuses){
#   if(!c%in%tree_temp$time){
#     line <- data.frame(id = i, status = NA, time = c)
#     missing_vars <- names(tree_temp)[which(!names(tree_temp)%in% names(lines))]
#     line[,missing_vars] <- NA
#     line <- line[,names(tree_temp)]
#     tree_temp <- rbind(tree_temp,line)
#   }
# }

## Reminder: as function to deal with datatypes issues
# as(one,class(other))
