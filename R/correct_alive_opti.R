#' Correct Trees Life Status in a Forest Inventory
#'
#' Spotting unseen trees and correcting their status.
#'
#' @param data Data.frame, no default. Forest inventory in the form of a long-format time series - one line is a measure for one individual during one census time.
#' @param id_col Character. The name of the column containing trees unique ids
#' @param time_col Character. The name of the column containing census year
#' @param alive_col Character. The name of the column containing tree vital status - 0=dead; 1=alive.
#' @param plot_col Character. The name of the column containing the plots indices.
#' @param byplot Logical. If there are several plots in your dataset, the correction is performed by plot, in case these would not be censuses the same years or with the same frequencies one another.
#' @param dead_confirmation_censuses Integer, defaults to 2. This is the number of censuses needed to state that a tree is considered dead, if unseen. In Paracou, we use the rule-of-thumb that if a tree is unseen twice, its probability to be actually dead is close to 1. The choice of this value involves that trees unseen during the X-1 last inventories can not be corrected for death, and thus mortality rates should not be calculated for these censuses.
#' @param use_size Character, but defaults to FALSE. Optional argument specifying that circumference or diameter must be used to create the vital status field. If your data already contains a field indicating whether the tree is dead -0 or FALSE- or alive -1 or TRUE-, let it to its default value. If you use this option, make sure beforehand that only live trees are measured - non-NA size - in your dataset's protocol.
#'
#' @return a data.frame containing the corrected data - with trees' corrected life statuses. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the trees were unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.
#' @export
#'
#' @examples
correct_alive_opti <- function(data,
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
  else names(data)[which(names(data)==time_col)] <- "time"
  ## ids
  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data)[which(names(data)==id_col)] <- "id"
  ## alive code
  if(use_size != FALSE & !alive_col%in%names(data)){
    stop("The name you indicated (or let to default) for the tree vital status column is apparently not in the colnames of the dataset. You must specify it, or if it does not exist, use the argument use_size to create it from measurements under the hypothesis that only live trees were measured in your inventory")
  }
  else names(data)[which(names(data)==id_col)] <- "status"
  ## plots
  if(byplot & !plot_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else if(byplot) names(data)[which(names(data)==plot_col)] <- "plot"

  ##Other
  if(!(is.numeric(dead_confirmation_censuses)&length(dead_confirmation_censuses)==1)) stop("argument dead_confirmation censuses must be an integer scalar")

  if(!is.logical(byplot)) stop("byplot must be a single logical value(TRUE or FALSE)")

  if(!"status" %in% names(data)) data$status <- NA
  data$status_corr <- data$status

  # Call internals by plot or not --------------------------------------------

  if(byplot){
    plots <- unique(data$plot)
    for(p in plots){
      data <- rbind(data[which(data$plot != p),], .correct_status_plotlevel(data[which(data$plot == p),],dead_confirmation_censuses,use_size))
    }
    return(data)
  }
  else return(.correct_status_plotlevel(data, dead_confirmation_censuses, use_size))
}


# Internals ---------------------------------------------------------------

#' Internal plot-level life status correction
#'
#' @param data_plot data.frame, a single-plot forest inventory which names correspond to the format set in correct_alive, which is on the long format for census years - one line is one tree measured at one census. It already contains a field named status_corr, which as this point is just a copy of the raw, uncorrected status field
#' @param dead_confirmation_censuses numeric scalar, the number of "unsighting" censuses from which we state that the tree is almost certainly dead. In Paracou, we set it to 2 considered censusing rythm and the experience of the censusing crew
#' @param use_size Character, but defaults to FALSE. Optional argument specifying that circumference or diameter must be used to create the vital status field. If your data already contains a field indicating whether the tree is dead -0 or FALSE- or alive -1 or TRUE-, let it to its default value. If you use this option, make sure beforehand that only live trees are measured - non-NA size - in your dataset's protocol.
#'
#' @return a data.frame containing the inputted plot-level data with trees' corrected life statuses. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the tree is unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.

.correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size){
  if(use_size != FALSE){
    if(!use_size %in% names(data_plot)){
      stop("use_size defaults to FALSE, but to activate this option, it must contain the name of the column containing circumference or diameter measurements")
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
    # print("data plot")
    # print(data_plot[which(data_plot$id != i),])
    # print("tree")
    # print(.correct_alive_tree(tree_temp, censuses, dead_confirmation_censuses,i))


    data_plot <- rbind(data_plot[which(data_plot$id != i),],
                       .correct_alive_tree(tree_temp,
                                           censuses,
                                           dead_confirmation_censuses,
                                           i))
  }
  return(data_plot)
}

#' Internal tree-level life status correction
#'
#' @param tree_temp a data.frame corresponding to a single tree's measurements, arranged by time
#' @param censuses
#' @param dead_confirmation_censuses
#' @param i character or numeric, but single value. the id of the tree that is being corrected in the function
#'
#' @return a data.frame containing the inputted individual-tree-level data with corrected life status. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the tree is unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.

.correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i){

  # Create lines for each census
  # Instead of re-looping, we can extract directly the censuses for which tree was not seen.
  absents <- which(!censuses %in% tree_temp$time)
  nabs <- length(absents)
  # Create new rows, but without knowing how many column are in data and what they contain !
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

    # Add other columns which names are unknown (to allow subsequent rbinding), and default-set it to NA
    new.rows[,names(tree_temp)[-which(names(new.rows)%in%names(tree_temp))]] <- NA
    tree_temp[(nrow(tree_temp)+1):(nrow(tree_temp)+nabs),names(new.rows)] <- new.rows
  }

  #Make sure that it is ordered by increasing census time
  tree_temp <- tree_temp[order(tree_temp$time),]

  #Find the first and last time the tree was seen alive. He was obviously alive during all censuses between those
  if(!all(is.na(tree_temp$status))){

    #Isolate the first and last census when seen alive.
    first_seen_alive <- which(tree_temp$status == 1)[1]
    last_seen_alive <- max(which(tree_temp$status == 1))
    # first_seen_alive <- min(which(tree_temp$status == 1))
    # last_seen_alive <- max(which(tree_temp$status == 1))

    #Correct tree status between both
    tree_temp$status_corr[first_seen_alive:last_seen_alive] <- 1

    # Then, determine what happened after the last time the tree was seen alive. Seen dead ? unseen ?
    # Here comes the argument dead_confirmation_censuses (that defaults to 2), which represents
    # how many consecutive censuses without seeing the tree are needed to state its death.

    #If the tree is declared dead just after having been seen alive for the last time, suppress the censuses after that
    if(!last_seen_alive == nrow(tree_temp)){
      # If the tree is seen dead right after the last census it has been seen alive
      # the next lines are useless, thus we suppress them
      if(!is.na(tree_temp$status[last_seen_alive+1]) &  tree_temp$status[last_seen_alive+1] == 0){
        tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
      }
      else{ #If the tree was actually unseen (status=NA)

        # If the tree is unseen more than dead_confirmation_censuses times
        # it means that it is almost certainly dead

        if(last_seen_alive < nrow(tree_temp)-(dead_confirmation_censuses-1)){

          # So, we correct stating that the tree died right after first unsighting
          tree_temp$status_corr[last_seen_alive+1] <- 0
          # and we erase the lines after it
          tree_temp <- tree_temp[-(last_seen_alive+2:nrow(tree_temp)),]
        }
        else{
          ## Open debate : how do we handle unsightings (keep, drop, tag)
          ## if there are not enough to state that the tree is dead ?

          # For now, I decided that these would be the only "NA" cases
          # let in the corrected vital status field. This can be modified
          # just by changing the next line and put some code instead of NA
          # I decided not to do so because status_corr would then be considered
          # as a "character" col, not anymore a "logical", which is unconvenient
          # to compute mortality as I wrote it in the corresponding func.
          tree_temp[(last_seen_alive+1):nrow(tree_temp),"status_corr"] <- NA
        }
      }
    }
    if(first_seen_alive > 1){
      # if(all(is.na(tree_temp$status_corr[1:(first_seen_alive-1)]))){
        tree_temp <- tree_temp[-(1:(first_seen_alive-1)),]
      # }
    }
  }
  return(tree_temp)
}

#' Create vital status field from size measurements under the hypothesis that only live trees were measured.
#'
#' @param data_plot data.frame, a single-plot forest inventory whithout information on tree vital status, in which only live trees were measured, which names correspond to the format set in correct_alive, which is on the long format for census years - one line is one tree measured at one census.
#' @param use_size character, the name of the column containing tree size measurements - diameter of circumference.
#'
#' @return the same data.frame with a status field. status = 1 in any line where size is non-NA

.use_size_status <- function(data_plot, use_size){
  data_plot$status <- NA
  data_plot$status[which(!is.na(data_plot[which(names(data_plot)== use_size)]))] <- 1
  return(data_plot)
}


# Minor enhancements that can be done -------------------------------------

# Secure the sub-functions to be used independently by the users that really want to test it ?
## i.e. making them relatively independant from above-level formating and ordering

# Rename back the output

# Create a status_correction_code to indicate the type of correction and the reason why ?
## can help flagging the "unseen not yet statable as dead" without loosing status_corr 's logical datatype
## but is maybe of low informative power, thus could be set as an option.
## but if we would do an option for every case like this, the arglist would be super long !

# Tools and alternative codes ---------------------------------------------

## deprecated loop on censuses to create new lines
# for(c in censuses){
#   if(!c%in%tree_temp$time){
#     line <- data.frame(id = i, status = NA, time = c)
#     missing_vars <- names(tree_temp)[which(!names(tree_temp)%in% names(lines))]
#     line[,missing_vars] <- NA
#     line <- line[,names(tree_temp)]
#     tree_temp <- rbind(tree_temp,line)
#   }
# }

