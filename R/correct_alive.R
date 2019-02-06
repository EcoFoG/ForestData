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
correct_alive <- function(data,
                          id_col = "idTree",
                          time_col = "CensusYear",
                          status_col = "CodeAlive",
                          plot_col = "Plot",
                          byplot = TRUE,
                          dead_confirmation_censuses = 2,
                          use_size = FALSE) {
  # Checks and preparation --------------------------------------------------

  # Trivial check of data arg
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }

  if (!is.logical(use_size))
    stop("use_size must be a single logical value(TRUE or FALSE)")
  if (!is.logical(byplot))
    stop("byplot must be a single logical value(TRUE or FALSE)")

  # if (!is.character(id_col))
  #   stop("id_col must be a single character")
  # if (!is.logical(time_col))
  #   stop("time_col must be a single character")
  # if (!is.character(status_col))
  #   stop("status_col must be a single character")
  # if (byplot & !is.character(plot_col))
  #   stop("plot_col must be a single character")

  #Checks if columns are well specified and temporarily replace their names

  ## Census years
  data <- check_rename_variable_col(time_col, "time_col",data)
  # if (!time_col %in% names(data)) {
  #   stop(
  #     "The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it"
  #   )
  # }
  # else
  #   names(data)[which(names(data) == time_col)] <- "time"

  ## ids
  data <- check_rename_variable_col(id_col, "id_col",data)
  # if (!id_col %in% names(data)) {
  #   stop(
  #     "The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it"
  #   )
  # }
  # else
  #   names(data)[which(names(data) == id_col)] <- "id"

  ## alive code
  if(!use_size){
    data <- check_rename_variable_col(status_col, "status_col",data)
  }else data$status <- NA

  data$status_corr <- data$status
  # if (use_size != FALSE & !alive_col %in% names(data)) {
  #   stop(
  #     "The name you indicated (or let to default) for the tree vital status column is apparently not in the colnames of the dataset. You must specify it, or if it does not exist, use the argument use_size to create it from measurements under the hypothesis that only live trees were measured in your inventory"
  #   )
  # }
  # else
  #   names(data)[which(names(data) == alive_col)] <- "status"

  ## plots
  if(byplot) data <- check_rename_variable_col(plot_col, "plot_col",data)
  # if (byplot & !plot_col %in% names(data)) {
  #   stop(
  #     "The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it"
  #   )
  # }
  # else if (byplot)
  #   names(data)[which(names(data) == plot_col)] <- "plot"


  ##Other
  if (!(is.numeric(dead_confirmation_censuses) &
        length(dead_confirmation_censuses) == 1))
    stop("argument dead_confirmation censuses must be an integer scalar")



  # if (!"status" %in% names(data))
  #   data$status <- NA
  # data$status_corr <- data$status

  # Call internals by plot or not --------------------------------------------

  if (byplot) {
    plots <- unique(data$plot)
    data <- do.call(rbind, lapply(plots,
                                  function(p){
                                    .correct_status_plotlevel(data[which(data$plot == p), ], dead_confirmation_censuses, use_size)
                                  }))
    # for (p in plots) {
    #   data <-
    #     rbind(
    #       data[which(data$plot != p), ],
    #       .correct_status_plotlevel(data[which(data$plot == p), ], dead_confirmation_censuses, use_size)
    #     )
    # }
  }
  else
    data <- .correct_status_plotlevel(data, dead_confirmation_censuses, use_size)

  names(data)[which(names(data == "id"))] <- id_col
  names(data)[which(names(data == "time"))] <- time_col
  names(data)[which(names(data == "status"))] <- status_col
  if(byplot) names(data)[which(names(data == "plot"))] <- plot_col
  return(data)
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

#' Internal tree-level life status correction
#'
#' @param tree_temp a data.frame corresponding to a single tree's measurements, arranged by time
#' @param censuses
#' @param dead_confirmation_censuses
#' @param i character or numeric, but single value. the id of the tree that is being corrected in the function
#'
#' @return a data.frame containing the inputted individual-tree-level data with corrected life status. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the tree is unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.

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

#' Create vital status field from size measurements under the hypothesis that only live trees were measured.
#'
#' @param data_plot data.frame, a single-plot forest inventory whithout information on tree vital status, in which only live trees were measured, which names correspond to the format set in correct_alive, which is on the long format for census years - one line is one tree measured at one census.
#' @param use_size character, the name of the column containing tree size measurements - diameter of circumference.
#'
#' @return the same data.frame with a status field. status = 1 in any line where size is non-NA

.use_size_status <- function(data_plot, use_size) {
  data_plot$status <- NA
  data_plot$status[which(!is.na(data_plot[which(names(data_plot) == use_size)]))] <-
    1
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
