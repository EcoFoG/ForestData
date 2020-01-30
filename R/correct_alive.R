#' Correct Trees Life Status in a Forest Inventory
#'
#' correct_alive spots unseen trees, adds corresponding lines and corrects their
#' status.
#'
#' @inheritParams correct_all
#'
#' @details
#'
#' \strong{\emph{Argument \code{death_confirmation_censuses}}}
#' \code{death_confirmation_censuses} is an argument that comes from the Paracou
#' forest plots' censusing protocol, in which tree death is stated with
#' certainty only if unsighting happens for at least the two last censuses. This
#' is because the temporal resolution -the frequency with which the plots are
#' censused- is high enough to use this cross-verification rule in case of
#' unsighting. This means that mortality rates cannot be calculated with
#' certainty for the last census, and that is why this argument is also in
#' \code{compute_mortality} and \code{compute_rates} functions. Please set this
#' argument according to your protocol's resolution and exigencies.
#'
#' \code{use_size} defaults to FALSE, and activates a specific internal function
#' that creates a \code{$status} field in the dataset. If you use this option,
#' make SURE that only LIVE trees are measured (with non-NA size) in your
#' dataset's protocol. In several protocols, e.g. the Paracou Disturbance
#' Experiment, dead trees are measured for the census when death is recorded. In
#' this case, this option must not be activated, and the status field has to be
#' created manually. If your data ALREADY contains a field indicating whether
#' the tree is dead -0 or FALSE-, or alive -1 or TRUE-, please let
#' \code{use_size} to its default value.
#'
#' @return a data.frame containing the corrected data, with trees' corrected
#'   life statuses. 1 = alive, 0 = dead. NAs indicate that the tree was unseen
#'   and cannot be considered dead yet. The output does not necessarily have the
#'   same number of lines as the input. Lines are added when the trees were
#'   unseen then seen alive again, with all columns being set NA except trees'
#'   id, plot, census year, corrected status, and the columns specified in
#'   \code{invariant_columns} argument. Useless lines, with NA status before
#'   first sight alive, or after death statement, are suppressed.
#' @export
#'
#' @examples
#' #Load the provided example dataset
#' data("example_census")
#'
#' #Take a look to its structure
#' str(example_census)
#'
#' #Correct it (short version with column names set with prepare_forestdata)
#'
#' prepare_forestdata(example_census,
#' plot_col="Plot",
#' id_col="idTree",
#' time_col="CensusYear",
#' status_col = "CodeAlive",
#' size_col="Circ",
#' measure_type = "C",
#' POM_col = "POM")
#'
#' example_status_corr <- correct_alive(example_census,
#' invariant_columns = c("Genus",
#' "Species",
#' "Family",
#' "Forest",
#' "binomial_name"))
#'
#' #Correct it (full call)
#' example_status_corr <- correct_alive(example_census,
#' id_col = "idTree",
#' time_col = "CensusYear",
#' status_col = "CodeAlive",
#' plot_col = "Plot",
#' byplot = TRUE,
#' dead_confirmation_censuses = 2,
#' use_size = FALSE,
#' invariant_columns = c("Genus",
#' "Species",
#' "Family",
#' "Forest",
#' "binomial_name"))
#'
#'
#' str(example_status_corr)
correct_alive <- function(data,
                          id_col = ifelse(is.null(getOption("id_col")), "idTree",getOption("id_col")),
                          time_col = ifelse(is.null(getOption("time_col")), "CensusYear",getOption("time_col")),
                          status_col = ifelse(is.null(getOption("status_col")), "CodeAlive",getOption("status_col")),
                          plot_col = ifelse(is.null(getOption("plot_col")), "Plot",getOption("plot_col")),
                          byplot = TRUE,
                          dead_confirmation_censuses = 2,
                          use_size = FALSE,
                          invariant_columns = c("Forest",
                                                "Family",
                                                "Genus",
                                                "Species",
                                                "binomial_name")){
  # Checks and preparation --------------------------------------------------
  names_args <- c(id_col, time_col, status_col, plot_col)
  for(n in 1:length(names_args))
    if(is.null(names_args[n]))
      stop(paste0("The following argument(s) need to be specified:",c("id_col", "time_col", "status_col", "plot_col")[n]))

  for(n in invariant_columns){
    if(!n %in% names(data)){
      stop(paste("invariant_columns argument must contain one or several column names (see help).",n,"is apparently not a dataset's column"))
    }
    else if(n %in% names_args){
      message(paste0("Note that you must not specify", n,
                     " as an invariant column since this name already corresponds to ",
                     c("id_col","time_col","status_col","plot_col")[which(names_args == n)]))
      invariant_columns <- invariant_columns[-which(invariant_columns == n)]
    }

  }
  # Trivial check of data arg
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }

  if (!is.logical(use_size))
    stop("use_size must be a single logical value(TRUE or FALSE)")
  if (!is.logical(byplot))
    stop("byplot must be a single logical value(TRUE or FALSE)")


  #Checks if columns are well specified and temporarily replace their names

  ## Census years
  data <- check_rename_variable_col(time_col, "time",data)


  ## ids
  data <- check_rename_variable_col(id_col, "id",data)


  ## alive code
  if(!use_size){
    data <- check_rename_variable_col(status_col, "status",data)
  }else data$status <- NA

  ## Initialize status_corr
  data$status_corr <- data$status

  ## plots
  if(byplot){
    data <- check_rename_variable_col(plot_col, "plot",data)
    ischar <- is.character(data$plot)
    if(!ischar){
      initial_class <- class(data$plot)
      data$plot <- methods::as(data$plot, "character")
    }
  }



  ##Other
  if (!(is.numeric(dead_confirmation_censuses) &
        length(dead_confirmation_censuses) == 1))
    stop("argument dead_confirmation censuses must be an integer scalar")

  # data <- data[order(data$id, data$time),]

  # if (!"status" %in% names(data))
  #   data$status <- NA
  # data$status_corr <- data$status

  # Call internals by plot or not --------------------------------------------

  if (byplot){

    plots <- unique(data$plot)
    #create progressbar
    pb <- utils::txtProgressBar(min = 0, max = length(plots), style = 3)






    data <- do.call(rbind,
                    lapply(plots,
                           function(p){
                             # Update between-plots progress bar
                             utils::setTxtProgressBar(pb, which(plots == p), title = "Plot")
                             .correct_status_plotlevel(data[which(data$plot == p), ],
                                                       dead_confirmation_censuses,
                                                       use_size,
                                                       invariant_columns,
                                                       plots,
                                                       p)
                             }))
    close(pb)
  }
  else
    data <- .correct_status_plotlevel(data,
                                      dead_confirmation_censuses,
                                      use_size,
                                      invariant_columns,
                                      plots = NULL,
                                      p = NULL)


  # print(names(data))
  names(data)[which(names(data) == "id")] <- id_col
  names(data)[which(names(data) == "time")] <- time_col
  names(data)[which(names(data) == "status")] <- status_col
  if(byplot){
    if(!ischar){
      data$plot <- methods::as(data$plot, initial_class)
    }
    names(data)[which(names(data) == "plot")] <- plot_col
  }
  return(data)
}


# Internals ---------------------------------------------------------------

# Internal plot-level life status correction
#
# @param data_plot data.frame, a single-plot forest inventory which names correspond to the format set in correct_alive, which is on the long format for census years - one line is one tree measured at one census. It already contains a field named status_corr, which as this point is just a copy of the raw, uncorrected status field
# @param dead_confirmation_censuses numeric scalar, the number of "unsighting" censuses from which we state that the tree is almost certainly dead. In Paracou, we set it to 2 considered censusing rythm and the experience of the censusing crew
# @param use_size Character, but defaults to FALSE. Optional argument specifying that circumference or diameter must be used to create the vital status field. If your data already contains a field indicating whether the tree is dead -0 or FALSE- or alive -1 or TRUE-, let it to its default value. If you use this option, make sure beforehand that only live trees are measured - non-NA size - in your dataset's protocol.
#
# @return a data.frame containing the inputted plot-level data with trees' corrected life statuses. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the tree is unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.

.correct_status_plotlevel <- function(data_plot, dead_confirmation_censuses, use_size,invariant_columns,plots,p){

  if(length(plots) > 1){
    message <- paste0("Correcting plot ",p," : ",which(plots == p),"/",length(plots))
  }

  if(use_size != FALSE){
    if(!use_size %in% names(data_plot)){
      stop("use_size defaults to FALSE, but to activate this option, it must contain the name of the column containing circumference or diameter measurements")
    }
    else{
      data_plot <- .use_size_status(data_plot, use_size)
    }
  }

  censuses <- sort(unique(data_plot$time), decreasing = FALSE)
  # print(censuses)
  ids <- unique(data_plot$id)
  data_plot <- data_plot[order(data_plot$id, data_plot$time),]

  data_plot <- do.call(rbind,lapply(ids,function(i) .correct_alive_tree(data_plot[which(data_plot$id == i),],
                                                                        censuses,
                                                                        dead_confirmation_censuses,
                                                                        i,
                                                                        invariant_columns)))
  return(data_plot)
}

# Internal tree-level life status correction
#
# @param tree_temp a data.frame corresponding to a single tree's measurements, arranged by time
# @param censuses numeric, censuses for the plot in which the tree is.
# @param dead_confirmation_censuses see correct_alive
# @param i character or numeric, but single value. the id of the tree that is being corrected in the function
#
# @return a data.frame containing the inputted individual-tree-level data with corrected life status. 1 = alive, 0 = dead. NAs indicate that the tree was unseen and cannot be considered yet. The output does not necessarily have the same number of lines as the input. Lines are added when the tree is unseen then seen alive again, with all columns being NA except trees' id, plot, census year and corrected status. Useless lines -with NA status before first sight alive, or after death- are suppressed.

.correct_alive_tree <- function(tree_temp, censuses, dead_confirmation_censuses, i, invariant_columns){
  #Store the names of the measured variables for tree_temp, for later use
  vars <- names(tree_temp)
  # tree_tempsav <- tree_temp
  if(!length(unique(tree_temp$plot)) == 1){
    stop(paste0("tree ",unique(tree_temp$id)," has multiple plots: " ,paste0(unique(tree_temp$plot), collapse = "/")))
  }

  #Find when the tree is first recorded ALIVE
  first_record <- ifelse(any(tree_temp$status==1),min(tree_temp$time[which(tree_temp$status == 1)]), NA)



  if(is.na(first_record)){ #Sometimes, there is no first record. It can correspond to two cases for which we warn explicitely. We choosed not to stop the function for these cases. The user must be careful enough to read the warning messages.
    if(all(is.na(tree_temp$status))){ #It could be that the tree's status is always NA, which is unlikely to happen but we still take this into consideration. Who knows?
      message <- paste0("tree ",i," has only NA life status. If it is a isolated outlyer, please manually check it. If there is no life status column in your dataset, you can create it from size measurement (see the vignette)")
    }else if(all(isFALSE(tree_temp$status))){ #It could be that recruitment AND death happened on the SAME between-censuses interval.
      message <- paste0("tree ",i," has only been recorded dead. It might be that it has been recruited and died on the same between-censuses interval. Please verify it")
    }
    warning(message)
  }else{
    #If there is an actual first record, let's take a look to when the tree was not seen (i.e. no corresponding line)

    if(any(!is.na(tree_temp$status) & tree_temp$status == 0)){ # if tree has ever been recorded dead
      last_death_record <- max(tree_temp$time[!is.na(tree_temp$status) & tree_temp$status==0]) # we take the last census for which it has been recorded dead (in case there are several)
      # print(last_death_record)
      if(any(tree_temp$time > last_death_record)){ # We then test if lines exist for censuses after last tree death
        after <- which(tree_temp$time > last_death_record) #let's call these censuses "after" if they exist
        if(any(!is.na(tree_temp$status[after]) & tree_temp$status[after] == TRUE)){ #If there is any "alive" report after last reported death
          absents <- (censuses > first_record & !censuses %in% tree_temp$time) #  we search for unsightings in all the censuses.
        }
        else{ # else we just search up to last death record...
          absents <- (censuses > first_record &
                        censuses < last_death_record &
                        !censuses %in% tree_temp$time)
        }
      }
      else{ #idem, if there are no lines for censuses ulterior to tree death report, we search for unsightings until tree death.
        absents <- (censuses > first_record &
                      censuses < last_death_record &
                      !censuses %in% tree_temp$time)
      }

    }
    else{ #if tree has not been reported dead yet, we search in all censuses
      absents <- (censuses > first_record & !censuses %in% tree_temp$time)
    }
# if(i == 75599){
#   print(absents)
#   print("-----------------------")
# }

    nabs <- sum(absents) # absent is a logical vector giving the census times for which trees were not seen.
    if(nabs > 0){
      # if(tree_temp$plot[1] == 1) print(tree_temp$plot[1])
      if("plot" %in% vars){
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = NA,
                               status_corr = NA,
                               plot = unique(tree_temp$plot),
                               stringsAsFactors =  FALSE)
        newnames <- c("id","time","status","status_corr","plot")

      }
      else{
        new.rows <- data.frame(id = i,
                               time = censuses[absents],
                               status = NA,
                               status_corr = NA,
                               stringsAsFactors = FALSE)
        newnames <- c("id","time","status","status_corr")
      }

      if(length(invariant_columns) > 0){
        # new.rows[,vars[-which(vars%in%newnames)]] <- NA
        new.rows[,invariant_columns] <- NA
        # if(i == 75599){
        #   print(new.rows)
        #   print("-----------------------")
        # }
        # print(invariant_columns)
        # new.rows[,vars[vars%in%invariant_columns]] <- reattribute_invariant_columns(new.rows = new.rows,
        #                                                                             invariant_columns = invariant_columns,
        #                                                                             tree_temp = tree_temp,
        #                                                                             i=i)
        new.rows <- reattribute_invariant_columns(new.rows = new.rows,
                                                  invariant_columns = invariant_columns,
                                                  tree_temp = tree_temp,
                                                  i=i)
      }
      # print(new.rows)
      # if(i == 75599){
      #   # print(tree_tempsav)
      #   print("############rows")
      #   print(c(ncol(new.rows),ncol(tree_temp)))
      # }
      new_rows_init <- (nrow(tree_temp)+1)
      new_rows_end <- (nrow(tree_temp)+nabs)
      tree_temp[new_rows_init:new_rows_end,vars] <- NA
      tree_temp[new_rows_init:new_rows_end,newnames] <- new.rows[,newnames]
      # if(i == 75599){
      #   # print(tree_tempsav)
      #   print("###########tree1")
      #   print(tree_temp)
      # }
      if(length(invariant_columns) > 0) tree_temp[new_rows_init:new_rows_end,invariant_columns] <- new.rows[,invariant_columns]

      tree_temp <- tree_temp[order(tree_temp$time),]
      # if(i == 75599){
      #   # print(tree_tempsav)
      #   print("############tree2")
      #   print(tree_temp)
      # }
    }
    if(!all(is.na(tree_temp$status))){
      if(all(!tree_temp$status)){
        print(tree_temp$status)
        print("#")
      }
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

# Create vital status field from size measurements under the hypothesis that only live trees were measured.
#
# @param data_plot data.frame, a single-plot forest inventory whithout information on tree vital status, in which only live trees were measured, which names correspond to the format set in correct_alive, which is on the long format for census years - one line is one tree measured at one census.
# @param use_size character, the name of the column containing tree size measurements - diameter of circumference.
#
# @return the same data.frame with a status field. status = 1 in any line where size is non-NA

.use_size_status <- function(data_plot, use_size) {
  data_plot$status <- NA
  size_index = which(names(data_plot) == use_size)
  rows_alive <- which(!is.na(data_plot[,size_index]))
  data_plot$status[rows_alive] <- 1
  return(data_plot)
}


# Minor enhancements that can be done -------------------------------------

# Secure the sub-functions to be used independently by the users that really want to test it ?
## i.e. making them relatively independant from above-level formating and ordering

# Create a status_correction_code to indicate the type of correction and the reason why ?
## can help flagging the "unseen not yet statable as dead" without loosing status_corr 's logical datatype
## but is maybe of low informative power, thus could be set as an option.
## but if we would do an option for every case like this, the arglist would be super long !



# Title
#
# @param new.rows
# @param invariant_columns
# @param tree_temp
# @param i
#
# @return
#
# @examples
reattribute_invariant_columns <- function(new.rows, invariant_columns, tree_temp,i){
  for(j in invariant_columns){
    # print(invariant_columns)
    # print("ok")
    # print(j)
    # print(new.rows)

    if(any(is.na(new.rows[,j]))){
      uni <- unique(tree_temp[, j])
      if(any(is.na(uni)))
        warning(paste0("The tree ",i," has NA values for the variable ",j, " that is supposed to be invariant"))
      uni <- uni[which(!is.na(uni))]
      if(length(uni) > 1){
        message = paste0("attribute ",
                         j,
                         " that you defined as a non-varying column -i.e. supposed to have always the same value for each measurement of the same tree- has multiple values for tree ",
                         i,
                         " and takes the values ",
                         uni)
        stop(message)
      }
      else if(length(uni) == 0){
        stop(paste0("Attribute ",j," has no replacement value for individual ",i))
      }
      else{
        new.rows[which(is.na(new.rows[,j])),j] <- uni
        # new.rows[which(is.na(new.rows[,which(names(new.rows) == j)])),which(names(new.rows) == j)] <- uni
      }
    }
  }
  # ret <- new.rows[,names(new.rows)%in%invariant_columns]
  # print(ret)
  return(new.rows)
}



# deprec ------------------------------------------------------------------
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

# reattribute_fixed_informations <- function(data,
#                                            fixed_attributes = c("Genus","Species","Plot","SubPlot","Plotsub"),
#                                            id_col = "idTree"){
#   names(data)[which(names(data) == id_col)] <- "id"
#   ids <- unique(data[,"id"])
#   data
#   for(i in ids){
#     for(j in fixed_attributes){
#       if(any(is.na(data[which(data$id == i), j]))){
#         print(i)
#         uni <- unique(data[which(data$id == i), j])
#         uni <- uni[which(!is.na(uni))]
#         if(length(uni) > 1){
#           message = paste0("attribute ",
#                            j,
#                            " that you defined as a non-varying column -i.e. supposed to have always the same value for each measurement of the same tree- has multiple values for tree ",
#                            i,
#                            " and takes the values ",
#                            uni)
#           stop(message)
#         }
#         else{
#           print(paste0("uni ",uni))
#
#           print(c(which(names(data) == j)))
#           data[which(data$id == i & is.na(data[,which(names(data) == j)])),which(names(data) == j)] <- uni
#         }
#       }
#     }
#   }
#   names(data)[which(names(data) == "id")] <- id_col
#   return(data)
# }
#

# @param data data.frame, Forest inventory in the form of a long-format time
#   series - one line is a measure for one individual during one census time.
# @param id_col character, The name of the column containing trees unique ids
# @param time_col character, The name of the column containing census year
# @param status_col character, The name of the column containing tree vital
#   status - 0=dead; 1=alive.
# @param plot_col character, The name of the column containing the plots
#   indices.
# @param byplot logical, If there are several plots in your dataset, the
#   correction is performed by plot, in case these would not be censuses the
#   same years or with the same frequencies one another.
# @param dead_confirmation_censuses integer, defaults to 2. This is the number
#   of censuses needed to state that a tree is considered dead, if unseen. In
#   Paracou, we use the rule-of-thumb that if a tree is unseen twice, its
#   probability to be actually dead is close to 1. The choice of this value
#   involves that trees unseen during the X-1 last inventories can not be
#   corrected for death, and thus mortality rates should not be calculated for
#   these censuses.
# @param use_size character, but defaults to FALSE. Optional argument
#   specifying that circumference or diameter must be used to create a vital
#   status field. To use with care, see Details section FIRST.
# @param invariant_columns character vector, containing the name of the columns
#   that do not vary among an individual tree's measurement, for example
#   species name, coordinates, or any information that is necessary for the
#   user to manipulate the dataset. When a tree is unseen for a given census,
#   then seen alive later on, the corresponding lines are added to the dataset,
#   with DEFINED values for status_corr, id, time and plot -if corrected with
#   \code{byplot=TRUE}-, but all the other variables are NA. Specifying which columns are invariant enables the completion of the lines added for. Defaults to null, but it is vigourously recommended to
#   set it appropriately.
