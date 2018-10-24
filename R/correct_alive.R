#' Title
#'
#' @param data A data.frame containing a forest census, to correct for tree's status.
#' @param measure_col character - The name of the column containing
#' @param id_col
#' @param time_col
#' @param alive_col
#' @param dead_confirmation_censuses
#' @param flag_corrections
#'
#' @return
#' @export
#'
#' @examples
correct_alive <- function(data,
                          measure_col = "Circ",
                          id_col = "idTree",
                          time_col = "CensusYear",
                          alive_col = "CodeAlive",
                          ignore_na_before = FALSE,
                          dead_confirmation_censuses = 2,
                          flag_corrections = T){

  # Safety checks -----------------------------------------------------------

  if(!measure_col%in%names(data)){
    stop("The name you indicated (or let to default) for tree sizes column (diameter or circumference) is apparently not in the colnames of the dataset. Please specify it")
  }
  if(!time_col%in%names(data)){
    stop("The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it")
  }
  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }


  # Preparation and protocol handling + checks ------------------------------

  ## correction_alive is a tagging variable that allow to flag the individuals and years that were corrected.
  if(flag_corrections) data$correction_alive <- FALSE

  if(!alive_col%in% names(data)){
    if(alive_col == "AliveCode"){
      warning("You seem not to have indicated the name of the column containing tree status (0: dead, 1:alive). It will be created from the measurement column")
      return(.data_status_from_measurements(data, measure_col, id_col,time_col,flag_corrections))
    }
    else stop("The name you indicated for the column containing tree status (0: dead, 1:alive) is not part in the colnames of the dataset. Please indicate it")
  }
  else{
    return(.correct_alive_data(data, time_col,id_col,alive_col,flag_corrections))
  }
}



# Internals ---------------------------------------------------------------

# Correct tree status from existing column: loop on tree ids

.correct_alive_data <- function(data,
                                # measure_col = "Circ",
                                time_col = "CensusYear",
                                id_col = "idTree",
                                alive_col = "CodeAlive",
                                flag_corrections){

  # data[,which(names(data) == measure_col)] <- "measure"
  data[,which(names(data) == time_col)] <- "time"
  data[,which(names(data) == id_col)] <- "id"
  data[,which(names(data) == alive_col)] <- "alive"

  data$status <- data$alive
  data <- data[with(data,order(id, time)),]

  ids <- unique(data$id)

  if(flag_corrections) data$correction_alive <- F

  for(i in 1:length(ids)){
    #Careful with the order: looks safe but to test. Safer would be to store the corrected tree in a temp objec and reattribute only the relevant vars, but it is less optimized
    data[which(data$id == ids[i]),] <- .correct_tree_status(data[which(data$id == ids[i]),],
                                                            flag_corrections)
  }

  data[,which(names(data) == "measure")] <- measure_col
  data[,which(names(data) == "time")] <- time_col
  data[,which(names(data) == "id")] <- id_col
  data[,which(names(data) == "alive")] <- alive_col

  return(data)
}

# Correct individual tree status

.correct_tree_status <- function(tree,
                                 flag_corrections){

  #Check if the tree is still alive at the last census
  #Here we could also flag when tree is dead at last census but alive in the penultimate census ?

  if(tree$alive[nrow(tree)] == 1){
    last <- nrow(tree)
  }
  else{
    #If not, find the latest measurement of the tree being alive, browsing observations from the last census to this date
    for(l in (nrow(tree)-1):1){
      if(tree$alive[l] == 1){
        last <- l
        break
      }
    }
  }

  #Then, find the first census at which the tree is declared alive.

  ######################## NORMALLY ###########################################################
  ##In Paracou database, a tree is not a posteriori registered with status NA                ##
  ## for the censuses preceding the one it is recruited at. Thus if tree is declared dead    ##
  ## or NA at first recruitment, this is an error to correct manually.                       ##
  ######################### BUT ###############################################################
  ##In some datasets, we can imagine that every tree has a line corresponding to each census.##
  ## This can be useful, for example to work on mortaliy rates etc. In this case, trees which##
  ## were not recruited at first census would have a NA at the first registered status. This ##
  ## is why I propose to throw a warning in this situation EXCEPT if the user specifies that ##
  ## everything is normal.                                                                   ##
  #############################################################################################

  if(!all(tree$alive == 0) & !all(is.na(tree$alive))){
    if(!tree$alive[1]==1){
      if(!ignore_na_before){
        #stop or warning ?
        stop(paste0("The tree which ID is ",tree$id," has a ",ifelse(tree$alive[1] == 0), "DEAD ",ifelse(is.na(tree$alive[1])), "NA ", "UNKNOWN "), "status at first measurement. If unrecruited trees appear in your table with a code indicating that it is unrecruited, make sure it is NA (and no other code) and set the argument ignore_na_before to TRUE. Else, correct this.")
      }
      else{
        for(y in 1:last){
          if(tree$alive[y]==1){
            first == y
          }
        }
      }
    }
    else first <- 1
  }


  if(first<last-1){
    tree$alive[first+1:last-1] <- 1
    if(flag_corrections) tree$correction_alive[first+1:last-1] <- 1
  }


  return(tree)
}


# Recalculate status column from circ or diam measurements: loop on tree ids

.data_status_from_measurements <- function(data, measure_col = "Circ", time_col = "CensusYear", id_col = "idTree",flag_corrections){

  data[,which(names(data) == measure_col)] <- "measure"
  data[,which(names(data) == time_col)] <- "time"
  data[,which(names(data) == id_col)] <- "id"

  data$status <- rep(NA, nrow(data))
  data <- data[with(data,order(id, time)),]

  ids <- unique(data$id)

  if(flag_corrections) data$correction_alive <- T

  for(i in 1:length(ids)){
    data[which(data$id == ids[i]), "status"] <- .tree_status_from_measurements(data$measure[which(data$id == ids[i])])
  }

  data[,which(names(data) == "measure")] <- measure_col
  data[,which(names(data) == "time")] <- time_col
  data[,which(names(data) == "id")] <- id_col

  return(data)
}


# Recalculate status from circ or diam measurements: individual tree level

.tree_status_from_measurements <- function(X){
  status<-rep(NA, length(X))
  ### first to last alive measure (even missing dbh values)
  status[(min(which(!is.na(X)))):(max(which(!is.na(X))))]<-1
  ### dead trees : last value = NA
  if (max(which(!is.na(X)))<length(X)){
    status[(max(which(!is.na(X)))+1):length(X)]<-0
  }
  return(status)
}
