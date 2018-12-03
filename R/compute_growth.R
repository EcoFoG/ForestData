compute_growth_diameter <- function(data,
                                    id_col,
                                    time_col,
                                    size_col,
                                    measure_type){

# Checks ------------------------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a data.frame class object")

  if(!id_col%in%names(data)){
    stop("wrong name for the column containing unique tree IDs")
  }
  else{
    names(data[which(names(data) == id_col)]) <- "id"
  }

  if(!time_col%in%names(data)){
    stop("wrong name for the column containing census year")
  }
  else{
    names(data[which(names(data) == time_col)]) <- "time"
  }

  if(!size_col%in%names(data)){
    stop("wrong name for the column containing tree size measure")
  }
  else{
    names(data[which(names(data) == size_col)]) <- "size"
  }

  if(!is.character(measure_type))
    stop("measure type must be a character: C or D for circumference and diameter, respectively")

  if(measure_type == "C") data[which(names(data)==size_col),] <- data[which(names(data)==size_col),]/pi
# Compute -----------------------------------------------------------------

  data <- .compute_growth(data[order(data$id, data$size),])

  names(data$size) <- size_col
  names(data$time) <- time_col
  names(data$id) <- id_col

  return(data)
}
compute_growth_circumference <- function(data,
                                    id_col,
                                    time_col,
                                    size_col,
                                    measure_type){

  # Checks ------------------------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a data.frame class object")

  if(!id_col%in%names(data)){
    stop("wrong name for the column containing unique tree IDs")
  }
  else{
    names(data[which(names(data) == id_col)]) <- "id"
  }

  if(!time_col%in%names(data)){
    stop("wrong name for the column containing census year")
  }
  else{
    names(data[which(names(data) == time_col)]) <- "time"
  }

  if(!size_col%in%names(data)){
    stop("wrong name for the column containing tree size measure")
  }
  else{
    names(data[which(names(data) == size_col)]) <- "size"
  }

  if(!is.character(measure_type))
    stop("measure type must be a character: C or D for circumference and diameter, respectively")

  if(measure_type == "D") data[which(names(data)==size_col),] <- data[which(names(data)==size_col),]*pi
  # Compute -----------------------------------------------------------------

  data <- .compute_growth(data[order(data$id, data$size),])

  names(data$size) <- size_col
  names(data$time) <- time_col
  names(data$id) <- id_col

  return(data)
}
.compute_growth_complete_data <- function(data){

  data <- data[order(data$id, data$size),]

  data$size_leaded <- c(data$size[-nrow(data)],NA)
  data$time_leaded <- c(data$time[-nrow(data)],NA)
  data$id_leaded <- c(data$id[-nrow(data)],NA)

  data$growth_absolute <- NA
  data$growth_annual <- NA

  data$growth_absolute[which(data$id_leaded == data$id)] <- data$size_leaded - data$size
  data$growth_annual[which(data$id_leaded == data$id)] <- (data$size_leaded-data$size)/(data$time_leaded - data$time)

  rm(list = c(data$size_leaded,data$id_leaded, data$time_leaded))

  return(data)

}
.compute_growth_gruyere <- function(data){
  data <- data[order(data$id, data$size),]

  data$size_leaded <- c(data$size[-nrow(data)],NA)
  data$time_leaded <- c(data$time[-nrow(data)],NA)
  data$id_leaded <- c(data$id[-nrow(data)],NA)

  data$growth_absolute <- NA
  data$growth_annual <- NA

  mismatch_ids <- data$id_leaded != data$id
  cresc_indices <- data$

  data$growth_absolute[which(!is.na(data$size))[-1]-1] <- data$size_leaded - data$size
  data$growth_annual[which(!is.na(data$size))[-1]-1] <- (data$size_leaded-data$size)/(data$time_leaded - data$time)

  # data$growth_absolute
  return(data)
}
