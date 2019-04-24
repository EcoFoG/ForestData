#' Compute Tree Annual Diameter Growth
#'
#' compute_growth function computes annual diameter increment for every tree per census interval
#'
#' @param data A data.frame containing a time-series tree-wise forest inventory -i.e. every line is a single tree measurement for a single year.
#' @param id_col Character. The name of the column containing trees unique ids
#' @param time_col Character. The name of the column containing census year
#' @param size_col A single character containing the name of the column corresponding to tree size measurements -either circumference or diameter.
#' @param measure_type A single character indicating whether tree sizes are given in circumferences -"C"- or diameter -"D"-.
#'
#' @return a data.frame with annual and absolute inter-census diameter increments. It has N-n rows, where N is the input dataset's nrow, and n the number of individual trees measurement in the dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' data("Paracou6")
#' compute_growth_diameter(Paracou6,
#' id_col = "idTree",
#' time_col = "CensusYear",
#' size_col = "CircCorr",
#' measure_type = "C")
#' }

compute_growth_diameter <- function(data,
                                    id_col = "idTree",
                                    time_col = "CensusYear",
                                    size_col = "CircCorr",
                                    measure_type = "C"){

# Checks ------------------------------------------------------------------
  if(!is.data.frame(data)) stop("data must be a data.frame")

  if(!id_col%in%names(data)){
    stop("wrong name for the column containing unique tree IDs")
  }
  else{
    names(data)[which(names(data) == id_col)] <- "id"
  }

  if(!time_col%in%names(data)){
    stop("wrong name for the column containing census year")
  }
  else{
    names(data)[which(names(data) == time_col)] <- "time"
  }

  if(!size_col%in%names(data)){
    stop("wrong name for the column containing tree size measure")
  }
  else{
    names(data)[which(names(data) == size_col)] <- "size"
  }

  if(!is.character(measure_type))
    stop("measure type must be a character: C or D for circumference and diameter, respectively")

  if(measure_type == "C") data[,which(names(data)==size_col)] <- data[,which(names(data)==size_col)]/pi
# Compute -----------------------------------------------------------------
# print(utils::head(data$id))
  # print(utils::head(data$size))
  data <- .compute_growth_complete_data(data[order(data$id, data$size),])

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

  data <- .compute_growth_complete_data(data[order(data$id, data$size),])

  names(data$size) <- size_col
  names(data$time) <- time_col
  names(data$id) <- id_col

  return(data)
}


.compute_growth_complete_data <- function(data){

  data <- data[order(data$id, data$time),]

  data$size_lag <- c(NA,data$size[-nrow(data)])
  data$time_lag <- c(NA,data$time[-nrow(data)])
  data$id_lag <- c(NA,data$id[-nrow(data)])

  data$growth_absolute <- NA
  data$growth_annual <- NA

  indices <- which(data$id_lag == data$id)
  data$growth_absolute[indices] <- data$size[indices] - data$size_lag[indices]
  data$growth_annual[indices] <- (data$size[indices]-data$size_lag[indices])/(data$time[indices] - data$time_lag[indices])

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
