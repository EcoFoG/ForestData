correct_size <- function(data,
                         measure_col,
                         time_col,
                         alive_col,
                         # measure_type,
                         limit = 20){

# Checks and names handling -----------------------------------------------

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

  # if(measure_type == "C") data[which(names(data)==size_col),] <- data[which(names(data)==size_col),]/pi

# Calculate growth rate ---------------------------------------------------
  source("compute_growth.R")
  data <- .compute_growth(data)
  data$corrected_size <- data$size
  data$code_correction <- NA


# Identify outlyers -------------------------------------------------------

  outlyers <- .identify_outlyers(data, method)

# Loop on outlyers to correct them ----------------------------------------

  for(i in outlyers){
    data[which(data$id == i),c("corrected_size","code_correction")] <- correct_individual()
  }
}


ma_correction <- function(data,
                          size_col,
                          time_col,
                          status_col,
                          negative_threshold = -2,
                          positive_threshold = 5,
                          measure_type,
                          unit = "cm") {
  # Do we correct for YEARLY decrease over 2cm dbh or for BETWEEN CENSUSES decrease ?

}

mega_correction <- function(size,
                            time,
                            status,
                            negative_threshold = -2,
                            positive_threshold = 5,
                            unit = "cm") {
# Do we correct for YEARLY decrease over 2cm dbh or for BETWEEN CENSUSES decrease ?

  }


.replace_missing <- function(tree){
  miss <- which(is.na(tree$size) & status == 1)
  present <- which(!is.na(tree$size))
  for(i in miss){
    if(i < min(present)){
      size_val <- tree$size[present[1:min(2, sum(present))]]
      time_val <- tree$time[present[1:min(2, sum(present))]]
    }
    else if(i > max(present){

    })
  }
}
