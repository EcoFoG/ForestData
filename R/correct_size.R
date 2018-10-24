correct_size <- function(data,
                         measure_col,
                         time_col,
                         alive_col,
                         # limit = 20,  # unused argument in original code
                         method = "Thresholds", # Other arguments could be
                         positive_tresh = 5,
                         negative_thresh = -2
                         ){

  # Checks ------------------------------------------------------------------

  if(!is.data.frame(data)){
    stop("data must be a dataframe")
  }

  if(!measure_col%in%names(data)){
    stop("The name you indicated (or let to default) for tree sizes column (diameter or circumference) is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[which(names(data) == measure_col)]) <- "size"

  if(!time_col%in%names(data)){
    stop("The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[which(names(data) == time_col)]) <- "time"

  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[which(names(data) == id_col)]) <- "id"


# Ordering by and extracting ID -------------------------------------------

data <- data[order(id, time),]
ids <- unique(data$id)

# Creation of new columns -------------------------------------------------

## Corrected circumference column
# code 0 = no correction
# code 1 = increase >5cm/yr + return
# code 2 = increase >5cm/yr + 2 series
# code 3 = decrease > 2cm + return
# code 4 = decrease >2cm + 2 series
data[,paste0("corrected_",measure_col)] <- data$size

## Flagging of corrected values with associated code
data$code_corr <- rep(0,nrow(data))

# First calculation of growth rate ----------------------------------------


# Outlyer detection -------------------------------------------------------

### Thresholds

### With explicit POM changes

### Using distributions quantiles

### Using automated outlyers detections methods for time series (from litterature and Avner)


# Correct_outlyers --------------------------------------------------------


}

# Internals ---------------------------------------------------------------
.correct_tree_size <- function(tree){

}



.replace_missing <- function(tree){ #function(size, time,status)

  tree <- tree[order(time),c("time","size","status")] # in case data is not ordered yet
  missing <- which(is.na(tree$size) & status == 1) # indices of the missing values
  present <- !is.na(tree$size) # to simplify the code written hereafter

  corrected_values <- rep(NA, length(tree$size))

  # correct each value - apply replaced by for: faster and also clearer.
  for(i in missing){
    if(i < min(which(present))){
      size_val <- tree$size[which(present)[1:min(2, sum(present))]]
      time_val <- tree$time[which(!is.na(tree$size))[1:min(2, sum(present))]]
    }
    else if(i > max(which(present))){
      size_val <- tree$size[which(present)[(sum(present)-1):sum(present)]]
      time_val <- tree$time[which(present)[(sum(present)-1):sum(present)]]

      size_val <- size_val[!is.na(size_val)]
      time_val <- time_val[!is.na(size_val)] ## Something weird here in C's original code
    }
    else{
      size_val <- tree$size[c(max(which(!is.na(tree$size[1:(i-1)]))), i+min(which(!is.na(tree$size[(i+1):length(tree$size)]))))]
      time_val <- tree$time[c(max(which(!is.na(tree$size[1:(i-1)]))), i+min(which(!is.na(tree$size[(i+1):length(tree$size)]))))]
    }

    reg <- lm(size_val ~ time_val)$coef
    corrected_values[i] <- reg[1] + reg[2]*tree$time[i]

    if (sum(!is.na(time_val))==1) {
      corrected_values[i] <- size_val
    }
  }
  return(corrected_values)
}


