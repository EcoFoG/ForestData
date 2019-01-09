correct_size <- function(data,
                         diameter_col = NA,
                         circumference_col = NA,
                         POM_col = NA,
                         time_col,
                         alive_col,
                         # limit = 20,  # unused argument in original code
                         method = "Thresholds", # Other arguments could be
                         positive_tresh_diameter = 5,
                         negative_thresh_diameter = -2,
                         ignore_POM = FALSE
                         ){

  # Checks ------------------------------------------------------------------

  # Trivial check of data arg
  if(!is.data.frame(data)){
    stop("data must be a dataframe")
  }

  # Checks and adaptations relative to circumference and diameter columns
  ##If no size column is specified, stop with explicit message
  if(is.na(diameter_col) & is.na(circumference_col)){
    stop("You must indicate the name of a column containing either circumference (argument circumference_col) or diameter (argument diameter col)")
  }
  else{
    ##If both size columns are specified, stop with explicit message
    if(!any(is.na(c(circumference_col,diameter_col)))){
      stop("You specified both diameter and circumference column names, please choose only one.")
    }
    else{
      ##Else, find which one is specified and check its validity, and stop with an explicit message if needed.
      if(is.na(diameter_col)){
        ## If diameter is NA then circumference is specified, then next step is to check if it is in data's column names
        if(diameter_col%in%names(data)){
          names(data[which(names(data) == circumference_col)]) <- "size" # tag size
        }
        else{
          ## If not, then stop
          stop("The name you indicated for trees' circumference column (argument circumference_col) is apparently not in the names of the dataset. Please specify the correct name for this column.")
        }
      }
      else if(is.na(circumference_col)){
        # Idem for circumference and diameter inverted.
        if(diameter_col%in%names(data)){
          names(data[which(names(data) == diameter_col)]) <- "size" # tag size
        }
        else{
          stop("The name you indicated for trees' diameter column (argument diameter_col) is apparently not in the names of the dataset. Please specify the correct name for this column.")
        }
      } ## Let an escape message in case of unexpected type error or exception.
      else stop("Unknown type exception relative to diameter_col and/or circumference_col argument(s). Please read the documentation.")
    }
  }


  if(!time_col%in%names(data)){
    stop("The name you indicated (or let to default) for the census year column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[which(names(data) == time_col)]) <- "time"

  if(!id_col%in%names(data)){
    stop("The name you indicated (or let to default) for the unique, individual tree IDs column is apparently not in the colnames of the dataset. Please specify it")
  }
  else names(data[which(names(data) == id_col)]) <- "id"

  if(!is.numeric(positive_tresh_diameter)){
    stop("The argument positive_thresh_diameter must be numeric, and contain the upper limit over which an annual growth rate is considered abnormally high")
  }
  else if(positive_tresh_diameter == 5){
    warning("The argument positive_thresh_diameter, which represents the upper limit over which a growth rate is considered abnormally high, is set to its default value. If you work on other plots than Paracou, we advise you to consider this point carefully.")
  }

  if(!is.numeric(negative_tresh_diameter)){
    stop("The argument negative_thresh_diameter must be numeric, and contain the lower limit under which an annual diameter growth rate is considered abnormally low (negative)")
  }
  else if(negative_tresh_diameter == -2){
    warning("The argument negative_thresh_diameter, which represents the lower limit under which an annual diameter growth rate is considered abnormally low(negative), is set to its default value. If you work on other plots than Paracou, we advise you to consider this point carefully.")
  }

  if(is.na(POM_col)){
    if(!ignore_POM){
      stop("You did not indicate any name for the column containing POM measurements. If you wish to perform the correction without using this source of information, you can set the argument ignore_POM to TRUE (unadvised and at your own risks).")
    }
    else{
      warning("Keep aware that you do not use POM measurements to perform the corrections, which lowers the reliability of the corrections performed by this function. Take it on your own responsibility.")
    }
  }
  else if(!POM_col %in% names(data)){
    stop("The name you indicated for the column containing POM values (argument POM_col) is not in data's column names. Please specify it correctly.")
  }
  else{
    names(data[which(names(data) == POM_col)]) <- "POM"
  }


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

  # First step to correct explicitely with POM if they exist

  # Then correct the other cases as in mega_correction
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


