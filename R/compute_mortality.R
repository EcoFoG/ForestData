compute_mortality <- function(data,
                              alive_col,
                              time_col,
                              corrected = F){

# Checks and errors -------------------------------------------------------

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!is.character(alive_col)){
    if("tidyverse" %in% installed.packages()){
      library(tidyverse); alive_col = dplyr::enquo(alive_col)
    }
    else{
      stop("alive_col must be a character object.")
    }
  }
  else{
    data[which(names(data) == alive_col)] <- "status"
  }

  if(!is.character(alive_col)){
    if("tidyverse" %in% installed.packages()){
      library(tidyverse); alive_col = dplyr::enquo(alive_col)
    }
    else{
      stop("alive_col must be a character object.")
    }
  }
  else{
    data[which(names(data) == alive_col)] <- "status"
  }

  if(!is.character(time_col)){
    if("tidyverse" %in% installed.packages()){
      library(tidyverse); time_col = dplyr::enquo(time_col)
    }
    else{
      stop("time_col must be a character object.")
    }
  }
  else{
    data[which(names(data) == time_col)] <- "time"
  }

  if(!corrected){
    data <- correct_alive(data,alive_col=alive_col,time_col=time_col)
    warning("You specified that your dataset was not corrected beforehand. It has been automatically corrected prior to mortality rate computation.")

  }

# Create columns for mortality computation --------------------------------

  data$status_lagged <- c(NA, data$status[-length(data$status)])
  data$time_lagged <- c(NA, data$status[-length(data$status)])

  times <- sort(unique(data$time), decreasing = F)

  mortality <- data.frame(time = paste(times[-length(times)],times[-1], sep = "_" ),
                          mortality = NA -)

  for(i in 1:(length(times)-1)){
    t0 <- times[i]
    t1 <- times[i+1]

    N0 <- sum(data$time == t0 & data$status == 1)
    N1 <- sum(data$time == t1 & data$status == 1 & data$status_lagged == 1)

    mortemp <- 1-(N1/N0) ^ (1/(t1-t0))
  }






}
