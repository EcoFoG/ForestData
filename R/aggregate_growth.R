aggregate_growth <- function(growth,
                             by=c("Plot","SubPlot"),
                             time_col = "time",
                             growth_col = "annual_growth",
                             percentiles = NULL,
                             stat = "sum"){

# Checks ------------------------------------------------------------------
  if(!is.data.frame(growth)){
    stop("growth must be a data.frame")
  }
  if(!is.character(by)){
    stop("by must be a character")
  }
  else{
    if(any(!by%in% names(growth))){
      stop("by must contain names that are in the growth table.")
    }
  }
  if(!is.numeric(percentiles)){
    stop("percentiles must be numeric")
  }
  else{
    if(any(percentiles>100|percentiles<1)){
      stop("percentiles must be a vector of numerics, each one ranging between 0 and 1.")
    }
  }

  # time_col = NULL #tag debug
  if(!(is.null(time_col))){
    if(!is.na(time_col)){
      by <- c(by, time_col)
    }
  }

  growth$bys <- do.call(paste, c(growth[,by], sep="_"))
# Result table format -----------------------------------------------------

  # do.call("mean", c(rnorm(10000000), list(NA),na.rm=T))

  growth_by <- unique(growth[,which(names(growth) %in% by)])
  growth_by$bys <- do.call(paste, c(growth_by[,by], list(sep="_")))
  growth_by <- data.frame(growth_by, "stat" = NA, stringsAsFactors = FALSE)
  if(!is.null(percentiles)){
    for(p in percentiles){
      growth_by[,paste0("P",p)] <- NA
    }
  }
  names(growth_by)[names(growth_by) == "stat"] <- eval(stat)

# Fill --------------------------------------------------------------------
  for(f in 1:nrow(growth_by)){
    rowval = growth_by[f,"bys"]
    # if(f %in% c(1,10,100)){
    #   print("stat");
    #   print(eval(stat))
    #
    # }
# print(do.call(eval(stat),c(growth[which(growth$bys == rowval),growth_col], list(na.rm = TRUE))))
    # print(growth_col)
    # print(growth[which(growth$bys == rowval),growth_col])
    growth_by[f,eval(stat)] <- do.call(eval(stat),args = list(x=growth[which(growth$bys == rowval),growth_col], na.rm = TRUE))
    if(!is.null(percentiles)){
      for(p in percentiles){
        growth_by[f,paste0("P",p)] <- quantile(growth[which(growth$bys == rowval),growth_col],p/100, na.rm = TRUE)
      }
    }
  }
  return(growth_by)
}
