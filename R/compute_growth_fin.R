#' Title
#'
#' @inheritParams correct_all
#' @param what_output character, one of 'annual' or 'absolute'. Defines if the output growth rates must be annualised growth rates or absolute between-census deltas (respectively).
#' @param aggregate logical, defining if the growthrates must be aggregated (TRUE) according to specified variables (argument 'by') with calculated values 'stat' and quantiles ('percentiles'); or returned for each individual (FALSE).
#' @param by character vector, names of the grouping variables used to aggregate values.
#' @param stat character, one of the following: 'mean','sum','median'. Statistics used if values are aggregated.
#' @param percentiles integer vector between 0 and 100. Percentiles are calculated if values are aggegated.
#' @param ... Optional arguments for advanced use. ask_stat: logical, defaults to TRUE, if set to FALSE, try directly to use the specified 'stat' for aggregation without checking it -thus, not failproof-.verbose: logical, defaults to TRUE, if set to FALSE or equivalent, show less messages.
#'
#' @return Either the input data.frame with individual-level calculated growth rates, or a data.frame with grouping variables and required statistics.
#' @export
#'
#' @examples
#' data(example_census)
#' growth <- compute_growth(example_census)
#' head(growth)
compute_growth <- function(data,
                           size_col = "Circ",
                           measure_type = "cir",
                           status_col = "CodeAlive",
                           id_col= "idTree",
                           time_col = "CensusYear",
                           what_output = "annual",
                           aggregate = FALSE,
                           by = c("Plot", "SubPlot"),
                           stat = "mean",
                           percentiles = c(5,95),
                           ...){

# Checks ------------------------------------------------------------------
  args <- list(...)
  verbose <- TRUE
  ask_stat <-  TRUE
  if(length(args) > 0){
    for(a in names(args)){
      switch(a,
             "verbose" = {
               verbose <- args[a]
             },
             "ask_stat" = {
               ask_stat = args[a]
             },
             warning(paste0("argument ",a," is unused, see the description.")))
    }
  }


  data <- check_rename_variable_col(size_col,"size_col",data)
  data <- check_rename_variable_col(id_col,"id_col",data)
  data <- check_rename_variable_col(time_col,"time_col",data)
  data <- check_rename_variable_col(status_col,"status_col",data)
  # print(names(data))
  # if(!isTRUE(pmatch(x=measure_type,table="Circumference"))){
  #   data$size <- data$size/pi
  # }
  # else if(!isTRUE(pmatch(x=measure_type,table="Diameter"))){
  #   stop("Argument 'measure_type' must partially match either Circumference of Diameter to indicate the measurement type")
  # }
  if(any(is.na(data$size)|is.na(data$time))){
    warning("There are NA values in size measurements and/or census time fields. These lines were deleted because a growth rate cannot be computed, thus not included in the output table.")
  }
    data <- data[which(!is.na(data$size)|!is.na(data$time)),]

  if(any(is.nan(data$time)|!is.finite(data$time))){
    warning("Census time and/or measurement fields contain non-finite or non-numeric values. Please check it. It has been automatically replaced by NA before calculation.")
    data <-data[which(!is.nan(data$time)|!is.finite(data$time)),]
  }

  if(isTRUE(aggregate)){
    if(!is.character(by)){
      stop("Argument 'by' must be a character vector.")
    }
    else if(any(!by %in% names(data))){
      print(by[!by %in% names(data)])
      stop("argument 'by' must contain the names of the column used to aggregate the output.")
    }

    if(!exists(eval(stat), mode = "function")){
      # subset(as.data.frame(installed.packages()), Priority %in% c("base","recommended"), select=c(Package, Priority))
      stop(paste0("The function you choosed with argument 'stat' apparently does not exist, or at least not on your terminal."))
    }

    if(is.null(percentiles)| !is.numeric(percentiles)){
      print(c(percentiles, class(percentiles)))
      stop("Argument 'percentiles' must be either NULL (default, no percentiles calculated) or integer (in this case, positive between 0 and 100)")
    }
    else if(any(percentiles < 1 | percentiles > 100)){
      stop("percentiles must be positive integers, between 0 and 100.")
    }
  }
  else if(!isFALSE(aggregate)){
    stop("The 'aggregate' argument must be logical (1/TRUE, 0/FALSE) and should indicate whether the output should be aggregated or not. If yes, aggregation is done by columns specified in 'by', with the statistics 'stat' and optional percentiles ('percentiles').")
  }



  data <- data[order(data$id, data$time),]
  # print(-nrow(data))
  # print(data$time[])
  data$time_lag <- c(NA, data$time[-nrow(data)])
  data$id_lag <- c(NA, data$id[-nrow(data)])
  data$size_lag <- c(NA, data$size[-nrow(data)])

  data$absolute_growth <- data$size-data$size_lag
  data$time_interval= paste(data$time,data$time_lag)

  data$absolute_growth[which(!(data$id == data$id_lag | is.na(data$id)))] <- NA
  data$time_interval[which(!(data$id == data$id_lag | is.na(data$id)))] <- NA
  if(any(is.na(data$status) | (data$status == 0))){
    # print(verbose)
    if(isTRUE(verbose)){
      message("The growth rates were not computed when tree life status was 0 (dead) or NA (unseen or uncertain), thus the growth rate on these lines was set to NA")
    }
    data[which(is.na(data$status) | data$status == 0),"absolute_growth"] <- NA
  }
  if(what_output == "annual"){
    data$annual_growth <- data$absolute_growth/(data$time-data$time_lag)
  }
  #minor quickfix
  data <- data[!is.na(data$absolute_growth),]

  if(isTRUE(aggregate)){
    if(!stat %in% c("mean","median","sum")){
      message("This function supports the following statistics for 'stat' argument: 'mean','median','sum'. ")
      message(paste0("The function name you provided is ",stat))
      agreement <- readline(prompt="Do you want to try forcing the use of your stat at your own risks?  y(yes)/n(no): ")
      if(agreement == "y"|agreement == "Y")
        agreement <- TRUE
      else if(agreement == "n"| agreement == "N")
        agreement <- FALSE
      else print("You did not type neither 'y' (yes) nor 'n' (no), I'll take it as a NO.")
      if(agreement){
        data <- aggregate_growth(growth = data,
                                 by=by,
                                 time_col = "time",
                                 growth_col = ifelse(what_output == "annual",
                                                     "annual_growth",
                                                     "absolute_growth"),
                                 percentiles = percentiles,
                                 stat = stat)
      }
      else{
        print("then, the non agregated data is returned.")
      }
    }
    else{
      data <- aggregate_growth(growth = data,
                               by=by,
                               time_col = "time",
                               growth_col = ifelse(what_output == "annual",
                                                   "annual_growth",
                                                   "absolute_growth"),
                               percentiles = percentiles,
                               stat = stat)
    }
  }
  return(data)
}
