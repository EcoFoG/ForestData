#' Compute Basal Area in Forest Inventories
#'
#' compute_ba computes Basal Area according to user-specified grouping variables -e.g. Plot, species, etc- and time.
#'
#' @param data A data.frame containing a time-series tree-wise forest inventory -i.e. every line is a single tree measurement for a single year.
#' @param size_col character containing the name of the tree size measurements column -either circumference or diameter.
#' @param measure_type A single character indicating whether tree sizes are given in circumferences -"C"- or diameter -"D"-.
#' @param by A character vector containing the name of the columns containing the variables -other than census time- according to which the result will be aggregated. Be it plots, subplots or species name...
#' @param surface Either a scalar containing the surface area of each plot -if they have the same dimensions- or a data.frame of the surface area according to some of the grouping variables -e.g. Plot and subplot. However, defaults to FALSE and in this case, only absolute BA is returned.
#' @param time_col A single character containing the name of the column corresponding to census years
#' @param status_col Single character, the name of the column containing (corrected) tree life status
#'
#' @return A dataframe containing, for each combination of grouping variables, the plot-level and per-ha basal area for a given census year.
#' @export
#'
#' @examples
#' data("example_recruits")
#'
#' ba_plots <- suppressWarnings(compute_ba(example_recruits,
#' size_col = "size_corr",
#' measure_type = "C",
#' time_col = "CensusYear",
#' by = "Plot",
#' surface = 1.5625))
#'
#' ba_taxa <- suppressWarnings(compute_ba(example_recruits,
#' size_col = "size_corr",
#' measure_type = "C",
#' time_col = "CensusYear",
#' by = c("binomial_name","Plot"),
#' surface = 1.5625))
#'
#' str(ba_plots)
#' head(ba_taxa)
#'
compute_ba <- function(data,
                       by = c("Plot","binomial_name"),
                       size_col = "size_corr",
                       measure_type = getOption("measure_type"),
                       time_col = getOption("time_col"),
                       status_col = "status_corr",
                       surface = 1.5625){


  if(!status_col %in% names(data)){
    stop("Argument status_col does not match any column name in the forest inventory you provided")
  }
  else{
    names(data)[names(data)==status_col] <- "status_corr"
  }

  if(!size_col %in% names(data)){
    stop("Argument size_col does not match any column name in the forest inventory you provided")
  }
  else{
    names(data)[which(names(data)==size_col)]<- "size"
    if(!is.numeric(data$size)){
      stop("Tree size measurements must be numeric, which is apparently not the case in your forest inventory")
    }
    else{
      if(any(is.na(data$size) & !is.na(data$status_corr) & data$status_corr == 1)){
        warning("Tree size measurements contain NA values. Have you used the correction and completion functions that we provide beforehand ?")
      }
      if(measure_type == "C"){
        data$size = data$size/pi
      }
    }
  }


  data$ba <- pi*(data$size*data$size)/4

  if(!(is.na(time_col)|is.null(time_col))){
    by <- c(by, time_col)
  }
  if(any(!by %in% names(data))){
    if(sum(!by %in% names(data)) == 1){
      stop(paste0("Argument 'by' contains an element that is not matching the dataset's fields: ",
                  "by[",which(!by %in% names(data)),"] = ",by[which(!by %in% names(data))],
                  ". For more information, please see the documentation page of the function"))
    }
    else{
      for(n in which(!by %in% names(data))){
        print(paste0("Argument 'by' contains an element that is not matching the dataset's fields: ",
                     "by[",n,"] = ",by[n],
                     "."))
        stop("For more information, please see the documentation page of the function")
      }
    }
  }
  else{
    # print("hey, let's get started")
    data <- data[which(data$status_corr ==1),]
    data$bys <- do.call(paste, c(data[,by], sep="_"))

      # do.call(c(), lapply(1:nrow(data), function(i) paste0(data[i,by], collapse = "_")))
    # unlist(lapply(1:nrow(data),
                              # function(l){paste0(data[l,by], collapse = "_")}))
    # print(by)
    # bys <- list()
    # for(b in by){
    #   # print(data[,which(names(data) == by[b])])
    #   bys[[b]] <- unique(data[,which(names(data) == b)])
    # }
    # bys[[time_col]] <- unique(data[,which(names(data) == time_col)])
    # print(bys)
    # basal_area <- expand.grid(bys, stringsAsFactors = FALSE)

    basal_area <- unique(data[,which(names(data) %in% by)])

    basal_area$bys <- do.call(paste, c(basal_area[,by], sep="_"))

    # for(i in 1:nrow(basal_area)){
    #   basal_area$bys<- paste0(basal_area[i,by], collapse = "_")
    # }

    # for(i in 1:nrow(basal_area))
    # basal_area$bys <- unlist(lapply(1:nrow(basal_area),
                                    # function(l){paste0(basal_area[l,by], collapse = "_")}))
# print("her")
    basal_area <- data.frame(basal_area,"absolute_basal_area" = NA, "surface_area" = NA, "basal_area_per_ha" = NA, stringsAsFactors = FALSE)

    if(!isFALSE(surface)){
      if(is.numeric(surface) & length(surface == 1)){
        basal_area$surface_area = rep(surface, nrow(basal_area))
      }
      else if(is.data.frame(surface)){
        # if(!length(which(!names(surface) %in% by))> 1){
          for(s in 1:nrow(surface)){

            surftemp <- surface[s,"surface"]

            matchs <- names(surface)[which(names(surface) %in% by)]
            corresp <- surface[s,matchs]
# print(matchs)
            expsurf <- paste0(paste0("basal_area$",
                                     matchs,
                                     " == ",
                                     corresp),
                              collapse = " & ")
            # print("here")
            # print(expsurf)
            # print(which(eval(parse(text = expsurf))))
            basal_area$surface_area[which(eval(parse(text = expsurf)))] <- surftemp
          }
        # }
      }
    }
    else{
      basal_area$surface_area = 1
    }

    for(f in 1:nrow(basal_area)){

      rowval = basal_area[f,"bys"]


      # exp <- paste0(paste0("data$",
      #                      by,
      #                      " == ",
      #                      ifelse(unlist(lapply(1:length(rowval),
      #                                           function(v){
      #                                             is.numeric(rowval[v])
      #                                           })),
      #                             rowval,
      #                             paste0("'",rowval,"'"))),
      #                      collapse = " & ")
      # print(rowval)
      # print(ifelse(is.numeric(rowval),
      #              rowval,
      #              paste0("'",rowval,"'")))
      # print(exp)
      # print(length(which(eval(parse(text=exp)))))
      basal_area[f,"absolute_basal_area"] <- sum(data[which(data$bys == rowval),"ba"], na.rm = F)
      # basal_area[f,"absolute_basal_area"] <- sum(data[which(as.character(data[,by]) == rowval),"ba"], na.rm = F)
      # basal_area[f,"absolute_basal_area"] <- sum(data[which(eval(parse(text=exp))),"ba"], na.rm = T)
      basal_area[f,"basal_area_per_ha"] <- basal_area[f,"absolute_basal_area"]/basal_area[f,"surface_area"]
    }
  }
  return(basal_area)
}

