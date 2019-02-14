library(tidyverse)
paracou <- EcoFoG::Paracou2df()

paracoutest <- paracou %>% filter(Plot %in% c(1:16))

profvis::profvis({
  data = paracoutest
           measure_col = "CircCorr"
           measure_type = "C"
           time_col = "CensusYear"
           by = c("Plot")
           surface = data.frame(Plot = c(1,2,16),surface=c(6.25,6.25,25))
    if(!measure_col %in% names(data)){
      stop("Argument measure_col does not match any column name in the forest inventory you provided")
    }
    else{
      names(data)[which(names(data)==measure_col)]<- "size"
      if(!is.numeric(data$size)){
        stop("Tree size measurements must be numeric, which is apparently not the case in your forest inventory")
      }
      else{
        if(anyNA(data$size)){
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


      bys <- list()
      for(b in by){
        # print(data[,which(names(data) == by[b])])
        bys[[b]] <- unique(data[,which(names(data) == b)])
      }
      basal_area <- expand.grid(bys, stringsAsFactors = FALSE)


      basal_area <- data.frame(basal_area,"absolute_basal_area" = NA, "surface_area" = NA, "basal_area_per_ha" = NA)

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

            expsurf <- paste0(paste0("basal_area$",
                                     matchs,
                                     " == ",
                                     corresp),
                              collapse = " & ")
            basal_area$surface_area[which(eval(parse(text = expsurf)))] <- surftemp
          }
          # }
        }
      }
      else{
        basal_area$surface_area = 1
      }

      for(f in 1:nrow(basal_area)){

        rowval = basal_area[f,by]
        # print("rowval")
        # print(str(rowval))
        exp <- paste0(paste0("data$",
                             by,
                             " == ",
                             rowval),
                      collapse = " & ")
        # print(exp)
        # print(length(which(eval(parse(text=exp)))))
        basal_area[f,"absolute_basal_area"] <- sum(data[eval(parse(text=exp)),"ba"], na.rm = T)
        basal_area[f,"basal_area_per_ha"] <- basal_area[f,"absolute_basal_area"]/basal_area[f,"surface_area"]
      }
    }
}
)
