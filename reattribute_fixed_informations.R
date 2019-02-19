reattribute_fixed_informations <- function(data,
                                           fixed_attributes = c("Genus","Species","Plot"),
                                           id_col = "idTree"){
  names(data)[which(names(data) == id_col)] <- "id"
  ids <- unique(data[,"id"])
  data
  for(i in ids){
    for(j in fixed_attributes){
      if(any(is.na(data[which(data$id == i), j]))){
        print(i)
        uni <- unique(data[which(data$id == i), j])
        uni <- uni[which(!is.na(uni))]
        if(length(uni) > 1){
          message = paste0("attribute ",
                           j,
                           " that you defined as a non-varying column -i.e. supposed to have always the same value for each measurement of the same tree- has multiple values for tree ",
                           i,
                           " and takes the values ",
                           uni)
          stop(message)
        }
        else{
          print(paste0("uni ",uni))
          # print(paste0(2, data[which(data$id == i), j]))
          # print(paste0(3,which(data$id == i)))
          # data[which(is.na(data[which(data$id == i), j])), j] <- uni
          # print(j)
          print(c(which(names(data) == j)))
          data[which(data$id == i & is.na(data[,which(names(data) == j)])),which(names(data) == j)] <- uni
        }
      }
    }
  }
  names(data)[which(names(data) == "id")] <- id_col
  return(data)
}
