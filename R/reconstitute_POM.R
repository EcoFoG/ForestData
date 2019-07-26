reconstitute_pom_paracou <- function(data,
                             default_pom = 1.3,
                             id_col = "idTree",
                             measure_col = "Circ",
                             # measure_type = "C",
                             measure_code_col = "CodeMeas",
                             # protocol = protocol(),
                             time_col = "CensusYear"){

# Checks ------------------------------------------------------------------


# Reformat data -----------------------------------------------------------

  names(data) [which(names(data) == measure_col)] <- "measure"
  names(data) [which(names(data) == measure_code_col)] <- "code"
  names(data) [which(names(data) == id_col)] <- "id"
  names(data) [which(names(data) == time_col)] <- "time"
  data <- data[order(data$id, data$time),]
  data$POM <- default_pom
  ids <- unique(data$id)

# Reconstitute pom loop ---------------------------------------------------

  for(i in ids){
    temp <- data[which(data$id == i),c("POM","code")]
    for(j in 1:nrow(temp)){
      if(j == 1){
        base <- default_pom
      }
      else{
        base <- temp$POM[j-1]
      }

      if(!is.na(temp$code[j]) & temp$code[j] != 0){
        if(temp$code[j] %in% c(1,2,3)){
          pomtemp <- base+(temp$code[j]*0.5)
        }
        else if(temp$code[j] == 10){
          pomtemp <- NA
        }
        else{
          pomtemp <- base
        }
      }
      else{
        pomtemp <- base
      }
print(temp$code[j])
  print(pomtemp)
      temp$POM[j] <- pomtemp


    #     ifelse(temp$code[j] != 0,
    #                         base,
    #                         ifelse(temp$code[j] %in% c(1,2,3,10),
    #                                ifelse(temp$code[j] %in% c(1,2,3),
    #                                       base+(as.numeric(temp$code[j])*0.5),
    #                                       NA),
    #                                NA
    #                         )
    #   )
    #
    }
    data[which(data$id == i),"POM"] <- temp$POM
    rm(temp)
  }

  names(data)[which(names(data) == "measure")] <- measure_col
  names(data)[which(names(data) == "code")] <- measure_code_col
  names(data)[which(names(data) == "time")] <- time_col
  names(data)[which(names(data) == "id")] <- id_col
  # names(data)
  # names(data) [which(names(data) == measure_col)] <- "measure"
  # names(data) [which(names(data) == measure_code_col)] <- "code"
  # names(data) [which(names(data) == id_col)] <- "id"
  # names(data) [which(names(data) == time_col)] <- "time"
  return(data)
}
