correct_size <- function(data,
                         size_col,
                         time_col,
                         status_col,
                         id_col,
                         positive_growth_threshold,
                         negative_growth_threshold,
                         default_POM){

# Checks and format -------------------------------------------------------
names(data)[which(names(data)== size_col)] <- "size"
names(data)[which(names(data)== status_col)] <- "status"
names(data)[which(names(data)== time_col)] <- "time"
names(data)[which(names(data)== id_col)] <- "id"

# Call internals by plot or not -------------------------------------------

data <- .correct_size_plot(data,
                           positive_growth_threshold,
                           negative_growth_threshold,
                           default_POM)

}


# Internals ---------------------------------------------------------------

.correct_size_plot <- function(data_plot,
                               positive_growth_threshold,
                               negative_growth_threshold,
                               default_POM){
  # Compute plot level growth
  data_plot$code_corr <- rep(0,nrow(data_plot))
  # print(data_plot$code_corr)


  data_plot$size_corr <- data_plot$size
  # print(data_plot$Circ)
  data_plot$cresc <- c(NA,diff(data_plot$size))
  mismatch <- diff(data_plot$id)
  print(data_plot$cresc)
  data_plot$cresc[which(mismatch != 0)+1] <- NA

  # Extract ids and loop on indivs

  ids <- unique(data_plot$id)

  for(i in ids){

    tree <- data_plot[which(data_plot$id == i),]
# print(which(data_plot$id == i))
    print("cres")
    print(tree$cresc)
    print("cresdone")
    cresc_tree <- tree$cresc

    POM <- tree$POM

    # tree[,c("size_corr","code_corr")] <- .correct_size_tree(size,
    #                                   cresc,
    #                                   status,
    #                                   time,
    #                                   POM)
    size <- tree$size
    status <- tree$status
    time <- tree$time
    size_corr <- tree$size_corr
    code_corr <- tree$code_corr

    # print(str(tree))
    # print(size_corr)
    res <- .correct_size_tree(size,
                              size_corr,
                              code_corr,
                              cresc_tree,
                              status,
                              time,
                              POM,
                              default_POM = 1.3,
                              i)

    # print(data_plot[which(data_plot$id == i),which(names(data_plot)%in% c("size_corr","code_corr"))])
    print('data_plot[which(data_plot$id == i),c("size_corr","code_corr")]')
    print(nrow(data_plot[which(data_plot$id == i),c("size_corr","code_corr")]))

    data_plot[which(data_plot$id == i),c("size_corr","code_corr")] <- res[,c("size_corr","code_corr")]
  }
  return(data_plot)
}







.correct_POM_changes <- function(size,
                               size_corr,
                               code_corr,
                               cresc,
                               time,
                               POM,
                               default_POM,
                               i){
ignore_negative_POM_changes = F
  code_corr <- rep(0, length(size))
  # Account for explicit POM changes

  if(POM[1] != default_POM){
    ####  WHAT ??? ###
    # How do we convert to dbh #
  }

  if(anyNA(POM)){
    POM <- .fill_na_POM(POM)
  }

  if(! all(POM == POM[1])){
    POM_diff <- diff(POM)
    if(!ignore_negative_POM_changes &
       any(POM_diff < 0)){
      msg <- paste0("It seems that you have negative POM changes ",
                    "for individual ",i,
                    " which is supposedly erroneous or abnormal.",
                    " If this is normal, run again with argument ignore_negative_pom_changes set to TRUE")
      stop(msg)
    }

    shifts <- which(!is.na(POM_diff) & POM_diff != 0)
    print("shifts")
    print(shifts)
    for(s in shifts){
      print("shifts numb")
      print(s)
      existing <- c(s-2,s-1,s+1,s+2)
      # print("prob1")
      # print(existing)
      existing <- existing[existing > 0 &
                             existing <= length(cresc) &
                             cresc[existing] > -2*pi &
                             cresc[existing] < 5*pi] #Because we don't want to use outlyers to compute expected growth...
      # print("probL")
      # print(existing)
      # print(existing > 0 &
              # existing <= length(cresc) )
      meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)
      # print("meancresc")
      # print(meancresc)
      # print("prob")
      # print(existing)
      # print("prob")
      # print("s+1:length(size_corr)"); print((s+1):length(size_corr))
      # print("s+1:length(size)"); print((s+1):length(size))
      # print("s+1:length(POM)"); print((s+1):length(POM))
      # print("size_corr[s+1] - size_corr[s]"); print(size_corr[s+1] - size_corr[s])
      # print("meancresc*(time[s+1]-time[s])"); print((meancresc*(time[s+1]-time[s])))
      # print(size_corr[s+1] - size_corr[s]) + (meancresc*(time[s+1]-time[s]))
      correction <- - (size_corr[s+1] - size_corr[s]) + (meancresc*(time[s+1]-time[s]))
      # print("correction");print(correction)
      size_corr[(s+1):length(size_corr)] <- (size_corr[(s+1):length(size_corr)]) + correction
      code_corr[(s+1):length(code_corr)] <- code_corr[(s+1):length(code_corr)]+1

      # print("s+1:length(size_corr)2"); print((s+1):length(size_corr))
      # print("s+1:length(size)2"); print((s+1):length(size))
      # print("s+1:length(POM)2"); print((s+1):length(POM))
      }
  }
  # Check for outlyers

  # Compute growth rates again ?

  # Spot outlyers type
  # print(size_corr)
  # print(code_corr)
  print(i)
  res <- data.frame("size_corr" = size_corr, "code_corr"=code_corr)
  # print(nrow(res))
  print(cbind(res,size, POM))
  return(res)
}
