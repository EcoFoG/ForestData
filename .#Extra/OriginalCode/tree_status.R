tree_status <- function(X){
  status<-rep(NA, length(X))
  ### first to last alive measure (even missing dbh values)
  status[(min(which(!is.na(X)))):(max(which(!is.na(X))))]<-1
  ### dead trees : last value = NA
  if (max(which(!is.na(X)))<length(X)){
    status[(max(which(!is.na(X)))+1):length(X)]<-0
  }
  return(status)
}