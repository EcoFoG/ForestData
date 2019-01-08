repl_missing <- function(Y, time, status = NULL) {
  
  if (length(status) == 0) {
    status = rep(1, length(Y))
  }
  
  miss <- which(is.na(Y) & status == 1)
  
  # miss: nb of the missing value(s)
  
  Y[miss] <- sapply(miss, function(i) {
    
    # case 1: i is the first value of the series
    if (i < min(which(!is.na(Y)))) {
      ## choose 2 next values
      yval <- Y[which(!is.na(Y))[1:min(2, sum(!is.na(Y)))]]
      tval <- time[which(!is.na(Y))[1:min(2, sum(!is.na(Y)))]]
    }
    
    #case 2: i is the last value of the series
    else if (i > max(which(!is.na(Y)))) {
      yval <- Y[which(!is.na(Y))[(sum(!is.na(Y)) - 1):sum(!is.na(Y))]]
      tval <-
        time[which(!is.na(Y))[(sum(!is.na(Y)) - 1):sum(!is.na(Y))]]
      yval <- yval[!is.na(yval)]
      tval <- tval[!is.na(yval)]
    }
    
    # case 3: i is between 2 non na values
    else {
      yval <-
        Y[c(max(which(!is.na(Y[1:(i - 1)]))), i + min(which(!is.na(Y[(i + 1):length(Y)]))))]
      tval <-
        time[c(max(which(!is.na(Y[1:(i - 1)]))), i + min(which(!is.na(Y[(i + 1):length(Y)]))))]
    }
    reg <- lm(yval ~ tval)$coef
    yi <- reg[1] + reg[2] * time[i]
    if (sum(!is.na(tval)) == 1) {
      yi <- yval
    }
    return(yi)
  })
  return(unlist(Y))
}