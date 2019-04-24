
### 1) function that replaces missing values ###

repl_missing <- function(Y, time, status) {
  miss <- which(is.na(Y) && status == 1)
  # miss: nb of the missing value(s)
  Y[miss] <- sapply(miss, function(i){
    # case 1: i is the first value of the series
    if (i < min(which(!is.na(Y))) ) {
      ## choose 2 next values
      yval <- Y[which(!is.na(Y))[1:min(2, sum(!is.na(Y)))]]
      tval <- time[which(!is.na(Y))[1:min(2, sum(!is.na(Y)))]]
    } #case 2: i is the last value of the series
    else if (i > max(which(!is.na(Y)))) {
      yval <- Y[which(!is.na(Y))[(sum(!is.na(Y))-1):sum(!is.na(Y))]]
      tval <- time[which(!is.na(Y))[(sum(!is.na(Y))-1):sum(!is.na(Y))]]
      yval <- yval[!is.na(yval)]
      tval <- tval[!is.na(yval)]
    } # case 3: i is between 2 non na values
    else {
      yval <- Y[c(max(which(!is.na(Y[1:(i-1)]))), i+min(which(!is.na(Y[(i+1):length(Y)]))))]
      tval <- time[c(max(which(!is.na(Y[1:(i-1)]))), i+min(which(!is.na(Y[(i+1):length(Y)]))))]
    }
    reg <- lm(yval ~ tval)$coef
    yi <- reg[1] + reg[2]*time[i]
    if (sum(!is.na(tval))==1) {yi <- yval}
    return(yi)
  })
  return(unlist(Y))
}

mega_correction_ <- function(data, X, tm, status, limit=20){
  X <- data[[X]]
  tm <- data[[tm]]
  status <- data[[status]]
  return(mega_correction(X, tm, status, limit))
}


### 2) global corrections ###
mega_correction <- function(X, tm, status, limit=20) {

  # Xsav if for browser() use: save initial value of X
  Xsav <- X
  #   X = data$dbh[data$idtree==id]
  #   tm = data$year[data$idtree==id]
  #   status = data$status[data$idtree==id]
  # cresc : relative annual diameter increment
  # cresc_abs: absolute annual diameter increment
  cresc <- rep(0, length(X)-1)
  cresc_abs <- rep(0, length(X)-1)
  if (sum(!is.na(X))>1) {
    cresc[which(!is.na(X))[-1]-1] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
    cresc[is.nan(cresc)] <- 0
    cresc_abs[which(!is.na(X))[-1]-1] <- diff(X[!is.na(X)])
  }
  recr_temp <- which(is.na(status))
  recr_temp2 <- last(recr_temp)
  recr <- recr_temp2+1 # recruitment census

  if (length(cresc)>0){

    ### increase > 5cm/year ###
    if (max(cresc)>5) {

      # census with excessive increase in dbh
      ab <- which.max(cresc)
      # surrounding values
      existing <- c(ab-2,ab-1,ab+1,ab+2)
      # Test if the surrounding values are in the boundaries
      existing <- existing[existing > 0 & existing <= length(cresc)]

      # Mean of the cresc of the surrounding values
      meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)

      up <- which.max(cresc[existing])
      down <- which.min(cresc[existing])

      # 1st case : excessive increase/decrease offset by a similar decrease in dbh, plus 5cm/yr
      if ( length(existing)>0 ) {
        if ((down>up &&(max(cresc_abs[existing]) + min(cresc_abs[existing]) < 5*(tm[existing[down]+1] - tm[existing[up]]))) |
            (up>down && (max(cresc_abs[existing]) + min(cresc_abs[existing]) < 5*(tm[existing[up]+1] - tm[existing[down]]))))
        {
          # correction: abnormal values are deleted and will be replaced later on (see missing)
          first <- min(which.max(cresc), which.min(cresc))+1
          last <- max(which.max(cresc), which.min(cresc))
          X[first:last] <- NA
        } else { # 2nd case: increase of more than 5 cm/yr with no return to initial values
          # we trust the "new measurements and change the 1st set of"block" with more values
          if ( sum(!is.na(X[1:ab])) < sum(!is.na(X))/2 ) {
            X[1:ab] <- X[1:ab] + cresc_abs[which.max(cresc)] - meancresc*diff(tm)[ab]
          } else {
            X[(ab+1):length(X)] <- X[(ab+1):length(X)] - cresc_abs[which.max(cresc)] + meancresc*diff(tm)[ab]
          }
        }
      }

      ## update cresc
      cresc <- rep(0, length(X)-1)
      cresc_abs <- rep(0, length(X)-1)
      if (sum(!is.na(X))>1) {
        cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
        cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
      }
    }

    ### decrease > 2cm ###
    if (min(cresc_abs)<(-2)) {
      # census with excessive decrease in dbh
      ab <- which.min(cresc_abs)
      # surrounding values
      existing <- c(ab-2,ab-1,ab+1,ab+2)
      existing <- existing[existing > 0 & existing < length(cresc)]
      meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)

      # 1st case: one low value and then return to a "normal" dbh
      if ( ab < length(cresc) & ( sum( (cresc_abs)[ab], (cresc_abs)[ab+1]) >= 0 ) ) {
        X[ab+1] <- NA }

      # 2nd case: one high value and then return to a "normal" dbh
      else if ( ab > 1 & sum((cresc_abs)[ab], (cresc_abs)[ab-1]) >= 0 ) { X[ab-1] <- NA }

      # 3rd case: no return to initial values: retrieve difference from 1st set of values (trust last measurements)
      # probably a new measurement height (because of buttresses, ...): we change the 2nd set of values
      else { X[-(1:ab)] <- X[-(1:ab)] - min(cresc_abs) + meancresc*diff(tm)[ab]
      }
    }

    ## update cresc
    cresc <- rep(0, length(X)-1)
    cresc_abs <- rep(0, length(X)-1)
    if (sum(!is.na(X))>1) {
      cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
      cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
    }


    ## update cresc
    cresc <- rep(0, length(X)-1)
    cresc_abs <- rep(0, length(X)-1)
    if (sum(!is.na(X))>1) {
      cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
      cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
    }

    ## replace missing values
    X <- repl_missing(X, tm, status)

    ### overgrown recruit
    if ( !is.na(recr) && length(recr)>0 && sum(!is.na(X))>0 ) {    ## recruited during measurement interval
      if( X[recr] > limit + diff(tm)[recr-1]*5) {  ### tolerated growth rate: 5cm/yr
        coef <- lm(X[!is.na(X)]~tm[!is.na(X)])$coefficients
        if (is.na(coef[2])) { X[is.na(status)] <- coef[1] } ### only 1 dbh value: replace all non-recruited dbh by this value
        else { X[is.na(status)] <- min(coef[1] + tm[is.na(status)]*coef[2], X[!is.na(X)][1]) }
      }
    }
    return(X)
  }
}

## 4) status: alive (1), not recruited (NA), dead (0)
tree.status <- function(X){
  status<-rep(NA, length(X))
  ### first to last alive measure (even missing dbh values)
  status[(min(which(!is.na(X)))):(max(which(!is.na(X))))]<-1
  ### dead trees : last value = NA
  if (max(which(!is.na(X)))<length(X)){
    status[(max(which(!is.na(X)))+1):length(X)]<-0
  }
  return(status)
}
