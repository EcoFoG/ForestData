### 1) global corrections ###
mega_correction <- function(X, tm, status, limit=20) {
  # Xsav if for browser() use: save initial value of X
  Xsav <- X
  code_corr = rep(0,length(X))
  # code 0 = no correction
  # code 1 = increase >5cm/yr + return
  # code 2 = increase >5cm/yr + 2 series
  # code 3 = decrease > 2cm + return
  # code 4 = decrease >2cm + 2 series

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

  if (length(cresc)>0){

    ### increase > 5cm/year ###
    if (max((cresc))>5) {

      # census with excessive increase in dbh
      ab <- which.max(cresc)
      # surrounding values
      existing <- c(ab-2,ab-1,ab+1,ab+2)
      existing <- existing[existing > 0 & existing <= length(cresc)]
      meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)

      up <- which.max(cresc[existing])
      down <- which.min(cresc[existing])

      # 1st case : excessive increase/decrease offset by a similar decrease in dbh, plus 5cm/yr
      if ( length(existing)>0 ) {
        if ((down>up & max(cresc_abs[existing]) + min(cresc_abs[existing])  < 5*(tm[existing[down]+1] - tm[existing[up]]) ) |
            (up>down &  max(cresc_abs[existing]) + min(cresc_abs[existing]) < 5*(tm[existing[up]+1] - tm[existing[down]])) )
          { #TAG ERROR: the conditions here are not well
          # correction: abnormal values are deleted and will be replaced later on (see missing)
          first <- min(which.max(cresc), which.min(cresc))+1
          last <- max(which.max(cresc), which.min(cresc))
          #TAG ERROR: Here, first and last are not correctly defined. They MUST be inside the interval (ab-2):(ab+2)
          X[first:last] <- NA
          code_corr[first:last] = 1
        } else {
          # 2nd case: increase of more than 5 cm/yr with no return to initial values
          # we trust the "new measurements and change the 1st set of"block" with more values
          if (sum(!is.na(X[1:ab]))<sum(!is.na(X))/2)
          {
            X[1:ab] <- X[1:ab] + cresc_abs[which.max(cresc)] - meancresc*diff(tm)[ab]
          } else {
            X[(ab+1):length(X)] <- X[(ab+1):length(X)] - cresc_abs[which.max(cresc)] + meancresc*diff(tm)[ab]
          }
          code_corr[1:ab] = 2
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
        X[ab+1] <- NA; code_corr[ab+1] = 3 }

      # 2nd case: one high value and then return to a "normal" dbh
      else if ( ab > 1 & sum((cresc_abs)[ab], (cresc_abs)[ab-1]) >= 0 ) { X[ab] <- NA; code_corr[ab] = 3 }

      # 3rd case: no return to initial values: retrieve difference from 1st set of values (trust last measurements)
      # probably a new measurement height (because of buttresses, ...): we change the 2nd set of values
      else { X[-(1:ab)] <- X[-(1:ab)] - min(cresc_abs) + meancresc*diff(tm)[ab]
      code_corr[-(1:ab)] = 4 }
    }

    ## update cresc
    cresc <- rep(0, length(X)-1)
    cresc_abs <- rep(0, length(X)-1)
    if (sum(!is.na(X))>1) {
      cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
      cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
    }
  }

  X = repl_missing(X,tm,status)

  # return(cbind(X,code_corr))
  return(c(X,code_corr))
}


repl_missing <- function(Y, time, status) {
  miss <- which(is.na(Y) & status == 1)
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
