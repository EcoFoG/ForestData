## test
dat = subset(data, idtree == "prg_1_0_21998")
X = dat$dbh
tm = dat$year
limit = 10
ladder = dat$ladder

dbh_correction <- function(X,
                           tm,
                           limit = 20,
                           first_census = NULL,
                           ladder = NULL) {
  # Xsav if for browser() use: save initial value of X
  Xsav <- X
  limit = min(limit)
  # first_census=min(c(first_census,tm))
  
  # cresc_abs: absolute annual diameter increment
  cresc <- rep(0, length(X) - 1)
  cresc_abs <- rep(0, length(X) - 1)
  if (sum(!is.na(X)) > 1) {
    cresc[which(!is.na(X))[-1] - 1] <-
      diff(X[!is.na(X)]) / diff(tm[!is.na(X)])
    cresc_abs[which(!is.na(X))[-1] - 1] <- diff(X[!is.na(X)])
  }
  
  if (length(cresc) > 0) {
    
    ### First, correct when there is a raised measurement
    # but some are very weird: is it worth changing them? 
    # if (sum(ladder,na.rm=TRUE) > 0) {
    #   raised = which(diff(c(NA, ladder)) == 1)
    #   if (length(raised) == 0 |
    #       isTRUE(X[raised] > X[raised - 1]) ) {
    #     raised = which.min(cresc) + 1 
    #   }
    #   X[raised:length(X)] = X[raised:length(X)] + (X[raised - 1] - X[raised]) + mean(cresc[-(raised -
    #                                                                                            1)])
    # }
    
    
    ####    if there is a DBH change > 5cm/year or < -2 cm   ####
    ### do as many corrections as there are abnormal DBH change values ###
    cresc_abn = sum(abs(cresc) >= 5 | cresc_abs < -2)
    if (cresc_abn > 0) {
      for (i in 1:cresc_abn) {
        # begin with the census with the highest DBH change
        ab <- which.max(abs(cresc))
        
        # check if this census is truly abnormal
        if (abs(cresc[ab]) >= 5 | cresc_abs[ab] < -2) {
          # values surrounding ab
          surround = c(ab - 2, ab - 1, ab + 1, ab + 2)
          # that have a meaning (no NAs or 0 values)
          surround = surround[surround > 0 &
                                surround <= length(cresc)]
          
          # mean DBH change around ab
          meancresc = max(mean(cresc[surround], na.rm = TRUE), 0)
          
          # moment of max and min DBH changes around ab (including ab, that should be one of the 2)
          sourround_ab = sort(c(surround, ab))
          up = sourround_ab[which.max(cresc[sourround_ab])]
          down = sourround_ab[which.min(cresc[sourround_ab])]
          
          if (length(surround) > 0) {
            # 1st case : excessive increase/decrease offset by a similar decrease in dbh, plus 5cm/yr
            # is there a value that could compensate the excessive DBH change?
            # check if removing those values would solve the problem (ie cresc < 5 & cresc_abs > -2 )
            if (isTRUE(down > up & cresc[up] * cresc[down] < 0 &
                       # first an increase and then a decrease in DBH
                       (X[down + 1] - X[up]) / (tm[down + 1] - tm[up])  < 5 &
                       X[down + 1] - X[up] > -2) |
                isTRUE(up > down & cresc[up] * cresc[down] < 0 &
                       # first an decrease and then a increase in DBH
                       (X[up + 1] - X[down]) / (tm[up + 1] - tm[down])  < 5 &
                       X[up + 1] - X[down] > -2)) {
              # correction: abnormal values are deleted and will be replaced later on (see missing)
              first <- min(up, down) + 1
              last <- max(up, down)
              X[first:last] <- NA
            }
            
            
            # 2nd case: abnormal DBH change with no return to initial values
            # we trust the set of measurements with more values
            # if they are the same size, then we trust the last one
            # ladders?
            else {
              if ((sum(!is.na(X[1:ab])) > sum(!is.na(X))/2) | isTRUE(ladder[ab] == 0 & ladder[ab+1] == 1)) {
                X[(ab + 1):length(X)] <-
                  X[(ab + 1):length(X)] - cresc_abs[which.max(abs(cresc))] + meancresc *
                  diff(tm)[ab]
              } else {
                X[1:ab] <-
                  X[1:ab] + (X[ab+1]-X[ab]) - meancresc * diff(tm)[ab]
              } 
            }
          }
          
          # cresc_abs: absolute annual diameter increment
          cresc <- rep(0, length(X) - 1)
          cresc_abs <- rep(0, length(X) - 1)
          if (sum(!is.na(X)) > 1) {
            cresc[which(!is.na(X))[-1] - 1] <-
              diff(X[!is.na(X)]) / diff(tm[!is.na(X)])
            cresc_abs[which(!is.na(X))[-1] - 1] <- diff(X[!is.na(X)])
          }
        }
      }
    }
    
    
    ## replace missing values
    if (any(!is.na(X))) { X <- repl_missing(X, tm) } else { X = rep(0, length(X)) }
  }
  return(X)
}