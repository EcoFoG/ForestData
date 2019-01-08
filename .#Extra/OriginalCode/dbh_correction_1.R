## test
dat = subset(data,idtree=="prg_3_0_18005")
X=dat$dbh
tm=dat$year
limit=10

dbh_correction <- function(X, tm, limit=20, first_census=NULL) {
  # Xsav if for browser() use: save initial value of X
  Xsav <- X
  limit=min(limit)
  # first_census=min(c(first_census,tm))
  
  # cresc_abs: absolute annual diameter increment
  cresc <- rep(0, length(X)-1)
  cresc_abs <- rep(0, length(X)-1)
  if (sum(!is.na(X))>1) {
    cresc[which(!is.na(X))[-1]-1] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
    cresc_abs[which(!is.na(X))[-1]-1] <- diff(X[!is.na(X)])
  }
  
  if (length(cresc)>0){
    
    #### (1) change > 5cm/year ####
    cresc5 = sum(abs(cresc)>=5)
    if (cresc5>0) {
      for (i in 1:cresc5){
        # census with excessive increase in dbh
        ab <- which.max(abs(cresc))
        if (abs(cresc[ab])>=5){
          # surrounding values
          existing <- c(ab-2,ab-1,ab+1,ab+2)
          existing <- existing[existing > 0 & existing <= length(cresc)]
          meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)
          existing_and_ab = (ab-2):(ab+2)
          existing_and_ab <- existing_and_ab[existing_and_ab > 0 & existing_and_ab <= length(cresc)]
            
          up <- which.max(cresc[existing_and_ab])
          down <- which.min(cresc[existing_and_ab])
          compensate <- which(abs(cresc_abs) == max(abs(cresc_abs[existing])))
          compensate <- compensate[compensate %in% existing]
            
          # 1st case : excessive increase/decrease offset by a similar decrease in dbh, plus 5cm/yr
          if ( length(existing)>0 ) { 
            if (isTRUE(down>up & abs(cresc_abs[ab] + cresc_abs[compensate])  < 5*(tm[existing_and_ab[down]+1] - tm[existing_and_ab[up]]) ) |
                isTRUE(up>down &  abs(cresc_abs[ab] + cresc_abs[compensate]) < 5*(tm[existing_and_ab[up]+1] - tm[existing_and_ab[down]])) ){
              # correction: abnormal values are deleted and will be replaced later on (see missing)
              first <- min(which.max(cresc), which.min(cresc))+1
              last <- max(which.max(cresc), which.min(cresc))
              X[first:last] <- NA
            } else {
              # 2nd case: increase of more than 5 cm/yr with no return to initial values
              # we trust the "new measurements and change the 1st set of"block" with more values
              if (sum(!is.na(X[1:ab]))<sum(!is.na(X))/2) {
                X[1:ab] <- X[1:ab] + cresc_abs[which.max(cresc)] - meancresc*diff(tm)[ab] } else { 
                  X[(ab+1):length(X)] <- X[(ab+1):length(X)] - cresc_abs[which.max(abs(cresc))] + meancresc*diff(tm)[ab]}
            } }
          
          ## update cresc
          cresc <- rep(0, length(X)-1)
          cresc_abs <- rep(0, length(X)-1)
          if (sum(!is.na(X))>1) {
            cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
            cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
          }
        }
        
      }
    }
    
    
    
    #### (2)  decrease > 2cm ####
    cresc2 = sum(min(cresc_abs)<=(-2))
    if (cresc2>0) {
      for (i in 1:cresc2){
        # census with excessive decrease in dbh
        ab <- which.min(cresc_abs)
        if(cresc_abs[ab]<= -2){
          # surrounding values
          existing <- c(ab-2,ab-1,ab+1,ab+2)
          existing <- existing[existing > 0 & existing < length(cresc)]
          meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)
          
          # 1st case: one low value and then return to a "normal" dbh
          if ( ab < length(cresc) & ( sum( (cresc_abs)[ab], (cresc_abs)[ab+1]) >= 0 ) ) { 
            X[ab] <- NA } 
          
          # 2nd case: one high value and then return to a "normal" dbh
          else if ( ab > 1 & sum((cresc_abs)[ab], (cresc_abs)[ab-1]) >= 0 ) { X[ab-1] <- NA }
          
          # 3rd case: no return to initial values: retrieve difference from 1st set of values (trust last measurements)
          # probably a new measurement height (because of buttresses, ...): we change the 2nd set of values
          # no more than 2cm/year
          else { X[-(1:ab)] <- X[-(1:ab)] - min(cresc_abs) + min(2,meancresc)*diff(tm)[ab] }    
          ## update cresc
          cresc <- rep(0, length(X)-1)
          cresc_abs <- rep(0, length(X)-1)
          if (sum(!is.na(X))>1) {
            cresc[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])/diff(tm[!is.na(X)])
            cresc_abs[which(!is.na(X))[-1]] <- diff(X[!is.na(X)])
          }
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
    
    ## replace missing values
    X <- repl_missing(X, tm)
  }
  return(X)
}