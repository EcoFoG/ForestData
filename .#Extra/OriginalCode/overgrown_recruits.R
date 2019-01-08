### overgrown recruit
if ( first_census<min(tm) & sum(!is.na(X))>0 ) {    ## recruited during measurement interval
  # recruitment year
  if( X[1] > limit + diff(tm)*5) {  ### tolerated growth rate: 5cm/yr
    coef <- lm(X[!is.na(X)]~tm[!is.na(X)])$coefficients
    if (is.na(coef[2])) { X[is.na(status)] <- coef[1] } ### only 1 dbh value: replace all non-recruited dbh by this value
    else { X[is.na(status)] <- min(coef[1] + tm[is.na(status)]*coef[2], X[!is.na(X)][1]) }
  } 
}
