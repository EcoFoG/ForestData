threshold = function(X, tm, thres){
 if (sum(!is.na(X) & X>= thres)>0 & sum(!is.na(X) & X<thres)>0){
   ## first measurement > threshold
   frstup = which(!is.na(X) & X>=thres)[1]
   inf = which(X<thres)
   odd = inf[inf>frstup]
   if (length(odd)>0) X[odd] = NA; X = repl_missing(X,tm); X[odd] = max(50,X[odd])
 }
  return(X)
}