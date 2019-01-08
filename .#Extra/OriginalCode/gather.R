## gather : reassembles 2 sets of values for 1 individual
gather <- function(X, tm, ab){
  n <- length(X)
  # ab : position of excessive difference 
  # lm for 2 subsets of dbh : before (1) and after(2) excessive difference
  coef1<- lm(X[1:ab] ~ tm[1:ab])$coefficients
  coef2 <- lm(X[(ab+1):n]~tm[(ab+1):n])$coefficients
  
  if (ab>1 & (n-ab)>1) {
    
    delta1<-(coef1[2]*tm[ab+1]+coef1[1])-(coef2[2]*tm[ab+1]+coef2[1])
    delta2<-(coef1[2]*tm[ab]+coef1[1])-(coef2[2]*tm[ab]+coef2[1])
    delta<-(delta1+delta2)/2
    
    if (ab>n/2) {
      X[(ab+1):n]<-X[(ab+1):n]+delta
    } else {
      X[1:ab]<-X[1:ab]-delta
    }
    
  } else if (ab==1) {
    X[1]<-coef2[2]*tm[1]+coef2[1]
  } else {
    X[n]<-coef1[2]*tm[n]+coef1[1]
  }
}