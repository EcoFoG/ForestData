plot_corr <- function(id){
  tree=which(data$idtree==id)
  yrange=c(min(data$dbh[tree], data$dbh_c[tree], na.rm=T), 
           max(data$dbh[tree], data$dbh_c[tree], na.rm=T))
  plot(data$dbh_c[tree] ~ data$year[tree], pch=16, col=2, 
       ylim=yrange, main=id, ylab="dbh", xlab="year")
  points(data$dbh[tree] ~ data$year[tree], col=4, pch=16) # a$cohort: number of the recruitment year
}