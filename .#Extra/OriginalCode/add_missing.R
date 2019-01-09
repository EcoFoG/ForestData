add_missing = function(yr, id){ #, d, mind){
  # mindbh = min(mind)
  # dat = data.table(year = yr,idtree=id,dbh=d)
  ## all possible measures = years x idtrees
  dat_compl = expand.grid(year=unique(yr),idtree=unique(id))
  # dat = merge(dat,dat_compl,by=c("idtree","year"),all=TRUE)
  # dat = dat[order(idtree,year)]
  # # # overgrown recruits
  # # recruits = dat[,.(min(year[!is.na(dbh)])>min(year)),.(idtree)]
  # # recruits = subset(dat, idtree %in% recruits$idtree[recruits$V1])
  # # overgrown = recruits[,.((first(dbh[!is.na(dbh)])>= mindbh + 5*diff(year)[first(which(!is.na(dbh)))-1])), by=.(idtree)]
  # # overgrown_id = as.character(overgrown$idtree[overgrown$V1])
  # # dt_overgrown = subset(dat, idtree %in% overgrown_id )
  # # dt_overgrown = dt_overgrown[order(idtree,year)]
  # # dt_overgrown = dt_overgrown[,.(year[1:max(which(!is.na(dbh)))],dbh[1:max(which(!is.na(dbh)))]),.(idtree)]
  # # # # # for other than overgrown 
  # # dt_normal = subset(dat, !(idtree %in% overgrown_id))
  # # # remove all measurements before recruitement and after death
  # # dt_normal = dt_normal[,.(year[min(which(!is.na(dbh))):max(which(!is.na(dbh)))],dbh[min(which(!is.na(dbh))):max(which(!is.na(dbh)))]),.(idtree)]
  # # colnames(dt_normal) = c("idtree","year","dbh")
  # # dt = rbind(dt_overgrown, dt_normal )
  # # dat = dat[,.(year,dbh,tree_status(dbh)),.(idtree)]
  dat_compl$id = paste(dat_compl$idtree,dat_compl$year)
  return(dat_compl$id)
}
