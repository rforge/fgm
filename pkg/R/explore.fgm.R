explore.fgm <-
function(xy1, xy2=xy1, group1=1, group2=group1, trait.table, pairs=cbind(1:dim(xy1)[1],1:dim(xy1)[1]), iter=10000, ratio=1, two.types=TRUE, replacement=FALSE, self.loop=FALSE){  
  output <- fgm(xy1=xy1, xy2=xy2, group1=group1, group2=group2, trait.table=trait.table, pairs=pairs, iter=1, ratio=ratio, scale.seq=runif(iter,0,max(dist(cbind(xy1,xy2)))), two.types=two.types, replacement=replacement, self.loop=self.loop)
  return(output) 
}
