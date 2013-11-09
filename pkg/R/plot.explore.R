plot.explore <- 
  function(fgm, plane=0){
  
  if (class(fgm)!="fgm") {stop("Object needs to be of class \"fgm\"")}
  if (plane!=0 & plane!=1 & plane!=2) {stop("plane needs to be either 0,1 or 2")}
  
  
  scale <- gm <- gv <- array(NA, dim=length(fgm$perm))
  for(i in 1:length(fgm$perm)){
    scale[i] <- fgm$perm[[i]]$scale
    gm[i] <- fgm$perm[[i]]$gm
    gv[i] <- fgm$perm[[i]]$gv  
  }
  
  if(plane %in% c(0,1)){ 
    plot(scale,gm, pch=16, col="#00000040", ylab="global mean")
    abline(h=fgm$obsv$m, col="red", lwd=2)
  }
  
  if(plane==0){par(ask=TRUE)}
  
  if(plane %in% c(0,2)){
    plot(scale,gv, pch=16, col="#00000040", ylab="global variance")
    abline(h=fgm$obsv$v, col="red", lwd=2)
  }
}