performance.plots <-
function(fgm){

  par.mfrow <- par("mfrow")
  par.mar <- par("mar")
  
  par(mfrow=c(2,2))
  par(mar=c(5,5,2,2))
  plot(fgm$test.scale,fgm$no.cells, pch=16, xlab="grid cell size", ylab="# cells", main="Number of randomized cells", col="#00000020")
  plot(fgm$test.scale,fgm$no.pairs, pch=16, xlab="grid cell size", ylab="# pairs", main="Number of accomplished pairs", col="#00000020")
  plot(fgm$test.scale,fgm$no.mates[,1], pch=16, xlab="grid cell size", ylab="mean(# options)", main="Number of individuals in partner pool for sex 1", col="#00000020")
  plot(fgm$test.scale,fgm$no.mates[,2], pch=16, xlab="grid cell size", ylab="mean(# options)", main="Number of individuals in partner pool for sex 2", col="#00000020")
  
  par(mfrow=par.mfrow)
  par(mar=par.mar)
  }
