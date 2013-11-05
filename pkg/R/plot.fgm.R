plot.fgm <-
function(fgm, plane=0, plot.zero=TRUE){
  
  if (class(fgm)!="fgm") {stop("Object needs to be of class \"fgm\"")}
  if (plane!=0 & plane!=1 & plane!=2 & plane!=3) {stop("plane needs to be either 0,1,2 or 3")}
  if (!is.logical(plot.zero)) {stop("plot.zero needs to be logical")}
  
  par.fig <- par("fig")
  par.mar <- par("mar")
  par.ask <- par("ask")
  
  if(plot.zero==TRUE){
    xlims <- range(c(0,sapply(fgm$perm,'[',1)[-length(fgm$perm)]))
  }else{
    xlims <- range(sapply(fgm$perm,'[',1)[-length(fgm$perm)])
  }

  if(plane %in% c(0,1)){
    
    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    
    ylims <- range(c(fgm$obsv$m,sapply(sapply(fgm$perm,'[',2),quantile,probs=0.025,na.rm = TRUE),sapply(sapply(fgm$perm,'[',2),quantile,probs=0.975,na.rm = TRUE))) 
    
    plot(sapply(fgm$perm,'[',1),sapply(sapply(fgm$perm,'[',2),mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="global mean")
    segments(unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',2),quantile,probs=0.025,na.rm = TRUE),unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',2),quantile,probs=0.975,na.rm = TRUE))
    abline(h=fgm$obsv$m, lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,sapply(sapply(fgm$perm,'[',2),mean,na.rm = TRUE)[length(fgm$perm)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,sapply(sapply(fgm$perm,'[',2),quantile,probs=0.025,na.rm = TRUE)[length(fgm$perm)],0,sapply(sapply(fgm$perm,'[',2),quantile,probs=0.975,na.rm = TRUE)[length(fgm$perm)])
    abline(h=fgm$obsv$m, lwd=2, col="red")
  }
  
  if(plane==0){par(ask=TRUE)}
  
  if(plane %in% c(0,2)){
    
    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    
    ylims <- range(c(fgm$obsv$v,sapply(sapply(fgm$perm,'[',3),quantile,probs=0.025,na.rm = TRUE),sapply(sapply(fgm$perm,'[',3),quantile,probs=0.975,na.rm = TRUE))) 
    
    plot(sapply(fgm$perm,'[',1),sapply(sapply(fgm$perm,'[',3),mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="global variance")
    segments(unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',3),quantile,probs=0.025,na.rm = TRUE),unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',3),quantile,probs=0.975,na.rm = TRUE))
    abline(h=fgm$obsv$v, lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,sapply(sapply(fgm$perm,'[',3),mean,na.rm = TRUE)[length(fgm$perm)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,sapply(sapply(fgm$perm,'[',3),quantile,probs=0.025,na.rm = TRUE)[length(fgm$perm)],0,sapply(sapply(fgm$perm,'[',3),quantile,probs=0.975,na.rm = TRUE)[length(fgm$perm)])
    abline(h=fgm$obsv$v, lwd=2, col="red")
    }
    
  if(plane %in% c(0,3)){
    
    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    
    ylims <- range(c(0,sapply(sapply(fgm$perm,'[',4),quantile,probs=0.025,na.rm = TRUE),sapply(sapply(fgm$perm,'[',4),quantile,probs=0.975,na.rm = TRUE))) 
    
    plot(sapply(fgm$perm,'[',1),sapply(sapply(fgm$perm,'[',4),mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="local variance")
    segments(unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',4),quantile,probs=0.025,na.rm = TRUE),unlist(sapply(fgm$perm,'[',1)),sapply(sapply(fgm$perm,'[',4),quantile,probs=0.975,na.rm = TRUE))
    abline(h=0, lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,sapply(sapply(fgm$perm,'[',4),mean,na.rm = TRUE)[length(fgm$perm)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,sapply(sapply(fgm$perm,'[',4),quantile,probs=0.025,na.rm = TRUE)[length(fgm$perm)],0,sapply(sapply(fgm$perm,'[',4),quantile,probs=0.975,na.rm = TRUE)[length(fgm$perm)])
    abline(h=0, lwd=2, col="red")
    }
   
  par(fig=par.fig)
  par(mar=par.mar)
  par(ask=par.ask)
  }
