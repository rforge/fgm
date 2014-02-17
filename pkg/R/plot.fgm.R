plot.fgm <-
function(fgm, plane=0, plot.zero=TRUE){
  
  if (class(fgm)!="fgm") {stop("Object needs to be of class \"fgm\"")}
  if (plane!=0 & plane!=1 & plane!=2) {stop("plane needs to be either 0,1 or 2")}
  if (!is.logical(plot.zero)) {stop("plot.zero needs to be logical")}
  
  par.fig <- par("fig")
  par.mar <- par("mar")
  par.ask <- par("ask")
 
  if(plot.zero==TRUE){
    xlims <- range(c(0,fgm$scales[-length(fgm$scales)]))
  }else{
    xlims <- range(fgm$scales)
  }

  if(fgm$correlate==TRUE){
  
    if(plane %in% c(0,1)){
    
      par(mfrow=c(1,1))
      par(fig=c(0,0.9,0,1))
      par(mar=c(5,5,2,0))
    
      ylims <- range(unlist(fgm$m.list))
    
      plot(fgm$scales,sapply(fgm$m.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="mean")
      segments(fgm$scales,sapply(fgm$m.list,quantile,probs=0.025,na.rm = TRUE),fgm$scales,sapply(fgm$m.list,quantile,probs=0.975,na.rm = TRUE))
      abline(h=fgm$m.list[[1]][1], lwd=2, col="red")
    
      par(fig=c(0.9,1,0,1),new=TRUE)
      par(mar=c(5,0.2,2,2))
      plot(0,sapply(fgm$m.list,mean,na.rm = TRUE)[length(fgm$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
      axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
      segments(0,sapply(fgm$m.list,quantile,probs=0.025,na.rm = TRUE)[length(fgm$scales)],0,sapply(fgm$m.list,quantile,probs=0.975,na.rm = TRUE)[length(fgm$scales)])
      abline(h=fgm$m.list[[1]][1], lwd=2, col="red")
      }
  
    if(plane==0){par(ask=TRUE)}
  
    if(plane %in% c(0,2)){
    
      par(mfrow=c(1,1))
      par(fig=c(0,0.9,0,1))
      par(mar=c(5,5,2,0))
    
      ylims <- range(unlist(fgm$v.list))
    
      plot(fgm$scales,sapply(fgm$v.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="variance")
      segments(fgm$scales,sapply(fgm$v.list,quantile,probs=0.025,na.rm = TRUE),fgm$scales,sapply(fgm$v.list,quantile,probs=0.975,na.rm = TRUE))
      abline(h=fgm$v.list[[1]][1], lwd=2, col="red")
    
      par(fig=c(0.9,1,0,1),new=TRUE)
      par(mar=c(5,0.2,2,2))
      plot(0,sapply(fgm$v.list,mean,na.rm = TRUE)[length(fgm$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
      axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
      segments(0,sapply(fgm$v.list,quantile,probs=0.025,na.rm = TRUE)[length(fgm$scales)],0,sapply(fgm$v.list,quantile,probs=0.975,na.rm = TRUE)[length(fgm$scales)])
      abline(h=fgm$v.list[[1]][1], lwd=2, col="red")
    }
  
  }else{
  
    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    
    ylims <- range(unlist(fgm$c.list))
    
    plot(fgm$scales,sapply(fgm$c.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="correlation")
    segments(fgm$scales,sapply(fgm$c.list,quantile,probs=0.025,na.rm = TRUE),fgm$scales,sapply(fgm$c.list,quantile,probs=0.975,na.rm = TRUE))
    abline(h=fgm$c.list[[1]][1], lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,sapply(fgm$c.list,mean,na.rm = TRUE)[length(fgm$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n")
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,sapply(fgm$c.list,quantile,probs=0.025,na.rm = TRUE)[length(fgm$scales)],0,sapply(fgm$c.list,quantile,probs=0.975,na.rm = TRUE)[length(fgm$scales)])
    abline(h=fgm$c.list[[1]][1], lwd=2, col="red")
    }
  
  par(fig=par.fig)
  par(mar=par.mar)
  par(ask=par.ask)
  }
