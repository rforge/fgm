plot.fg <-
function(fg, plane=0, plot.zero=TRUE, sign.level=0.05){
  
  if (class(fg)!="fg") {stop("Object needs to be of class \"fg\"")}
  if (plane!=0 & plane!=1 & plane!=2) {stop("plane needs to be either 0,1 or 2")}
  if (!is.logical(plot.zero)) {stop("plot.zero needs to be logical")}
  
  p.val <- function(perm,obsv){
    return(min((sum(perm>=obsv))/(sum(is.na(perm)==FALSE))*2,(sum(perm<=obsv))/(sum(is.na(perm)==FALSE))*2,1))   
  }
  
  par.fig <- par("fig")
  par.mar <- par("mar")
  par.ask <- par("ask")
 
  if(plot.zero==TRUE){
    xlims <- range(c(0,fg$scales[-length(fg$scales)]))
  }else{
    xlims <- range(fg$scales)
  }
  
  if(fg$pairwise==FALSE){
    
    pvals <- sapply(fg$c.list,p.val,obsv=fg$c.list[[1]][1])
    cols <- ifelse(pvals<=sign.level,"blue","black")

    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    ylims <- range(fg$c.list[[1]][1]-unlist(fg$c.list))
    plot(fg$scales,fg$c.list[[1]][1]-sapply(fg$c.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, col=cols[-length(fg$scales)], xlab="grid cell size", ylab="unexplained Moran's I")
    segments(fg$scales,fg$c.list[[1]][1]-sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE),fg$scales,fg$c.list[[1]][1]-sapply(fg$c.list,quantile,probs=0.975,na.rm = TRUE), col=cols[-length(fg$scales)])
    abline(h=0, lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,fg$c.list[[1]][1]-sapply(fg$c.list,mean,na.rm = TRUE)[length(fg$scales)], ylim=ylims, pch=16, col=cols[length(fg$scales)], xlab="", ylab="",xaxt="n", yaxt="n")
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,fg$c.list[[1]][1]-sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE)[length(fg$scales)],0,fg$c.list[[1]][1]-sapply(fg$c.list,quantile,probs=0.975,na.rm = TRUE)[length(fg$scales)],col=cols[length(fg$scales)])
    abline(h=0, lwd=2, col="red")
    
  }else if (fg$correlate==FALSE){
    if(plane %in% c(0,1)){
      
      pvals <- sapply(fg$m.list,p.val,obsv=fg$m.list[[1]][1])
      cols <- ifelse(pvals<=sign.level,"blue","black")
      
      par(mfrow=c(1,1))
      par(fig=c(0,0.9,0,1))
      par(mar=c(5,5,2,0))
      ylims <- range(unlist(fg$m.list))
      plot(fg$scales,sapply(fg$m.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, col=cols[-length(fg$scales)], xlab="grid cell size", ylab="mean")
      segments(fg$scales,sapply(fg$m.list,quantile,probs=0.025,na.rm = TRUE),fg$scales,sapply(fg$m.list,quantile,probs=0.975,na.rm = TRUE), col=cols[-length(fg$scales)])
      abline(h=fg$m.list[[1]][1], lwd=2, col="red")
    
      par(fig=c(0.9,1,0,1),new=TRUE)
      par(mar=c(5,0.2,2,2))
      plot(0,sapply(fg$m.list,mean,na.rm = TRUE)[length(fg$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n",col=cols[length(fg$scales)])
      axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
      segments(0,sapply(fg$m.list,quantile,probs=0.025,na.rm = TRUE)[length(fg$scales)],0,sapply(fg$m.list,quantile,probs=0.975,na.rm = TRUE)[length(fg$scales)],col=cols[length(fg$scales)])
      abline(h=fg$m.list[[1]][1], lwd=2, col="red")
      }
  
    if(plane==0){par(ask=TRUE)}
  
    if(plane %in% c(0,2)){
      
      pvals <- sapply(fg$v.list,p.val,obsv=fg$v.list[[1]][1])
      cols <- ifelse(pvals<=sign.level,"blue","black")
      
      par(mfrow=c(1,1))
      par(fig=c(0,0.9,0,1))
      par(mar=c(5,5,2,0))
      ylims <- range(unlist(fg$v.list))
      plot(fg$scales,sapply(fg$v.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, col=cols[-length(fg$scales)], xlab="grid cell size", ylab="variance")
      segments(fg$scales,sapply(fg$v.list,quantile,probs=0.025,na.rm = TRUE),fg$scales,sapply(fg$v.list,quantile,probs=0.975,na.rm = TRUE), col=cols[-length(fg$scales)])
      abline(h=fg$v.list[[1]][1], lwd=2, col="red")
    
      par(fig=c(0.9,1,0,1),new=TRUE)
      par(mar=c(5,0.2,2,2))
      plot(0,sapply(fg$v.list,mean,na.rm = TRUE)[length(fg$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n", col=cols[length(fg$scales)])
      axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
      segments(0,sapply(fg$v.list,quantile,probs=0.025,na.rm = TRUE)[length(fg$scales)],0,sapply(fg$v.list,quantile,probs=0.975,na.rm = TRUE)[length(fg$scales)], col=cols[length(fg$scales)])
      abline(h=fg$v.list[[1]][1], lwd=2, col="red")
    }

  }else{
    
    pvals <- sapply(fg$c.list,p.val,obsv=fg$c.list[[1]][1])
    cols <- ifelse(pvals<=sign.level,"blue","black")
    
    par(mfrow=c(1,1))
    par(fig=c(0,0.9,0,1))
    par(mar=c(5,5,2,0))
    
    ylims <- range(fg$c.list[[1]][1]-unlist(fg$c.list))
    plot(fg$scales,sapply(fg$c.list,mean,na.rm = TRUE), ylim=ylims, xlim=xlims, pch=16, xlab="grid cell size", ylab="correlation", col=cols[-length(fg$scales)])
    segments(fg$scales,sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE),fg$scales,sapply(fg$c.list,quantile,probs=0.975,na.rm = TRUE), col=cols[-length(fg$scales)])
    abline(h=fg$c.list[[1]][1], lwd=2, col="red")
    
    par(fig=c(0.9,1,0,1),new=TRUE)
    par(mar=c(5,0.2,2,2))
    plot(0,sapply(fg$c.list,mean,na.rm = TRUE)[length(fg$scales)], ylim=ylims, pch=16, xlab="", ylab="",xaxt="n", yaxt="n", col=cols[length(fg$scales)])
    axis(1, at=0, lab=quote(infinity), cex.axis=1.5)
    segments(0,sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE)[length(fg$scales)],0,sapply(fg$c.list,quantile,probs=0.975,na.rm = TRUE)[length(fg$scales)], col=cols[length(fg$scales)])
    abline(h=fg$c.list[[1]][1], lwd=2, col="red")
    }
  par(fig=par.fig)
  par(mar=par.mar)
  par(ask=par.ask)
  }
