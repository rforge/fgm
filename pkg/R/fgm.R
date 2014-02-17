fgm <-
function(xy, group=1, marks, iter=999, ratio=1, scale.seq=seq(from=0, to=max(dist(xy)), length.out=21)[2:21], bootstrap=FALSE, correlate=FALSE){  
  
  # check data input
  
  if (dim(xy)[2]!=2) {stop("xy does not have the right dimensions")}
  if (!is.numeric(xy)) {stop("xy must be be numeric")}  
  if (!is.numeric(marks)) {stop("marks must be be numeric")}
  if (group!=1) {if (!(dim(xy)[1]==length(group) && dim(xy)[1]==dim(marks)[1] && length(group)==dim(marks)[1])){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy, group, marks")}}
  if (group==1) {if (dim(xy)[1]!=dim(marks)[1]){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy, marks")}}
  if (!is.numeric(iter)) {stop("iter needs to be numeric")}  
  if (!is.numeric(ratio)) {stop("ratio needs to be numeric")}  
  if (!is.logical(bootstrap)) {stop("bootstrap needs to be logical")}   
  if (length(iter)!=1) {stop("iter is not allowed to have more than one value")}  
  if (length(ratio)!=1) {stop("ratio is not allowed to have more than one value")}  
  if (length(bootstrap)!=1) {stop("bootstrap is not allowed to have more than one value")}   
  
  time1 <- Sys.time()
  
  xy <- as.matrix(xy)
  group <- as.integer(as.factor(group))
  
  if(correlate==FALSE){
    perms <- m.list <- v.list <- list()
  }else{
    perms <- c.list <- list()
    if(correlate==TRUE){correlate=="pearson"}
    cor.est <- function(x,y, ...){cor.test(x,y, ...,method=correlate)$estimate}
  }
  
  cat("\n")
  cat("======================","\n")
  cat(" Floating Grid Method","\n")
  cat("======================","\n")
  cat("\n")
  cat("Progress bar for spatially restricted permutations","\n")
  cat("0% |------------------------------------------------| 100%","\n")
  cat("   ")
 
  for(i in 1:length(scale.seq)){
    
    scale <- scale.seq[i]
    
    if(bootstrap==FALSE){
      perms[[i]] <- fgrand(xy=xy,z=1:dim(xy)[1], scale=scale, group=group, iter=iter, FUN=fyshuffle, add.obs=TRUE)
    }else{
      perms[[i]] <- fgrand(xy=xy,z=1:dim(xy)[1], scale=scale, group=group, iter=iter, FUN=function(x){x[sample.int(length(x),replace=TRUE)]}, add.obs=TRUE)      
    }

    if(correlate==FALSE){
      m.list[[i]] <- cal.stat(perms[[i]],marks,mean, na.rm=TRUE)
      v.list[[i]] <- cal.stat(perms[[i]],marks,var, na.rm=TRUE)
    }else{
      c.list[[i]] <- cal.stat(perms[[i]],marks,cor.est, na.rm=TRUE)
    }
    
    if(length(scale.seq)<50){
     cat(rep("=",diff(round(seq(from=0,to=50,length.out=length(scale.seq)+1)))[i]), sep="")
    }else{
      if(i %in% ceiling(1:50*(length(scale.seq)/50))){
        cat("=")
        }  
      }
    } # end of iterations
 
  cat("\n")
  cat("\n")
  cat("Non-spatial permutation test is now running.......","\n")
  
  perms[[length(scale.seq)+1]] <- c(observed=list(1:dim(xy)[1]),replicate(iter,list(fyshuffle(1:dim(xy)[1]))))
  if(correlate==FALSE){
    m.list[[length(scale.seq)+1]] <- cal.stat(perms[[i]],marks,mean, na.rm=TRUE)
    v.list[[length(scale.seq)+1]] <- cal.stat(perms[[i]],marks,var, na.rm=TRUE)
  }else{
    c.list[[length(scale.seq)+1]] <- cal.stat(perms[[i]],marks,cor.est, na.rm=TRUE)
  }
    
  cat("\n")
  time2 <- round(Sys.time()-time1,2)
  cat("\n")
  cat(paste("The analysis took", time2,attr(time2,"unit")),"\n")
  cat("\n")
  
  if(correlate==FALSE){
    output <- list(m.list=m.list, v.list=v.list, iter=iter, scales=c(scale.seq,Inf),correlate=correlate)  
  }else{
    output <- list(c.list=c.list, iter=iter, scales=c(scale.seq,Inf),correlate=correlate)    
  }
  
  class(output)<-"fgm"
  
  return(output)
  
  }
