mcfgm <-
function(xy1, xy2=xy1, group1=1, group2=group1, trait.table, pairs=cbind(1:dim(xy1)[1],1:dim(xy1)[1]), iter=999, ratio=1, scale.seq=seq(from=0, to=max(dist(rbind(xy1,xy2))), length.out=21)[2:21], two.types=TRUE, replacement=FALSE, self.loop=TRUE){  
  
  # check data input
  
  if (dim(xy1)[2]!=2) {stop("xy1 does not have the right dimensions")}
  if (!is.numeric(xy1)) {stop("xy1 needs to be numeric")}  
  if (dim(xy2)[2]!=2) {stop("xy2 does not have the right dimensions")}  
  if (!is.numeric(xy2)) {stop("xy2 needs to be numeric")}
  if (!is.numeric(trait.table)) {stop("trait.table needs to be numeric")}
  if (group1!=1) {if (!(dim(xy1)[1]==length(group1) && dim(xy1)[1]==dim(trait.table)[1] && length(group1)==dim(trait.table)[1])){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy1, group1, trait.table")}}
  if (group1==1) {if (dim(xy1)[1]!=dim(trait.table)[1]){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy1, trait.table")}}
  if (group2!=1) {if (!(dim(xy2)[1]==length(group2) && dim(xy2)[1]==dim(trait.table)[2] && length(group2)==dim(trait.table)[2])){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy2, group2, trait.table")}}
  if (group2==1) {if (dim(xy2)[1]!=dim(trait.table)[2]){stop("one or more of the following arrays does not have the appropriate length or dimensions: xy2, trait.table")}}
  if (!is.numeric(iter)) {stop("iter needs to be numeric")}  
  if (!is.numeric(ratio)) {stop("ratio needs to be numeric")}  
  if (!is.logical(replacement)) {stop("replacement needs to be logical")}   
  if (!is.logical(self.loop)) {stop("self.loop needs to be logical")}
  if (length(iter)!=1) {stop("iter is not allowed to have more than one value")}  
  if (length(ratio)!=1) {stop("ratio is not allowed to have more than one value")}  
  if (length(replacement)!=1) {stop("replacement is not allowed to have more than one value")}   
  if (length(self.loop)!=1) {stop("self.loop is not allowed to have more than one value")} 
  
  library(multicore)
  
  time1 <- Sys.time()
  
  xy1 <- as.matrix(xy1)
  group1 <- as.integer(as.factor(group1))
  xy2 <- as.matrix(xy2)
  group2 <- as.integer(as.factor(group2))
  
  cat("\n")
  cat("=============================================","\n")
  cat(" Floating Grid Method for parallel computing","\n")
  cat("=============================================","\n")
  cat("\n")
  cat("Progress bar for spatial permutations","\n")
  cat("0% |------------------------------------------------| 100%","\n")
  cat("   ")
  
  perm.list <- list()
  
  for(i in 1:length(scale.seq)){
    
    scale <- scale.seq[i]
    
    perms <- mclapply(1:iter,run.perm, scale=scale, xy1=xy1, xy2=xy2, group1=group1, group2=group2, trait.table=trait.table, pairs=pairs, replacement=replacement, self.loop=self.loop, ratio=ratio, mc.set.seed=TRUE, mc.preschedule=TRUE)
    
    perm.list[[i]] <- list(scale=scale,
                           gm=unlist(lapply(perms,'[',1)),
                           gv=unlist(lapply(perms,'[',2)),
                           lv=unlist(lapply(perms,'[',3)),
                           nc=unlist(lapply(perms,'[',4)),
                           np=unlist(lapply(perms,'[',5)),
                           nm1=unlist(lapply(perms,'[',6)),
                           nm2=unlist(lapply(perms,'[',7)))
    
    if(length(scale.seq)<50){
      cat(rep("=",diff(round(seq(from=0,to=50,length.out=length(scale.seq)+1)))[i]), sep="")
    }else{
      if(i %in% ceiling(1:50*(length(scale.seq)/50))){
        cat("=")
      }  
    }
  } # end of iterations
  
  obsv <- list(m=mean(diag(trait.table[pairs[,1],pairs[,2]])), v=var(diag(trait.table[pairs[,1],pairs[,2]])))
  
  cat("\n")
  cat("\n")
  cat("Non-spatial permutation test is now running.......","\n")
  
  gm <- gv <- lv <-  array(NA, dim=iter)
  
  for(t in 1:iter){
    gm[t] <- mean(diag(trait.table[,fyshuffle(1:dim(xy2)[1])]))
    gv[t] <- var(diag(trait.table[,fyshuffle(1:dim(xy2)[1])]))
    lv[t] <- gv[t]-obsv$v       
  }
  
  perm.list[[length(scale.seq)+1]] <- list(scale=Inf,
                                           gm=gm,
                                           gv=gv,
                                           lv=lv,
                                           nc=rep(1,iter),
                                           np=rep(min(dim(trait.table)),iter),
                                           nm1=rep(dim(trait.table)[2],iter),
                                           nm2=rep(dim(trait.table)[1],iter))
  
  cat("\n")
  time2 <- round(Sys.time()-time1,2)
  cat("\n")
  cat(paste("The analysis took", time2,attr(time2,"unit")),"\n")
  cat("\n")
  
  output <- list(obsv=obsv, perm=perm.list, iter=iter)
  
  class(output)<-"fgm"
  
  return(output)
  
}
