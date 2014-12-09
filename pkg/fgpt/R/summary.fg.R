summary.fg <-
function(fg){
  
  if (class(fg)!="fg") {stop("Object needs to be of class \"fg\"")}
  
  p.val <- function(perm,obsv){
    return(min((sum(perm>=obsv))/(sum(is.na(perm)==FALSE))*2,(sum(perm<=obsv))/(sum(is.na(perm)==FALSE))*2,1))   
  }

  fail <- function(perm){
    return(sum(is.na(perm))/length(perm)*100)  
  }
  
  if(fg$pairwise==FALSE){
    c.table <- cbind(fg$scales,fg$c.list[[1]][1]-sapply(fg$c.list,mean,na.rm = TRUE),fg$c.list[[1]][1]-sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE),fg$c.list[[1]][1]-sapply(fg$c.list,quantile,na.rm = TRUE,probs=0.975),sapply(fg$c.list,p.val,obsv=fg$c.list[[1]][1]),sapply(fg$c.list,fail))
    pvals.ordered <- c.table[order(c.table[,5]),5]
    p.border <- max(c(pvals.ordered[which(pvals.ordered<=((1:length(fg$scales))/length(fg$scales)*0.05))],0))
    c.table <- cbind(c.table,c.table[,5]<=p.border)
    colnames(c.table) <- c("cell_size","mean","lower_limit", "upper_limit", "P_value", "% failures","Reject H0")
    rownames(c.table) <- rep("",dim(c.table)[1])
  }else if(fg$correlate==FALSE & fg$pairwise==TRUE){
    m.table <- cbind(fg$scales,fg$m.list[[1]][1],sapply(fg$m.list,mean,na.rm = TRUE),sapply(fg$m.list,quantile,probs=0.025,na.rm = TRUE),sapply(fg$m.list,quantile,na.rm = TRUE,probs=0.975),sapply(fg$m.list,p.val,obsv=fg$m.list[[1]][1]),sapply(fg$m.list,fail))
    pvals.ordered <- m.table[order(m.table[,6]),6]
    p.border <- max(c(pvals.ordered[which(pvals.ordered<=((1:length(fg$scales))/length(fg$scales)*0.05))],0))
    m.table <- cbind(m.table,m.table[,6]<=p.border)  
    colnames(m.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures","Reject H0")
    rownames(m.table) <- rep("",dim(m.table)[1])
    v.table <- cbind(fg$scales,fg$v.list[[1]][1],sapply(fg$v.list,mean,na.rm = TRUE),sapply(fg$v.list,quantile,probs=0.025,na.rm = TRUE),sapply(fg$v.list,quantile,na.rm = TRUE,probs=0.975),sapply(fg$v.list,p.val,obsv=fg$v.list[[1]][1]),sapply(fg$v.list,fail))
    pvals.ordered <- v.table[order(v.table[,6]),6]
    p.border <- max(c(pvals.ordered[which(pvals.ordered<=((1:length(fg$scales))/length(fg$scales)*0.05))],0))
    v.table <- cbind(v.table,v.table[,6]<=p.border)
    colnames(v.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures","Reject H0")
    rownames(v.table) <- rep("",dim(v.table)[1])
  }else{
    c.table <- cbind(fg$scales,fg$c.list[[1]][1],sapply(fg$c.list,mean,na.rm = TRUE),sapply(fg$c.list,quantile,probs=0.025,na.rm = TRUE),sapply(fg$c.list,quantile,na.rm = TRUE,probs=0.975),sapply(fg$c.list,p.val,obsv=fg$c.list[[1]][1]),sapply(fg$c.list,fail))
    pvals.ordered <- c.table[order(c.table[,6]),6]
    p.border <- max(c(pvals.ordered[which(pvals.ordered<=((1:length(fg$scales))/length(fg$scales)*0.05))],0))
    c.table <- cbind(c.table,c.table[,6]<=p.border)
    colnames(c.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures","Reject H0")
    rownames(c.table) <- rep("",dim(c.table)[1])
  }
  
  cat("\n")
  cat("=====================================","\n")
  cat(" Floating Grid Permutation Technique","\n")
  cat("=====================================","\n")
  cat("\n")
  cat(paste("Number of scales:",length(fg$scales)),"\n")
  cat(paste("Number of iterations per scale:",fg$iter),"\n")
  if(fg$pairwise==FALSE){
    cat("\n")
    cat("---------------------------------------------------------","\n")
    cat(" Results for univariate analyses, unexplained Moran's I:","\n")
    cat("---------------------------------------------------------","\n")
    cat("\n")
    print(c.table, row.names=FALSE) 
    cat("----------------------------------------------------------------------------","\n")
    cat(" NOTE: Rejection of H0 based on Benjamini-Hochberg procedure (alpha = 0.05)","\n")
    cat("\n")
  }else if(fg$correlate==FALSE){
    cat("\n")
    cat("------------------------------------------","\n")
    cat(" Results for multivariate analyses, mean:","\n")
    cat("------------------------------------------","\n")
    cat("\n")
    print(m.table, row.names=FALSE) 
    cat("\n") 
    cat(" Results for multivariate analyses, variance:","\n")
    cat("----------------------------------------------","\n")
    cat("\n")  
    print(v.table, row.names=FALSE)
    cat("----------------------------------------------------------------------------","\n")
    cat(" NOTE: Rejection of H0 based on Benjamini-Hochberg procedure (alpha = 0.05)","\n")
    cat("\n")
  }else{
    cat("\n")
    cat("-----------------------------------","\n")
    cat(" Results for multivariate analyses,",fg$correlate,"correlation:","\n")
    cat("-----------------------------------","\n")
    cat("\n")
    print(c.table, row.names=FALSE) 
    cat("----------------------------------------------------------------------------","\n")
    cat(" NOTE: Rejection of H0 based on Benjamini-Hochberg procedure (alpha = 0.05)","\n")
    cat("\n")
    }
}
