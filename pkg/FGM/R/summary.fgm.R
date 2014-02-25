summary.fgm <-
function(fgm){
  
  if (class(fgm)!="fgm") {stop("Object needs to be of class \"fgm\"")}
  
  p.val <- function(perm,obsv){
    return(min((sum(perm>=obsv))/(sum(is.na(perm)==FALSE))*2,(sum(perm<=obsv))/(sum(is.na(perm)==FALSE))*2,1))   
  }

  fail <- function(perm){
    return(sum(is.na(perm))/length(perm)*100)  
  }
  
  if(fgm$correlate==FALSE){
    m.table <- cbind(fgm$scales,fgm$m.list[[1]][1],sapply(fgm$m.list,mean,na.rm = TRUE),sapply(fgm$m.list,quantile,probs=0.025,na.rm = TRUE),sapply(fgm$m.list,quantile,na.rm = TRUE,probs=0.975),sapply(fgm$m.list,p.val,obsv=fgm$m.list[[1]][1]),sapply(fgm$m.list,fail))
    colnames(m.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures")
    rownames(m.table) <- rep("",dim(m.table)[1])
    v.table <- cbind(fgm$scales,fgm$v.list[[1]][1],sapply(fgm$v.list,mean,na.rm = TRUE),sapply(fgm$v.list,quantile,probs=0.025,na.rm = TRUE),sapply(fgm$v.list,quantile,na.rm = TRUE,probs=0.975),sapply(fgm$v.list,p.val,obsv=fgm$v.list[[1]][1]),sapply(fgm$v.list,fail))
    colnames(v.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures")
    rownames(v.table) <- rep("",dim(v.table)[1])
  }else{
    c.table <- cbind(fgm$scales,fgm$c.list[[1]][1],sapply(fgm$c.list,mean,na.rm = TRUE),sapply(fgm$c.list,quantile,probs=0.025,na.rm = TRUE),sapply(fgm$c.list,quantile,na.rm = TRUE,probs=0.975),sapply(fgm$c.list,p.val,obsv=fgm$c.list[[1]][1]),sapply(fgm$c.list,fail))
    colnames(c.table) <- c("cell_size","observed","mean","lower_limit", "upper_limit", "P_value", "% failures")
    rownames(c.table) <- rep("",dim(c.table)[1])
  }
  
  cat("\n")
  cat("======================","\n")
  cat(" Floating Grid Method","\n")
  cat("======================","\n")
  cat("\n")
  cat(paste("Number of scales:",length(fgm$scales)),"\n")
  cat(paste("Number of iterations per scale:",fgm$iter),"\n")
  if(fgm$correlate==FALSE){
  cat("\n")
  cat("-------------------","\n")
  cat(" Results for mean:","\n")
  cat("-------------------","\n")
  cat("\n")
  print(m.table, row.names=FALSE) 
  cat("\n") 
  cat(" Results for variance:","\n")
  cat("-----------------------","\n")
  cat("\n")  
  print(v.table, row.names=FALSE)
  cat("\n") 
  }else{
    cat("\n")
    cat("--------------------------","\n")
    cat(" Results for",fgm$correlate,"correlation:","\n")
    cat("--------------------------","\n")
    cat("\n")
    print(c.table, row.names=FALSE) 
    cat("\n") 
  }
}
