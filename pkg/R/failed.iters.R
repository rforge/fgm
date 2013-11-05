failed.iters <-
function(fgm){
  
  if(sum(is.na(fgm$perm$m)==TRUE)>0){
    hist(fgm$test.scale[is.na(fgm$perm$m)==TRUE], xlab="spatial scale", main="Number of failed simulations")
  }else{
    cat("No failed simulations detected")
    }
  }
