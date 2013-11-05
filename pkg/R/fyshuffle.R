fyshuffle <-
function(seq){
  seq <- unlist(seq)
  n <- length(seq)
  i <- n 
  while (i>0){
    j <- floor(runif(1) * i+1)
    if (i != j){
      tmp <- seq[i]
      seq[i] <- seq[j]
      seq[j] <- tmp
    }
    i <- i - 1
  }
  return(seq)
  }
