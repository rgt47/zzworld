edit.distance <- function (s1,s2) {
  t1 <- strsplit(s1,split="")[[1]]
  t2 <- strsplit(s2,split="")[[1]]
  n1 <- length(t1)
  n2 <- length(t2)
  d <- array(0,dim=c(n1+1,n2+1))
  d[,1] <- 0:n1
  d[1,] <- 0:n2
  for (i in 2:(n1+1)) {
    for (j in 2:(n2+1)) {
      d[i,j] <- min(d[i-1,j]+1, d[i,j-1]+1)
      if (t1[i-1] == t2[j-1])
        d[i,j] <- min(d[i,j], d[i-1,j-1])
      else
        d[i,j] <- min(d[i,j], d[i-1,j-1]+1)
    }
  }
  d
}