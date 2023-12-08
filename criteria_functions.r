cyc <- function(x){
  n <- length(x)
  A <- diag(n)
  A[1,] <- x
  for(i in 2:n) A[i,] <- c(A[i-1,-1],A[i-1,1])
  A
}

offdiag.max.cyc <- function(x){
  X <- cyc(x)
  n <- length(x)
  max(abs(t(X[1,]) %*% t(X[2:n,])))
}


d2 <- function(D){
  min(dist(D))^2
}

offdiag.max.cyc <- function(x){
  X <- cyc(x)
  n <- length(x)
  max(abs(t(X[1,]) %*% t(X[2:n,])))
}
