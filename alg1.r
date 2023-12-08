library(NMOF)

source('criteria_functions.R')

Mm.Utype.mirror.cyc <- function(m, init = NULL, K=2000, J=1000, I=100){
  
  x0 <- init
  
  neighbour <- function(x1){
    
    return(sample(x1,m)*sample(c(-1,1),m,replace=TRUE))
    
  }
  
  
  ## *Threshold Accepting*
  
  algo <- list()
  
  algo$nD <- K
  
  algo$nT <- I
  
  algo$nS <- J
  
  algo$x0 <- x0
  
  algo$neighbour <- neighbour
  
  OF<-offdiag.max.cyc
  
  sol <- TAopt(OF, algo = algo)
  
  bestsol <- sol
  
  xf1=bestsol$xbest
  
  best.criterion = bestsol$OFvalue
  
  return(list(x=xf1, criterion=best.criterion))
  
}

# generated design for s = 5, m = 15 for example

s <- 5

m <- 15

xx <- sample(rep((-(s-1)/2):((s-1)/2),m/s))

res <- Mm.Utype.mirror.cyc(m=m, init = xx)

xxopt = res$x

Xopt = cyc(xxopt)

Dopt <- rbind(Xopt,-Xopt)

Dopt

d2(Dopt)
