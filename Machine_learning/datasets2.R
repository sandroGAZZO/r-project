# Script contenant des fonctions permettant de générer des données ayant un certain type de structure

circles <- function(n)
{
  g1 <- runif(n,0,2*pi)
  g2 <- runif(n,0,2*pi)
  r1 <- runif(n,0,1)
  r2 <- runif(n,1.5,2.5)
  x1 <- c(r1*cos(g1),r2*cos(g2))
  x2 <- c(r1*sin(g1),r2*sin(g2))
  d <- cbind(x1,x2)#,group=c(rep(0,n),rep(1,n)))
  return(data.frame(d))  
}

difftaille <- function(n){
  x11 <- rnorm(n)
  x12 <- rnorm(n)
  x21 <- rnorm(n,2,0.25)
  x22 <- rnorm(n,2,0.25)
  d <- cbind(x1=c(x11,x21),x2=c(x12,x22))#,group=c(rep(1,n1),rep(2,n2)))
  return(data.frame(d))
}

carres <- function(n){
  p <- 0.6
  n1 <- floor(p*n)
  n2 <- n-n1
  n3 <- floor(n2*p)
  x11 <- c(runif(n1,0.2,2.2),runif(n2-n3,0.2,2.2),runif(n3,2.2,4))
  x12 <- c(runif(n1,0,2),runif(n2-n3,2,4),runif(n3,0,4))
  x21 <- -c(runif(n1,0.2,2.2),runif(n2-n3,0.2,2.2),runif(n3,2.2,4))
  x22 <- 4-c(runif(n1,0,2),runif(n2-n3,2,4),runif(n3,0,4))
  d <- cbind(x1=c(x11,x21),x2=c(x12,x22))#,group=c(rep(1,n),rep(2,n)))
  return(data.frame(d))  
}

# anneaux <- function(n){
#   g1 <- runif(n,0,2*pi)
#   g2 <- runif(n,0,2*pi)
#   r1 <- runif(n,2,3)
#   r2 <- runif(n,1.5,2.5)
#   d <- cbind(x1=c(r1*cos(g1),2.5+r2*cos(g2)),x2=c(rep(0,n),r2*sin(g2)),x3=c(r1*sin(g1),rep(0,n)),group=c(rep(1,n),rep(2,n)))
#   return(data.frame(d))
# }

losanges <- function(n){
  x11 <- runif(n,0,sqrt(2))
  x12 <- runif(n,0,sqrt(2))
  x21 <- sqrt(2)+runif(n,0,sqrt(2))
  x22 <- sqrt(2)+runif(n,0,sqrt(2))
  r <- matrix(c(cos(-pi/4),-sin(-pi/4),sin(-pi/4),cos(-pi/4)),nr=2,byrow = TRUE) # matrice de rotation
  x1 <- cbind(x11,x12)
  x2 <- cbind(x21,x22)
  y1 <- t(apply(x1,1,function(x){r%*%x}))
  y2 <- t(apply(x2,1,function(x){r%*%x}))
  d <- cbind(x1=c(y1[,1],y2[,1]),x2=c(y1[,2],y2[,2]))#,group=c(rep(1,n),rep(2,n)))
  return(data.frame(d))
}
