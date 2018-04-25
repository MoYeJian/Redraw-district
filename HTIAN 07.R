# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Assignment 07
# Due Date: Mar 16
# Brief: 3D graphics 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy


m <- as.matrix(read.csv('points.csv', header = TRUE, row.names = 1))
if (!require(plotly)){
  install.packages("plotly")
  require(plotly)
}

p <- plot_ly (x = m[1,] / m[4,], y = m[2,]/ m[4,], z = m[3,]/m[4], type = "scatter3d", mode="markers+lines")

#functions 
translate <- function(x,y,z){
  Tr <- diag( rep(1,4))
  # Tr1 <-matrix( ncaol = 4, nrow = 4)
  Tr [,4]<- c(x,y,z,1)
  Tr
}

scale <- function(x,y,z){
  Tr <- diag( rep(1,4))
  # Tr1 <-matrix( ncaol = 4, nrow = 4)
  Tr[,1] <- c(x,0,0,0)
  Tr[,2] <- c(0,y,0,0)
  Tr[,3] <- c(0,0,z,0)
  Tr [,4]<- c(0,0,0,1)
  Tr
}
project <- function(l, r, b, t, n, f){
  Tr <- diag( rep(1,4))
  # Tr1 <-matrix( ncaol = 4, nrow = 4)
  Tr[1,] <- c((2*n)/(r-l),	0,	(r+l)/(r-l),	0)
  Tr[2,] <- c(0,	(2*n)/(t-b),	(t+b)/(t-b),	0)
  Tr[3,] <- c(0,	0,	-(f+n)/(f-n),	(-2*f*n)/(f-n))
  Tr [4,]<- c(0,0,-1,0)
  Tr
}





S <- scale(2, 2, 2)
m1<-S %*% m

M <-translate (3,3,-1)


T1 <- translate(0, 0, -4)
s <- scale (1.5, 1.5, 1.5)
T2 <- translate(0, 0, 4)
m2 <- T1%*%s%*%T2%*%m

S <- scale(0.5,0.5,2)#
m3 <- s%*% m
allm <- cbind(m, m1,m2,m3) # append m1 to the m matrix
labels <- c(rep('original', ncol(m)), rep('scaled', ncol(m)))
allm_plot <- plot_ly (x = allm[1,] /allm[4,], y = allm[2,]/ allm[4,], z = allm[3,]/allm[4], type = "scatter3d", mode="markers+lines")

print(allm_plot)