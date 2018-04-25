# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Project 1
# Due Date: Mar 16
# Brief: plot 3-D 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy


X0 <- 1
Y0 <- 1
Z0 <- 1

SIGMA <- 10
RHO <- 28
BETA <- 8.0/3
ITERS <- 10000
TMAX <- 100 

if (!require(plotly)){
  install.packages('plotly')
  require(plotly)
}

t <- seq(0,TMAX, length = ITERS)
x <- rep(0,ITERS)
y <- rep(0,ITERS)
z <- rep(0,ITERS)

dx <- function(x, y, z, dt) {
  SIGMA * (y-x) * dt
}
dy <- function(x, y, z, dt) {
  (x * (RHO - z) - y) * dt
}
dz <- function(x, y, z, dt){
  (x * y - BETA * z)* dt
}
z[1] <- Z0
y[1] <- Y0
x[1] <- X0

for (i in 2:ITERS){
  dt <- t[i]- t[i-1]
  x[i] <- x[i-1]+ dx(x[i-1], y[i-1], z[i-1], dt)
  y[i] <- y[i-1]+ dy(x[i-1], y[i-1], z[i-1], dt)
  z[i] <- z[i-1]+ dz(x[i-1], y[i-1], z[i-1], dt)
}

p <- plot_ly(x=x,y=y,z=z,type='scatter3d', mode='lines')
print(p)
