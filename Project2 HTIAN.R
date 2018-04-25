# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Project 02
# Due Date: Apr 6
# Brief:  Random Walks and Robotic Vacuum  
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy
Battery <- 500

xs<- rep(0, Battery)
ys <- rep(0,Battery)

N<- 10
x0 <- N/2
y0 <- N/2
xs[1] <- x0
ys[1] <- y0


Room <-matrix(0, ncol =N , nrow =N )
Room[x0, y0] <- 1
count<- 0

for (i in 2: Battery){
  xs[i] <- xs[i-1]
  ys[i]<- ys[i-1]
  Random <- sample(1:4,1)
  if (Random == 1){
    xs[i]<- xs[i-1] +1
  }
  if (Random == 2){
    xs[i]<- xs[i-1] -1
  }
  if (Random == 3){
    ys[i]<- ys[i-1] +1
  }
  if (Random == 4){
    ys[i]<- ys[i-1] -1
  }
  if (xs[i]> N ){
    xs[i] <- N
  }
  if (ys[i] > N){
    ys[i] <- N
  }
  if (xs[i] < 0 ){
    xs[i] <- 0
  }
  if (ys[i] <0){
    ys[i] <- 0
  }
  Room[xs[i],ys[i]] <- 1
}
  if (!require(plotly)){
    install.packages("plotly")
    require(plotly)
  }
total <- sum(Room)
area <- N*N
percentage <- total / area

print(total)
print("Size of Battery (Iterations):")
print(Battery)
print( "width of room")
print(N)
print("height of room")
print(N)
print("Percentage of room covered:")
print(percentage)


aggre_x <- function(x){
  frame0 <- x[1]
  for (i in 2: length(x) ){
    frame0 <- c(frame0, x[1:i]) 
  }
}

comba_x <- aggre_x(xs)
comba_y <- aggre_x(ys)

frames <- function(n){
  a<- 1
  for (i in 2:N){
    a <- c(a,rep(i,i))
  }
}
p <- plot_ly(x= comba_x, y= comba_y, type = "scattergl", mode = "lines", frame = frames(Battery))
p <- layout(p, x_axis <- list(range<- c(1,N)), y_axis <- list(range (c(1,N))))
p <- animation_opts(p,10,redraw = FALSE)
print(p)