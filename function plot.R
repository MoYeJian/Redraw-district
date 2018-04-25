M <- 2
K <- 1
V0 <- 0
X0 <- 2
ITERS <- 100
TMAX <- 10

#dv
a<- function(x,dt){
  -K*x/M * dt
}

t <- seq(0,TMAX, length = ITERS)
v <- rep(0,ITERS)
x <- rep(0,ITERS)

v[1] <- V0
x[1] <- X0

for (i in 2: ITERS){
  dt <- t[i]- t[i-1]
  v[i] <- v[i-1]+a(x[i-1], dt)
  x[i] <- x[i-1] + v[i] * dt
}

#plot(t,x)
#points(t,v,col="red")

cc

accumulate_t = c(t[1])
accumulate_x = c(x[1])
frames = c(t[1])

for (frame in 2: ITERS){
  accumulate_t = c(accumulate_t, t[1:frame])
  accumulate_x = c(accumulate_x, x[1:frame])
  frames = c(frames,rep(frame, frame) )
}




p <- plot_ly(x=accumulate_t,y=accumulate_x, frame = frames, type="scattergl", mode = "markers+lines")

p <- animation_opts(p, 1, redraw = FALSE)
print(p)
