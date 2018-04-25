# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Assignment 06
# Due Date: Feb 23
# Brief: limit definition of the derivative 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

x <- 1  
h = seq(0.000001,1, len=100)

f <- function (n){
2*n^2 +1                                           
}
fprime <- function(n){
  4*n
}

deriv = function(x,h){
  (f(x+h) - f(x))/h
}
err = function (h){
  abs(deriv(1,h)-fprime (1))
}
plot(h, err(h),type = "l")     #my plot is linear, which is what we expect; it agrees with what we talk during the class.