# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Assignment 08
# Due Date: April 6
# Brief: market model 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

# Net Profit = SalesVolume * (SellingPrice - UnitCost) - FixedCosts
FixedCosts <- 120000



N <- 1000
total <- 0
countnum <- 0
net_prpofic <- function(x1,x2,x3){
  x1*(x2-x3)-FixedCosts
}

for (i in 2:N){
  UnitCost <- rnorm(1, mean = 6.50, sd = 0.75)
  pick <- sample (3,1,replace =FALSE)
  if (pick ==1){
    x <-net_prpofic(50000,11.00,UnitCost)
  }
  if (pick ==2){
    x<-net_prpofic(75000,10.00,UnitCost)
  }
  if (pick ==3){
    x<-net_prpofic(100000,8.00,UnitCost)
  }
  total <- total +x
  countnum <- countnum + 1
}
aver_pro <- total/countnum
print(aver_pro)