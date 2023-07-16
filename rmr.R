pseudo<-function(k){#псевдовипадкові числа
  n<-200 # кiлькiсть чисел
  a<-65539 # RANDU параметри
  #c0<-0 #
  c0<-runif(n=1,min=0,max=1000000)
  m<-2^31 #
  I<-numeric(n) # цiлочислова послiдовнiсть
  I[1]<-2^15+2
  for(i in 2:n){
    I[i]<-(a*I[i-1]+c0)%% m
  }
  k<-I/m # псевдовипадковi числа
}


RepMedian<-function(y,x)#метод повторних медіан
{
  yy<-rep(y,length(y))-rep(y,each=length(y))
  xx<-rep(x,length(x))-rep(x,each=length(x))
  bb<-yy/xx
  bb<-matrix(bb,nrow=length(x))
  b1m<-median(apply(bb,1,median,na.rm=T))
  b0m<-median(y-b1m*x)
  c(b0m,b1m)
}



cycles <- 50

n<- 50
b0<-1
b1<-0.5
sigma <- 1
x<-rnorm(n)
y<-b0+b1*x+sigma*rnorm(n)

intercepts_res <- array(dim = c(0))
x_res <- array(dim = c(0))
for (i in 1:cycles){
  x<-rnorm(n)
  y<-b0+b1*x+sigma*rnorm(n)
  x[floor(runif(1, min=1, max=n))]=floor(runif(1, min=0, max=50))
  y[floor(runif(1, min=1, max=n))]=floor(runif(1, min=0, max=50))
  lsc<-lm(y~x)
  rm<-RepMedian(y,x)
  intercept_lsc <- coef(lsc)["(Intercept)"]
  x_coeff_lsc <- coef(lsc)["x"]
  intercept_rm <- rm[1]
  x_coeff_rm <- rm[2]
  
  plot(x,y)
  abline(c(b0,b1),col="black",lty="solid",lwd=4)
  abline(lsc,col="blue",lty="dashed",lwd=3)
  abline(rm,col="red",lty="dashed",lwd=3)
  legend(x="topright",inset = 0.025,legend=c("Real","RMM","LSM"),
         col=c("black","red","blue"),lty = c(1, 2,2))
  
  cat("\n")
  print(i)
  cat("\n")
  
  cat("\n")
  print("LSR")
  cat("\n")
  intersept_lsc_res <- abs(intercept_lsc-b0)
  x_lsc_res<- abs(x_coeff_lsc-b1)
  print(intercept_lsc)
  print(x_coeff_lsc)
  
  cat("\n")
  print("RMR")
  cat("\n")
  intersept_rm_res <- abs(intercept_rm-b0)
  x_rm_res<- abs(x_coeff_rm-b1)
  print(intercept_rm)
  print(x_coeff_rm)
  
  intercepts_res <-append(intercepts_res,intersept_rm_res<intersept_lsc_res)
  x_res <-append(x_res,x_rm_res<x_lsc_res)
}

sum(intercepts_res)
sum(x_res)


