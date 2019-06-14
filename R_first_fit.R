rm(list=ls())
library(DataAnalyze)

#generating the data-set######################
x<-seq(from = -1000, to=1000, by=0.2)
rm(list = 'x')
x<-seq(-1,-0.5, 0.002)
class(x)
ran<-40*runif(min=-10,max=10, length(x))
y<-3*exp(-(((x-4)/2)^2))+6*exp(-(((x-3)/1)^2))
y1<-y+ran
plot(x,y, type = 'l', col='red' )
points(x,y1, col='blue')
#############################################

############################################
#Start writing the minimization algorithm

#Here is the vector to be minimized#######################
v<-vector(mode='numeric', length = 6)
v[1]<-12 
v[2]<-9 
v[3]<-2
v[4]<-6
v[5]<-7
v[6]<- 5
##########################################################

#The function to be fit##################################
f<-function(v){
  y <-v[1]*exp(-v[4]*x)+v[2]*exp(-v[5]*x)+v[3]*exp(-v[6]*x)
  return(y)
}
########################################################

#The sum of error squared##############################
MSE_f<-function(f){
  m<-(sum((f(v)-y1)^2))
  return(m)
}
######################################################

#Define the proportionality function##################
l<-function(k){
  if(k==1){
    l<-(q[2]-q[3])/(length(x))
  }
  else{
    l<-1
  }
  return(l)
}

for(k in 1:10){
q<-vector(mode='numeric', length=4)
t<-0.02
va<-0
q[1]<-MSE_f(f(v))
#l<-1
it<-2000000
####The optimization loop#######################################################
for(j in 1:length(v)){
for (i in 1:it) {
  if(i==it){
    print('Solution did not converge')
    break
  }
  if(i==1) {
    v[j]<-v[j]+t
    q[2]<-MSE_f(f(v))
    v[j]<-v[j]-2*t
    q[3]<-MSE_f(f(v))
    v[j]<-v[j]+2*t
    if(q[1]>q[2] & q[1]>q[3]){
      if(abs(q[1]-q[3])>abs(q[1]-q[2])){
        va<- -1
      }
      else{va<-1
      }
    }
    else if(q[2]>q[1] & q[3]<q[1]){
      va<- -1
    }
    else if(q[2]<q[1] & q[3]>q[1]){
      va<- 1
    }
    else if(q[2]>q[1] & q[3]>q[1]){
      va<-0
    }
    else{
      va<-0
    }
  }
  else{
    if(va==0){
      print('Local minima reached')
      yres<-f(v)
      if(j==6 & k==5){
      lines(x,yres)
      }
      break
    }
    else if(va==1){
      q[2]<-MSE_f(f(v))
      v[j]<-v[j]+l(k)*t
      q[3]<-MSE_f(f(v))
      if(q[3]>q[2]){
        va<-0
        print('Local minima reached')
        yres<-f(v)
        if(j==6 & k==5){
        lines(x,yres)
        }
        break
      }
      else{
        next
      }
    }
    else{
      q[2]<-MSE_f(f(v))
      v[j]<-v[j]-l(k)*t
      q[3]<-MSE_f(f(v))
      if(q[3]>q[2]){
        va<-0
        print('Local minima reached')
        yres<-f(v)
        if(j==6 & k==5){
        lines(x,yres)
        }
        break
      }
      else{
        next
      }
    }  
  }
  
}
}
}
#####################################code_end#########################



#####################################code_test_begins#################
tt<-seq(-3000,3000, by=2)
M<-vector(mode='numeric', length = length(tt))
for(i in 1:length(tt)){
  v[1]<-tt[i]
  M[i]<-MSE_f(f(v))
}
plot(tt,M, type = "l")
lines(x, f(v))
