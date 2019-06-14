rm(list=ls())
library(DataAnalyze)

#generating the data-set######################
x<-seq(from = -10, to=10, by=0.05)
#rm(list = 'x')
#x<-seq(-1,-0.5, 0.002)
#class(x)
ran<-0.1*runif(min=-1,max=1, length(x))
y<-3*exp(-1*(((x-4)/2)^2))+6*exp(-1*(((x-3)/1)^2))
y1<-y+ran
#plot(x,y, type = 'l', col='red' )
plot(x,y1, col='blue')
#############################################

############################################
#Start writing the minimization algorithm

#Here is the vector to be minimized#######################
v<-vector(mode='numeric', length = 6)
v[1]<-8
v[2]<-4 
v[3]<-2
v[4]<-3
v[5]<-6
v[6]<- 3
##########################################################

#The function to be fit##################################
f<-function(v){
  y <-v[1]*exp(-1*(((x-v[2])/v[3])^2))+v[4]*exp(-1*(((x-v[5])/v[6])^2))
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
   # l<-(q[2]-q[3])/(length(x))
    l<-1
  }
  else{
    l<-1
  }
  return(l)
}
######################################################

d<-'TRUE'     #bit set to false whenever solution does not converge within the given number of iterations


for(k in 1:10){
  if(d=='FALSE'){
    print('solution did not converge')
    break
  }
q<-vector(mode='numeric', length=4)
t<-0.02
va<-0
q[1]<-MSE_f(f(v))
#l<-1
it<-20000
####The optimization loop#######################################################
for(j in 1:length(v)){
  if(d=='FALSE'){
    print('solution did not converge')
    break
  }
for (i in 1:it) {
  if(i==it){
    print('Solution did not converge')
    d<-'FALSE'
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
if(va==0 & d=='TRUE'){
  lines(x,v[1]*exp(-1*(((x-v[2])/v[3])^2)), col='green')
  lines(x,v[4]*exp(-1*(((x-v[5])/v[6])^2)), col='cyan')
}
#####################################code_end#########################


