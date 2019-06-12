library(DataAnalyze)
x<-seq(from = -1000, to=1000, by=0.2)
rm(list = 'x')
x<-seq(-1,-0.5, 0.002)
class(x)
ran<-40*runif(min=-10,max=10, length(x))
y<-2*exp(-3*x)+6*exp(-7.3*x)+4*exp(-1.7*x)
y1<-y+ran
plot(x,y, type = 'l', col='red' )
points(x,y1, col='blue')
#############################################

############################################
#Start writing the minimization algorithm
v<-vector(mode='numeric', length = 6)
v[1]<-10 
v[2]<-5 
v[3]<-6
v[4]<-2
v[5]<-9
v[6]<-3
f<-function(v){
  y_<-v[1]*exp(-v[4]*x)+v[2]*exp(-v[5]*x)+v[3]*exp(-v[6]*x)
  return(y)
}
MSE_f<-function(f){
  m<-sum((f-y1)^2)
  return(m)
}
q<-vector(mode='numeric', length=2000)
t<-0.2
va<-0
q[1]<-MSE_f(f(v))
for (i in 1:length(q)) {
  if(i==1) {
    v[1]<-v[1]+t
    q[2]<-MSE_f(f(v))
    v[1]<-v[1]-2*t
    q[3]<-MSE_f(f(v))
    v[1]<-v[1]+2*t
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
      lines(x,yres)
      break
    }
    else if(va==1){
      q[2]<-MSE_f(f(v))
      v[1]<-v[1]+t
      q[3]<-MSE_f(f(v))
      if(q[3]>q[2]){
        va<-0
        print('Local minima reached')
        yres<-f(v)
        lines(x,yres)
        break
      }
      else{
        next
      }
    }
    else{
      q[2]<-MSE_f(f(v))
      v[1]<-v[1]-t
      q[3]<-MSE_f(f(v))
      if(q[3]>q[2]){
        va<-0
        print('Local minima reached')
        yres<-f(v)
        lines(x,yres)
        break
      }
      else{
        next
      }
    }  
  }
  
}
