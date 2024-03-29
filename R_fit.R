rm(list=ls())
install.packages('devtools')
library(devtools)
install_github('Chitran1987/DataAnalyze1.0')
library(DataAnalyze1.0)
############Function Start#################################################
fit_2D<-function(func, dat, it, op_v){
  
  plot(dat[2,], dat[1,])
  
  
  #The sum of error squared##############################
 # MSE_f<-function(func){
    #try(if(class(func)!='function'){stop('argument to MSE_f(f) has to be a function')}
      #  else{
  #        m<-(sum((func(op_v)-dat[1,])^2))
   #       return(m)  
       # })
  #}
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
  
  
  for(k in 1:100){
    if(d=='FALSE'){
      print('solution did not converge')
      break
    }
    q<-vector(mode='numeric', length=4)
    t<-0.002
    va<-0
    q[1]<-MSE_f(func, op_v, dat)
    #l<-1
    #it<-20000
    ####The optimization loop#######################################################
    for(j in 1:length(op_v)){
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
          op_v[j]<-op_v[j]+t
          q[2]<-MSE_f(func,op_v, dat)
          op_v[j]<-op_v[j]-2*t
          q[3]<-MSE_f(func, op_v, dat)
          op_v[j]<-op_v[j]+2*t
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
            yres<-func(op_v)
            if(j==length(op_v) & k==5){
              lines(x,yres, col='red')
            }
            break
          }
          else if(va==1){
            q[2]<-MSE_f(func, op_v, dat)
            op_v[j]<-op_v[j]+l(k)*t
            q[3]<-MSE_f(func, op_v, dat)
            if(q[3]>q[2]){
              va<-0
              print('Local minima reached')
              yres<-func(op_v)
              if(j==length(op_v) & k==5){
                lines(x,yres, col='red')
              }
              break
            }
            else{
              next
            }
          }
          else{
            q[2]<-MSE_f(func, op_v, dat)
            op_v[j]<-op_v[j]-l(k)*t
            q[3]<-MSE_f(func, op_v, dat)
            if(q[3]>q[2]){
              va<-0
              print('Local minima reached')
              yres<-func(op_v)
              #if(j==length(op_v) & k==100){
               # lines(x,yres, col='red')
              #}
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
  if(d==TRUE){
  return(op_v)
    lines(x,yres, col='red')
  }
  else{
    return(NULL)
  }
  }
##########Function end##################################################



#########start testing##################################################

###########generate random dataset######################################
rm(list = ls())
x<-seq(-3,3, by=0.01)
y<- 3*exp(-1*(((x-0.5)/(1.5))^2))+1.5*exp(-1*(((x+0.27)/(2))^2))+2*exp(-1*(((x-1.75)/(1))^2))
plot(x,y)
dat<-matrix( c(y,x), nrow = 2, ncol = length(x), byrow = 'TRUE')
#v<-vector(mode='numeric',length = 3)
func<-function(v){
  y<-v[1]*exp(-1*(((x-v[2])/(v[3]))^2))+v[4]*exp(-1*(((x-v[5])/(v[6]))^2))+v[7]*exp(-1*(((x-v[8])/(v[9]))^2))
  return(y)
}
i<-200000
lines(x, func(c(3.484,0.612,1.502,1.852,1.786,0.976,1.386,-0.600,1.750)), col='red')
op_v<-c(1000,0.480,1.426, 2.316,1.744,1.034,1.522,-0.223,1.844)
fit_2D(func, dat, i, op_v)
class(func)
fit_2D(func(v), dat, i, v)
clp<-function(){
  dev.off()
  return(NULL)
}
lines(x, func(op_v), col='red')
