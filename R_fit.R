rm(list=ls())
############Function Start#################################################
fit_2D<-function(func, dat, it, op_v){
  
  plot(dat[2,], dat[1,])
  
  
  #The sum of error squared##############################
  MSE_f<-function(func){
    #try(if(class(func)!='function'){stop('argument to MSE_f(f) has to be a function')}
      #  else{
          m<-(sum((func(op_v)-dat[1,])^2))
          return(m)  
       # })
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
    q[1]<-MSE_f(func(op_v))
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
          q[2]<-MSE_f(func(op_v))
          op_v[j]<-op_v[j]-2*t
          q[3]<-MSE_f(func(op_v))
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
            q[2]<-MSE_f(func(op_v))
            op_v[j]<-op_v[j]+l(k)*t
            q[3]<-MSE_f(func(op_v))
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
            q[2]<-MSE_f(func(op_v))
            op_v[j]<-op_v[j]-l(k)*t
            q[3]<-MSE_f(func(op_v))
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
        }
        
      }
    }
  }

  #####################################code_end#########################
  return(op_v)
}
##########Function end##################################################



#########start testing##################################################

###########generate random dataset######################################
rm(list = ls())
x<-seq(-20,20, by=0.1)
y<-x^2+x+1+10*runif(length(x))
plot(x,y)
dat<-matrix( c(y,x), nrow = 2, ncol = length(x), byrow = 'TRUE')
#v<-vector(mode='numeric',length = 3)
func<-function(v){
  y<-v[1]*x*x+v[2]*x+v[3]
  return(y)
}
i<-20000
lines(x, f(c(1,1,3)), col='red')
op_v<-c(1,2,3)
fit_2D(func, dat, i, op_v)
class(func)
fit_2D(func(v), dat, i, v)
