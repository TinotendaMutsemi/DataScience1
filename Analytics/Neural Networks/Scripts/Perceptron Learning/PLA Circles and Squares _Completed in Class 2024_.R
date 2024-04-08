rm(list = ls())
library(ggplot2)
library(scales)
# Read in the data:
dat = read.table('CandS_images.txt',h= T)
head(dat)
tail(dat)

X = as.matrix(dat[,-1])
Y = matrix(dat[,1],ncol = 1)

# Look at some observations:
par(mfrow = c(3,3))
for(i in 1:9){
image(matrix(X[i,],20,20),col = viridis_pal()(50))
}

# Write an R-function that applies the PLA:
PLA = function(X,Y, threshold=0.9,iterlim  = 1000, plt = FALSE)
{
	# Create a design matrix and weight vector:
	X = cbind(1,X)      # N x p design matrix
	p = dim(X)[2]
	w = matrix(0,p,1)              # p x 1 weight matrix
  error    = c()
	yhat     = sign(X%*%w)
	error[1] = mean(abs(yhat-Y)!=0)
  count    = 1
  w_min    = w
  e_min    = error[count]
  # Iterate through the updating rule until classified perfectly:
  while((mean(abs(yhat-Y)==0)<threshold)&(count<iterlim))
  {
  	wh_miss = which(abs(yhat-Y)!=0)
  	i.      = sample(wh_miss,1)
  	w       =  w + Y[i.]*X[i.,]
  	yhat    = sign(X%*%w) 
    count   = count+1
    error[count] = mean(abs(yhat-Y)!=0)
    
    if(error[count]<=e_min)
    {
      w_min = w
      e_min = error[count]
    }
    if(plt)
    { 
      if((count%%100==0)|(count<10))
      {
       par(mfrow = c(2,2))
       image(matrix(X[i.,-1],20,20),col = viridis_pal()(50),main = paste('Response = ',Y[i.]))
       image(matrix(w[-1],20,20),col = viridis_pal()(50),main = paste('Iteration = ',count))
       Sys.sleep(1)
      }
    }
  }
  ret = list(error = error, w = w_min,predictions = sign(X%*%w_min), count = count)
}
quartz() #windows()
res = PLA(X,Y,0.9,25000,T)
#par(mfrow = c(1,1))
#plot(res$error,type = 's')
#res$count
tab = cbind(dat$Y,res$predictions)
colnames(tab) = c('True Response', 'Prediction')
head(tab)
tail(tab)
#graphics.off()
#par(mfrow = c(2,2))

#image(matrix(res$weights[-1],20,20),col = viridis_pal()(50),main = 'Weights')
#plot(res$error, type = 's')

