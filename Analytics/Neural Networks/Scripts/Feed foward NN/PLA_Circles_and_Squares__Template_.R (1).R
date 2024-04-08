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
	X =                 # N x p design matrix
	p = dim(X)[2]
	w =                 # p x 1 weight matrix
  error    = 
	yhat     =  
	error[1] = mean(abs(yhat-Y)!=0)
  count = 1
  # Iterate through the updating rule until classified perfectly:
  while((mean(abs(yhat-Y)==0)<threshold)&(count<iterlim))
  {
  	wh_miss = which(abs(yhat-Y)!=0)
  	i.      = sample(wh_miss,1)
  	w       =  w + Y[i.]*X[i.,]
  	yhat    = sign(X%*%w) 
    count   = count+1
    error[count] = mean(abs(yhat-Y)!=0)
    
    if(plt)
    {
      
    }
  }
  
  print(count)
  ret = list()
}
quartz() #windows()
res = PLA(X,Y,0.9,500,T)

tab = cbind(dat$Y,res$predictions)
colnames(tab) = c('True Response', 'Prediction')
head(tab)
tail(tab)
graphics.off()
#par(mfrow = c(2,2))

#image(matrix(res$weights[-1],20,20),col = viridis_pal()(50),main = 'Weights')
#plot(res$error, type = 's')

