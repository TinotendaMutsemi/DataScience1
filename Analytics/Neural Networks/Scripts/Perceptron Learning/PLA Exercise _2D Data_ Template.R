rm(list = ls())

# Read in the data:
dat = read.table('C:\\Users\\mutse\\OneDrive\\Desktop\\UCT\\DataScience1\\Analytics\\Neural Networks\\Scripts\\Perceptron Learning\\PLA_2D-1.txt',h= T)

X = as.matrix(dat[,1:2])
Y = as.matrix(dat[,3])

plot(X[,2]~X[,1], pch = c(4,16)[(Y+1)/2+1])

# Write an R-function that applies the PLA:
PLA = function(X, Y, threshold, iter = 1000)
{
  # Create a design matrix and weight vector:
  X = cbind(1,X)
  p = ncol(X)
  N = nrow(X)
  w = matrix(0,p,1)
  Yhat = sign(X%*%w)
  # Iterate through the updating rule until classified perfectly:
  count = 1
  error = c()
  error[count] = mean(abs(Yhat - Y))

  while((mean(abs(Yhat-Y)==0)<threshold)&(count < 1000))
  {
    wh_miss = which(abs(Yhat != Y) != 0)
    istar = sample(wh_miss, 1)
    w = w + Y[istar]*X[istar,]
    Yhat = sign(X%*%w)
    count = count + 1
    error[count] = mean(abs(Yhat - Y) != 0)
    
    #plot(X[,3]~X[,2], pch = c(4,16)[(Y+1)/2+1] ,main = paste0('Iteration: ',count))
    #abline(-w[1]/w[3],-w[2]/w[3])
    #Sys.sleep(0.1)
    
  }

  # Return(hopefully not rubbish):
  ret = list(w_hat = w,count = count,error = error)
  return(ret)
}

res = PLA(X,Y,0.95,100)
plot(res$error,type= 's',ylim = c(0,1))
abline(h = 0, lty = 3)


w   = res$w_hat
plot(X[,2]~X[,1], pch = c(4,16)[(Y+1)/2+1])
abline(-w[1]/w[3],-w[2]/w[3])


nruns = 1000
store_count = rep(0,nruns)
for(i in 1:nruns)
{
  res = PLA(X,Y,0.95,1000)
  store_count[i] = res$count 
}

hist(store_count, breaks = 30, col = 'grey', border = 'white')
