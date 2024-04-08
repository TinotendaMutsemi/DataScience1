rm(list = ls())

# Read in the data:
dat = read.table('PLA_1D.txt',h= T)


# Write an R-function that applies the PLA:
PLA = function(X1,Y)
{
  # Create a design matrix and weight vector:
  X = cbind(1, X)
  p =  dim(X)[2]
  N = dim(X)[1]
  w = matrix(0, p, 1)
  Yhat = sign(X%*%w)
  # Iterate through the updating rule until classified perfectly:
  count = 1
  error = c()
  error[count] = mean(abs(Yhat - Y) != 0)
  
  #try setting a threshold for the while loop at say 0.95
  #plot the line of fit with sys.sleep 
  #use the pockect algorithim to store the best w
  while((any(abs()))&(count < iter))
  {
    wh_miss = which(Yhat != Y)
    istar = sample(wh_miss, 1)
    w = w + Y[istar]*X[istar,]
    Yhat = sign(X%*%w)
    count = count + 1
    error[count] = sum(Yhat != Y)
    
  }
  
  # Return(hopefully not rubbish):
  ret = list(count = count, w = w, error = error)

}

res = PLA(X, Y, 200)
plot(res$error, type = 's')
abline (h = 0, col = 'red')



w = res$w_hat
plot(X, Y, col = ifelse(Y == 1, 'red', 'blue'), pch = 19)
abline(-w[2]/w[3], -w[1]/w[3], col = 'green')



# Run the PLA 100 times and plot the histogram of the number of iterations to convergence:

nruns = 100
score_count = rep(0, nruns)
for (i in 1:nruns)
{
  res = PLA(X, Y, 200)
  score_count[i] = res$count
}

hist(score_count, breaks = 20, col = 'lightblue', xlab = 'Iterations to convergence')

