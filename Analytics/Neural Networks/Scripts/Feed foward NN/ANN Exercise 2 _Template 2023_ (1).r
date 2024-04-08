rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed(2020)
N = 50
x = runif(N,-1,1)
e = rnorm(N,0,1)
y = 2*sin(3*pi*x)+e

plot(y~x,pch = 16, col ='blue')
xx = seq(-1,1,1/100)
yy = 2*sin(3*pi*xx)
lines(yy~xx,col ='grey80')

# Get the data in matrix form:
X = matrix(x,N,1)
Y = matrix(y,N,1)


# Specify activation functions for the hidden and output layers:
sig1 = function(z)
{
   1/(1+exp(-z))
}
sig2 = function(z)
{
   z
}

# Write a function that evaluates the neural network (forward recursion):
# X     - Input matrix (N x p)
# Y     - Output matrix(N x q)
# theta - A parameter vector (all of the parameters)
# m     - Number of nodes on hidden layer
# lam   - Regularisation parameter (see later)
neural_net = function(X,Y,theta,m,lam)
{
	 # Relevant dimensional variables:
   N = dim(X)[1]
   p = dim(X)[2]
   q = dim(Y)[2]
   
   # Populate weight-matrix and bias vectors:
   index = 1:(p*m)
   W1    = matrix(theta[index],p,m)
   index = max(index)+1:(m*q)
   W2    = matrix(theta[index],m,q)
   index = max(index)+1:(m)
   b1    = matrix(theta[index],m,1)
   index = max(index)+1:(q)
   b2    = matrix(theta[index],q,1)
   
   # Evaluate network:
   out   = rep(0,N)
   error = rep(0,N)
   for(i in 1:N)
   {
       a0 = matrix(X[i,],p,1)
       a1 = sig1(t(W1)%*%a0+b1)
       a2 = sig2(t(W2)%*%a1+b2)
       out[i] = a2
   }

   # Calculate error:
   E1 = sum((out-Y)^2)/(1*N)
   E2 = 0
   
   # Return predictions and error:
   return(list(out = out, E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:
p = 1
q = 1
m = 10
npars = p*m+m*q+m+q
theta_rand = runif(npars,-1,1)

obj = function(pars)
{
  res = neural_net(X,Y,pars,m,0)
  return(res$E1)
}
obj(theta_rand)

res_opt = nlm(obj,theta_rand,iterlim = 500)
res_opt

res = neural_net(X,Y,res_opt$estimate,m,0)
res
points(res$out~x, pch = 16, col ='red')

xx = seq(-1,1,1/100)
XX = cbind(xx)
YY = matrix(0,length(xx),1)
res_resp= neural_net(XX,YY,res_opt$estimate,m,0)
lines(res_resp$out~xx, col = 'red')
