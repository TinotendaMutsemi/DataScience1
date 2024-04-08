rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed(2019)
N = 50
x = runif(N,-1,1)
e = rnorm(N,0,sqrt(0.5))
y = 2*x+2*sin(3/2*pi*x)+e

plot(y~x,pch = 16, col ='blue')
xx = seq(-1,1,1/100)
yy = 2*xx+2*sin(3/2*pi*xx)
lines(yy~xx,col ='grey80')

# Get the data in matrix form:
X = matrix(x,N,1)
Y = matrix(y,N,1)


# Specify activation functions for the hidden and output layers:
sig1 = function(z)
{
	
}

sig2 = function(z)
{
   
}
# Error contribution C_i
g = function(AL,Y)
{
     
}

# Write a function that evaluates the neural network (forward recursion):
# X     - Input matrix (N x p)
# Y     - Output matrix(N x q)
# theta - A parameter vector (all of the parameters)
# m     - Number of nodes on hidden layer
# nu   - Regularisation parameter (see later)
neural_net = function(X,Y,theta,m,nu)
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

   db1 = 
   db2 = 
   dW1 = 
   dW2 = 
   
   for(i in 1:N)
   {
        a0 = matrix(X[i,],p,1)
        z1 = 
        a1 = 
        z2 = 
        a2 = 
   }
   
   
   # Calculate error:
   E1 = 
   E2 = E1+nu*(sum(abs(W1))+sum(abs(W2)))
   
   # Return predictions and error:
   return(list(out = out,dtheta = , E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:
p = 1
q = 1
m = 10
npars = p*m+m*q+m+q
theta_rand = runif(npars,-1,1)
lambda = 1

res_fitted= neural_net(X,Y,theta_rand,m,0)
res_fitted

# Gradient testing:




