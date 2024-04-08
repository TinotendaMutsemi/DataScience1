rm(list = ls())

# Let's fake a dataset and see if the network evaluates:
set.seed()
plot(y~x)

# Get the data in matrix form:
X = 
Y = 


# Specify activation functions for the hidden and output layers:
sig1 = function(x)
{

}
sig2 = function(x)
{

}

# Write a function that evaluates the neural network (forward recursion):
# X     - Input matrix (N x p)
# Y     - Output matrix(N x q)
# theta - A parameter vector (all of the parameters)
# m     - Number of nodes on hidden layer
# lam   - Regularisation parameter (see later)
neural_net = function()
{
	 # Relevant dimensional variables:
   N = 
   p = 
   q = 
   
   # Populate weight-matrix and bias vectors:
   ...
   W1    = ...
   ...
   W2    = ...
   ...
   b1    = ...
   ...
   b2    = ...
   
   # Evaluate network:
   out   = rep(0,N)
   error = rep(0,N)
   for(i in 1:N)
   {
   ...
   ...
   ...
   ...
   ...
   }

   # Calculate error:
   ...
   E1 = ...
   E2 = ...
   
   # Return predictions and error:
   return(list(out = out, E1 = E1, E2 = E2))
}

# We need to know the number of parameters in the network:
npars = ...
theta_rand = ...
res = neural_net(X,Y,theta_rand,m,0)

plot(y~x,pch = 16,col = 'blue')
points(res$out~x,pch = 16, col = 'red')
legend('topright',c('Y','Predictions'), pch = 16, col = c('blue','red'))


