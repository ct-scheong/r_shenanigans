#############################
# Ka Ieng Cheong
# February 24th 2015
#############################

####################################################################################
#Define the function "fakedata" as given in assignment

#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){

if(! require(MASS))
{
	install.packages("MASS")
}
if(! require(mvtnorm))
{
	install.packages("mvtnorm")
}

require(MASS)
require(mvtnorm)

# obtain dimension
d <- length(w)-1

# compute the offset vector and a Basis consisting of w and its nullspace
offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
Basis <- cbind(Null(w[1:d]), w[1:d])	 

# Create samples, correct for offset, and extend
# rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
S <- S + matrix(rep(offset,n),n,d,byrow=T)
S <- cbind(S,1)

# compute the class assignments
y <- as.vector(sign(S %*% w))

# add corrective factors to points that lie on the hyperplane.
S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
y = as.vector(sign(S %*% w))
return(list(S=S, y=y))

} # end function fakedata

####################################################################################
#Define the function "classify"

#Inputs
#S: sample data set
#z: z = (v,-c) is the vector defining the hyperplane

#Outputs
#y: vector of the associated class labels

classify <- function(S, z){
  y = sign(S %*% z)
  return (y)
  }
#end function classify

#####################################################################################
#Define the function "perceptrain"

#Inputs
#S: sample data set
#y: vector of the associated class labels

#Outputs
#z: normal vector z_h of the hyperplane
#Z.history: matrix containing history of z_i's

perceptrain <- function(S,y){
z = rep(0,ncol(S)) #initial weights
Z.history = vector() #matrix of z history
# start iteration at 1
iteration = 1
# initial while loop is switched on
misclassify = TRUE

while (misclassify){
# runs the initial classification function
y.hat = classify(S,z)

# resets the counters
grad.cost = 0
mistake.count = 0

# define the gradient of the cost function
for (i in 1:nrow(S))
  { 
  if (y.hat[i] != y[i])
    {
  grad.cost = grad.cost + (-y[i] * S[i,])
  #counts the number of mistakes
  mistake.count = mistake.count + 1
    }
  }

# define alpha and the new z
alpha = 1/iteration
z = z - alpha * grad.cost

# adds to iteration and to Z.history
iteration = iteration + 1
Z.history = rbind(Z.history,z)

# flips off the while loop if we reach a cost function of 0
if (mistake.count == 0){
  misclassify = FALSE
}
}
# returns the list
return(list(z=z, Z.history=Z.history))
}

####################################################################################

# generate 3D random vector z between 0 and 1
z.rand = as.vector(runif(3,0,1))

# generate fake dataset "data"
data = fakedata(z.rand,100)

# do a test on data and capture the weights in z.train
z.train = perceptrain(data$S,data$y)

# plot training data and z.train vector

data.plot = data.frame(cbind(data$S[,1:2],as.factor(data$y)))
attach(data.plot)
plot(X1, X2, col=c("red","blue")[X3],main="Training Data")

# plot the z vector
point.a = c(-z.train$z[3]/z.train$z[1],0)
point.b = c(0,-z.train$z[3]/z.train$z[2])

slope.line = (point.b[2]-point.a[2])/(point.b[1]-point.a[1])
b.line = point.a[2]- slope.line*point.a[1]

abline(b.line,slope.line)

# plot some iterations from Z.history

z.hist.seq = seq(1,nrow(z.train$Z.history),as.integer(nrow(z.train$Z.history)/5))

par(mfrow=c(3,2))

for (i in 1:length(z.hist.seq)){
  plot(X1, X2, col=c("red","blue")[X3],main=(paste("Training Data, iteration ", z.hist.seq[i], sep="")))
  point.a = c(-z.train$Z.history[i,3]/z.train$Z.history[i,1],0)
  point.b = c(0,-z.train$Z.history[i,3]/z.train$Z.history[i,2])
  
  slope.line = (point.b[2]-point.a[2])/(point.b[1]-point.a[1])
  b.line = point.a[2]- slope.line*point.a[1]
  abline(b.line,slope.line)}

detach(data.plot)

# generate a new fake dataset "data2" test data, with same z
data2 = fakedata(z.rand,100)

# run the classifier on dataset "data2" with training z vector
data2.yhat = classify(data2$S,z.train$z)

# compare to the actual y
misclassify.count = 0
for (i in 1:length(data2.yhat)){
  if (data2.yhat[i] != data2$y[i]){
misclassify.count = misclassify.count +1}}

# print misclassify count and rate
print(misclassify.count)
print(misclassify.count/length(data2.yhat))

# plot data2
par(mfrow=c(1,1))
data.plot2 = data.frame(cbind(data2$S[,1:2],as.factor(data2$y)))
attach(data.plot2)
plot(X1, X2, col=c("red","blue")[X3],main="Test Data")

# plot the z vector

point.a = c(-z.train$z[3]/z.train$z[1],0)
point.b = c(0,-z.train$z[3]/z.train$z[2])

slope.line = (point.b[2]-point.a[2])/(point.b[1]-point.a[1])
b.line = point.a[2]- slope.line*point.a[1]

abline(b.line,slope.line)
detach(data.plot2)