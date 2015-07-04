#############################
# Shery Cheong
# April 12 2015
#############################

####################################################################################
H<-matrix(readBin("histograms.bin", "double", 640000), 40000, 16)

# define function "MultinomialEM"
MultinomialEM <- function(H,k,tau)
  {
# add small amount to H to deal with computation errors
H = (H + 0.01)
# select K random clusters from H
t = H[sample(nrow(H), k), ]

# set initial weight c
c = rep(1/k,k)

# initially set while loop to true
keepgoing = TRUE

# while true, run EM algorithm
  while (keepgoing)
  {
  # E-Step
  phi = exp(H %*% log(t(t)))
  z = t(t(phi)*c)
  a = z * matrix(rep(1/rowSums(phi), k), nrow(H), k)
  
  # M-Step
  c = apply(a, 2, sum)/nrow(H)
  b = t(a) %*% H
  t = t(apply(b, 1, function(x) x/sum(x)))
  
  # Define new phi and new a
  phi.new = exp(H %*% log(t(t)))
  z.new = t(t(phi.new)*c)
  a.new = z.new * matrix(rep(1/rowSums(phi.new), k), nrow(H), k)  
  # Calculate difference
  delta = norm(a.new - a)
  
  # If delta is smaller than tau, turn the while loop off
  if (delta<tau)
  {keepgoing = FALSE}
    }
# select the max cluster k from each row
m = apply(a, 1, which.max)

# return the m matrix
return (m)
}

####################################################################################

# tau=0.01

par(mfrow = c(2, 3))
for (k in c(3,4,5))
{
  m = MultinomialEM(H=H,k=k,tau=.01)
  
  matrix = matrix(m,200,200)
  rotate <- function(x) t(apply(x, 2, rev))
  reverse <- function(x) apply(x, 2, rev)
  matrix = rotate(rotate(matrix))
  matrix= reverse(matrix)
  title = paste("tau=0.01, k=", k)
  image(matrix, axes = FALSE, col = grey(seq(0, 1, length = 256)),main = title)}

####################################################################################

#tau=1

for (k in c(3,4,5))
{
  m = MultinomialEM(H=H,k=k,tau=1)
  
  # organize the m matrix into 200 rows, since there are 200x200 histograms/pixels
  matrix = matrix(m,200,200)
  title = paste("tau=1, k=", k)
  matrix = rotate(rotate(matrix))
  matrix= reverse(matrix)
  image(matrix, axes = FALSE, col = grey(seq(0, 1, length = 256)),main = title)}

####################################################################################

#tau=10
par(mfrow = c(1, 3))

for (k in c(3,4,5))
{
  m = MultinomialEM(H=H,k=k,tau=10)
  
  # organize the m matrix into 200 rows, since there are 200x200 histograms/pixels
  matrix = matrix(m,200,200)
  matrix = rotate(rotate(matrix))
  matrix= reverse(matrix)
  title = paste("tau=10, k=", k)
  image(matrix, axes = FALSE, col = grey(seq(0, 1, length = 256)),main = title)}