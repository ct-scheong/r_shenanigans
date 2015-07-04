#############################
# Shery Cheong
# March 4th 2015
#############################

# imports file
library(jpeg)
zz = readJPEG("cmandenoisy.jpg")

# define matrix
imagevec= matrix(0,nrow=256,ncol=1)

# define radius and set of h values
for (o in c(0.1,0.5,1,5,10)){
radius=4
h = o
  
# loop through each pixel and apply gaussian kernel regression
newimage = matrix(, nrow = nrow(zz), ncol = ncol(zz))

for (a in 1:nrow(zz)){
  for (b in 1:ncol(zz)){
r = a - radius
c = b - radius
num = 0
den = 0
s = r+ 2*radius-1
d = c + 2*radius -1
for (j in r:s){
for (i in c:d){
  
if(!(i<1) & !(j<1) & !(i>=256) & !(j>256)){
    kern = exp(-((dist(rbind(c(j,i),c(a,b)))/h)^2)/2)
  num = num + kern*zz[j,i]
  den = den + kern
}}}
newimage[a,b]= ifelse(den == 0, 0,num/den) 
}}
imagevec = cbind(imagevec,newimage)
}

imagevec=as.matrix(imagevec)

# outputs resulting image
zzzz = writeJPEG(imagevec,target = "output.jpg")